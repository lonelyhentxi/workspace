from multiprocessing import Pool
import json
from pandas.io.parsers import TextFileReader
from os import path
from path_service import DATA_DIR, TMP_DIR
import pandas as pd
from typing import Dict, Callable, List, Set, Any, Tuple

POOL_SIZE = 5
CHUNK_SIZE = 10000000


def df_iter_do(text_reader: TextFileReader, processor: Callable[[Tuple[int, pd.DataFrame]], Any],
               aggregator: Callable) -> Any:
    global POOL_SIZE
    pool = Pool(POOL_SIZE)
    res = pool.map(processor, enumerate(text_reader))
    return aggregator(res)


class _CreateApiProcessor:
    def __call__(self, chunk_info: Tuple[int, pd.DataFrame]) -> Set[str]:
        chunk_index, chunk = chunk_info
        print(f"creating api dict in chunk {chunk_index}...")
        api_column: pd.Series = chunk.loc[:, "api"]
        return set(api_column.unique())


class _CreateApiAggregator:
    def __call__(self, chunk_api_sets: List[Set[str]]) -> Set[str]:
        ret = set()
        for item in chunk_api_sets:
            ret = ret.union(item)
        ret.add('*')
        return ret


def get_api_index_from_dict(api_dict: Dict[str, int], api: str) -> int:
    ret = api_dict.get(api)
    if ret is None:
        return api_dict.get('*')
    return ret


def create_api_dict(api_dict_path: str, api_index_dict_path: str) -> (Dict[str, int], Dict[int, str]):
    global CHUNK_SIZE
    text_reader = pd.read_csv(path.join(DATA_DIR, "security_train.csv"), chunksize=CHUNK_SIZE, iterator=True)

    api_set: Set[str] = df_iter_do(text_reader=text_reader, processor=_CreateApiProcessor(),
                                   aggregator=_CreateApiAggregator())
    api_dict = dict()
    api_index_dict = dict()
    for api_index, api in enumerate(api_set):
        api_dict.update([(api, api_index)])
        api_index_dict.update([(api_index, api)])
    with open(api_dict_path, "w") as api_dict_file:
        api_dict_file.write(json.dumps(api_dict, sort_keys=False))
        print("api dict file created.")
    with open(api_index_dict_path, "w") as api_index_dict_file:
        api_index_dict_file.write(json.dumps(api_index_dict, sort_keys=False))
        print("api index dict file created.")
    return api_dict, api_index_dict


def load_api_dict() -> (Dict[str, int], Dict[int, str]):
    api_dict_path = path.join(DATA_DIR, "api_dict.json")
    api_index_dict_path = path.join(DATA_DIR, "api_index_dict.json")
    try:
        print("loading api dict...")
        with open(api_dict_path, "r") as api_dict_file:
            api_dict = json.loads(api_dict_file.read())
        print("load existed api dict file succeed.")
        with open(api_index_dict_path, "r") as api_index_dict_file:
            api_index_dict = json.loads(api_index_dict_file.read())
            api_index_dict = dict(list(map(lambda x: (int(x[0]), x[1]), api_index_dict.items())))
    except IOError or json.JSONDecodeError as e:
        print("load existed api dict file failed, creating...")
        api_dict, api_index_dict = create_api_dict(api_dict_path, api_index_dict_path)
    return api_dict, api_index_dict


class _TransformDataProcessor:
    def __init__(self, _api_dict: Dict[str, int], _new_tsv_columns: List[str]):
        self.api_dict = _api_dict
        self.new_tsv_columns = _new_tsv_columns

    def __call__(self, chunk_info: Tuple[int, pd.DataFrame]) -> pd.DataFrame:
        chunk_index, chunk = chunk_info
        print(f"transforming csv to new tsv in chunk {chunk_index}...")
        dataset = pd.DataFrame([], columns=self.new_tsv_columns)
        group = ['file_id', 'tid']
        if 'label' in self.new_tsv_columns:
            group.append('label')
            id_groups = chunk.groupby(group)
            for i, ((fid, tid, label), row) in enumerate(id_groups):
                index = f"{fid}-{tid}"
                apis_index = row['api'].apply(lambda x: get_api_index_from_dict(self.api_dict, x)).tolist()
                dataset.loc[index, :] = pd.Series([fid, tid, apis_index, label], self.new_tsv_columns)
        else:
            id_groups = chunk.groupby(group)
            for i, ((fid, tid), row) in enumerate(id_groups):
                index = f"{fid}-{tid}"
                apis_index = row['api'].apply(lambda x: get_api_index_from_dict(self.api_dict, x)).tolist()
                dataset.loc[index, :] = pd.Series([fid, tid, apis_index], self.new_tsv_columns)
        return dataset


class _TransformDataAggregator:
    def __init__(self, _new_tsv_columns: List[str]):
        self.new_tsv_columns = _new_tsv_columns

    def __call__(self, trunk_new_dfs: List[pd.DataFrame]) -> pd.DataFrame:
        ret_df: pd.DataFrame = pd.DataFrame([], columns=self.new_tsv_columns)
        for trunk_index, trunk_new_df in enumerate(trunk_new_dfs):
            trunk_new_df.to_csv(path.join(TMP_DIR, f'transform_tmp_{trunk_index}.tsv'), sep='\t', index=False)
        for trunk_new_df in trunk_new_dfs:
            start_row_index, start_row = next(trunk_new_df.iterrows())
            if start_row_index in ret_df.index:
                ret_df.loc[start_row_index, 'apis_index'] += trunk_new_df.loc[start_row_index, 'apis_index']
                ret_df = ret_df.append(trunk_new_df.iloc[1:, :])
            else:
                ret_df = ret_df.append(trunk_new_df)
        return ret_df


def transform_csv_to_json(new_dataset_path: str, api_dict: Dict[str, int], train_mode: bool) -> pd.DataFrame:
    global CHUNK_SIZE
    file_name = 'train' if train_mode else 'test'
    new_tsv_columns = ["fid", 'tid', 'apis_index']
    if train_mode:
        new_tsv_columns.append('label')
    text_reader = pd.read_json(path.join(DATA_DIR, f"security_{file_name}.csv"), chunksize=CHUNK_SIZE, iterator=True)

    new_dataset: pd.DataFrame = df_iter_do(text_reader, processor=_TransformDataProcessor(api_dict, new_tsv_columns),
                                           aggregator=_TransformDataAggregator(new_tsv_columns))
    print("saving transformed new tsv...")
    new_dataset.to_json(new_dataset_path, lines=True, orient="records")
    return new_dataset


def load_new_dataset(api_dict: Dict[str, int], train_mode: bool) -> pd.DataFrame:
    file_name = 'train' if train_mode else 'test'
    new_tsv_path = path.join(DATA_DIR, f"new_{file_name}.json")

    try:
        print(f"loading transformed {file_name} file...")
        ret_df = pd.read_json(new_tsv_path, orient="records", lines=True)
        print(f"loading transformed {file_name} file succeed.")
    except IOError:
        print(f"load existed transformed {file_name} file failed, creating...")
        ret_df = transform_csv_to_json(new_tsv_path, api_dict, train_mode)
    return ret_df


def train_df_to_txt(dataset: pd.DataFrame, text_name: str):
    print(f'transforming data to form {text_name}...')
    file_groups = dataset.groupby(["fid", "label"])
    with open(path.join(DATA_DIR, text_name), 'w') as text_file:
        for i, ((fid, label), group) in enumerate(file_groups):
            text = f"__label__{label} "
            for row_index, row in group.iterrows():
                text = text + ' '.join(map(lambda x: str(x), row['apis_index'])) + '.'
            text += '\n'
            text_file.write(text)


def test_df_to_txt(dataset: pd.DataFrame, text_name: str):
    print(f'transforming data to form {text_name}...')
    file_groups = dataset.groupby(["fid"])
    with open(path.join(DATA_DIR, text_name), 'w') as text_file:
        for i, ((fid), group) in enumerate(file_groups):
            text = ""
            for row_index, row in group.iterrows():
                text = text + ' '.join(map(lambda x: str(x), row['apis_index'])) + '.'
            text += '\n'
            text_file.write(text)


if __name__ == "__main__":
    ret_api_dict, ret_api_index_dict = load_api_dict()
    transformed_train = load_new_dataset(ret_api_dict, train_mode=True)
    transformed_test = load_new_dataset(ret_api_dict, train_mode=False)
    train_df_to_txt(transformed_train, "new_train")
    test_df_to_txt(transformed_test, "new_test")
