from os import path

import pandas as pd
from .path_service import PathService

path_service = PathService()
exp2_resource_dir = path_service.get_resource("exp2")
encoding_format = "gb2312"


class Exp2Assistant:
    def __init__(self, stage: int):
        """
        load data from resource directory
        :param stage: "阶段一" (1) or "阶段二" (2)
        :return: (train dataframe,test dataframe)
        """
        assert (stage == 1 or stage == 2)
        self.stage = stage
        stage_name = f"stage{stage}"
        prefix = "d" if stage == 1 else "f"
        self.stage_name = stage_name
        self.prefix = prefix
        self.stage_dir = path.join(exp2_resource_dir, stage_name)
        train_df = pd.read_csv(path.join(self.stage_dir, f"{prefix}_train.csv"), encoding=encoding_format)
        self.train_data = train_df.iloc[:, :-1]
        self.train_label = train_df.iloc[:, -1:]
        self.test_data = pd.read_csv(path.join(self.stage_dir, f"{prefix}_test.csv"), encoding=encoding_format)
        self.sample_data = pd.read_csv(path.join(self.stage_dir, f"{prefix}_sample.csv"), encoding=encoding_format)
        if stage == 1:
            self.pre_process()

    def pre_process(self):
        skip_columns = ["id", "体检日期"]
        self.train_data["性别"] = (self.train_data["性别"] == "男")
        self.test_data["性别"] = (self.test_data["性别"] == "男")
        self.train_data.drop(columns=skip_columns, axis=1, inplace=True)
        self.test_data.drop(columns=skip_columns, axis=1, inplace=True)

    def save_label(self, pre_labels):
        columns = (["label"] if self.stage == 2 else ["血糖"])
        pd.DataFrame(pre_labels, columns=columns).to_csv(path.join(self.stage_dir, f"{self.prefix}_label.csv"),
                                                         encoding=encoding_format, index=False)
