import pandas as pd
import numpy as np
import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--model_prefix", default=None, type=str, required=True)
parser.add_argument("--out_path", default=None, type=str, required=True)
args = parser.parse_args()

df = pd.read_csv('data/test.csv', sep=',')
temp = pd.read_csv(f'{args.model_prefix}/sub.csv', sep='\t')
res = pd.DataFrame([])
res['label'] = np.argmax(temp[['label_0', 'label_1']].values, -1)
res['id'] = temp['id']
res[['id', 'label']].to_csv(args.out_path, index=False, sep=',')
