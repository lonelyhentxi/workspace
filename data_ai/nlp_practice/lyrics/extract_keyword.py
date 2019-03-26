from lyrics.data import get_lyrics, get_stop_words
from typing import Iterable, Dict
from sklearn.feature_extraction.text import TfidfTransformer, CountVectorizer
import pandas as pd
import re


def pre_process(text: str) -> str:
    text = text.lower()
    text = re.sub("</?.*?>", " <> ", text)
    text = re.sub("(\\d|\\W)+", " ", text)
    return text


def sort_coo(coo_matrix):
    tuples = zip(coo_matrix.col, coo_matrix.data)
    return sorted(tuples, key=lambda x: (x[1], x[0]), reverse=True)


def extract_topn_from_vector(feature_names, sorted_items, topn=5):
    """get the feature names and tf-idf score of top n items"""

    sorted_items = sorted_items[:topn]

    score_vals = []
    feature_vals = []

    for idx, score in sorted_items:
        fname = feature_names[idx]
        score_vals.append(round(score, 3))
        feature_vals.append(feature_names[idx])
    results = {}
    for idx in range(len(feature_vals)):
        results[feature_vals[idx]] = score_vals[idx]

    return results


class TextExtractor:

    def __init__(self):
        self.has_fit = False
        self.docs = None
        self.cv = None
        self.word_count_vec = None
        self.tfidf_transformer = TfidfTransformer(smooth_idf=True, use_idf=True)
        self.tf_idf_vec = None

    def fit(self, texts: Iterable[str]) -> None:
        if self.has_fit:
            raise Exception("This extractor has been fit")
        self.docs = list(map(lambda t: pre_process(t), texts))
        stop_words = get_stop_words()
        self.cv = CountVectorizer(max_df=0.85, stop_words=stop_words)
        self.word_count_vec = self.cv.fit_transform(self.docs)
        self.tfidf_transformer.fit(self.word_count_vec)

    def predict(self, text: str) -> Dict:
        tf_idf_vector = self.tfidf_transformer.transform(self.cv.transform([pre_process(text)]))
        sorted_items = sort_coo(tf_idf_vector.tocoo())
        keywords = extract_topn_from_vector(self.cv.get_feature_names(), sorted_items, 5)
        return keywords


if __name__ == "__main__":
    songs = get_lyrics()
    print("load file success.")
    extractor = TextExtractor()
    extractor.fit(songs)
    print(extractor.predict(songs[0]))
