import os
from clint.textui import progress
import requests
import hashlib

SONG_DATA_PATH = os.path.join(os.path.dirname(os.path.realpath(__file__)), "songdata.csv")
SONG_DATA_LINK = "https://raw.githubusercontent.com/GrayRobert/" \
                 "big-data-project/master/src/main/resources/temp/data/songdata.csv"
SONG_DATA_MD5 = "7b5072c4cb7c84d89798592f1635bfc8"

STOP_WORD_PATH = os.path.join(os.path.dirname(os.path.realpath(__file__)), "stopwords.txt")
STOP_WORD_LINK = "https://raw.githubusercontent.com/kavgan/nlp-in-practice/master/tf-idf/resources/stopwords.txt"
STOP_WORD_MD5 = "742a346fbcc61b8e899917b53a53dfb1"


def md5(path: str):
    hash_md5 = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def get_lyrics():
    if not os.path.exists(SONG_DATA_PATH) or md5(SONG_DATA_PATH) != SONG_DATA_MD5:
        if os.path.exists(SONG_DATA_PATH):
            os.remove(SONG_DATA_PATH)
        print(f"lyrics \"songdata.csv\" not exist, try download from {SONG_DATA_LINK}")
        r = requests.get(SONG_DATA_LINK, stream=True)
        with open(SONG_DATA_PATH, 'wb') as f:
            total_length = int(r.headers.get('content-length'))
            for chunk in progress.bar(r.iter_content(chunk_size=1024), label="Downloading data",
                                      expected_size=(total_length / 1024) + 1):
                if chunk:
                    f.write(chunk)
                    f.flush()
    all_songs = []
    with open(SONG_DATA_PATH, 'r') as f:
        content = f.readlines()
        current_a_song = []
        for line in content:
            if line.rstrip("\n") == "\"":
                current_a_song_str: str = "".join(current_a_song)
                all_songs.append(current_a_song_str)
            else:
                current_a_song.append(line)
    return all_songs


def get_stop_words():
    if not os.path.exists(STOP_WORD_PATH) or md5(STOP_WORD_PATH) != STOP_WORD_MD5:
        if os.path.exists(STOP_WORD_PATH):
            os.remove(STOP_WORD_PATH)
        print(f"\"stopwords.txt\" not exist, try download from {STOP_WORD_LINK}")
        r = requests.get(STOP_WORD_LINK, stream=True)
        with open(STOP_WORD_PATH, 'wb') as f:
            total_length = int(r.headers.get('content-length'))
            for chunk in progress.bar(r.iter_content(chunk_size=1024), label="Downloading data",
                                      expected_size=(total_length / 1024) + 1):
                if chunk:
                    f.write(chunk)
                    f.flush()
    with open(STOP_WORD_PATH, "r", encoding="utf-8") as f:
        stopwords = f.readlines()
        stop_set = set(m.strip() for m in stopwords)
        return frozenset(stop_set)


if __name__ == "__main__":
    print(md5(SONG_DATA_PATH))
    print(md5(STOP_WORD_PATH))
