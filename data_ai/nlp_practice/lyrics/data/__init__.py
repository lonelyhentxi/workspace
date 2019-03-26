import os
from clint.textui import progress
import requests
import hashlib

SONG_DATA_PATH = os.path.join(os.path.dirname(os.path.realpath(__file__)), "songdata.csv")
SONG_DATA_LINK = "https://raw.githubusercontent.com/GrayRobert/" \
                 "big-data-project/master/src/main/resources/temp/data/songdata.csv"
SONG_DATA_MD5 = "7b5072c4cb7c84d89798592f1635bfc8"


def md5(path: str):
    hash_md5 = hashlib.md5()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def get_lyrics():
    if not os.path.exists(SONG_DATA_PATH) or md5(SONG_DATA_PATH) != SONG_DATA_MD5:
        try:
            print(f"lyrics \"songdata.csv\" not exist, try download from {SONG_DATA_LINK}")
            r = requests.get(SONG_DATA_LINK, stream=True)
            with open(SONG_DATA_PATH, 'wb') as f:
                total_length = int(r.headers.get('content-length'))
                for chunk in progress.bar(r.iter_content(chunk_size=1024), label="Downloading data",
                                          expected_size=(total_length / 1024) + 1):
                    if chunk:
                        f.write(chunk)
                        f.flush()
        except Exception as e:
            print(e)
            os.remove(SONG_DATA_PATH)
    all_songs = []
    with open(SONG_DATA_PATH, 'r') as f:
        content = f.readlines()
        current_a_song = []
        for line in content:
            if line.rstrip("\n") == "\"":
                current_a_song_str: str = "".join(current_a_song)
                all_songs.append(current_a_song_str)
    return all_songs


if __name__ == "__main__":
    print(md5(SONG_DATA_PATH))
