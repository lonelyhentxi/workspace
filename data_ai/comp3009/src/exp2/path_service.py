from os import path

current_dir: str = path.dirname(__file__)


class PathService:
    ROOT_PATH: str = path.join(current_dir, "..", "..")
    RESOURCE_PATH: str = path.join(ROOT_PATH, "resource")
    SRC_PATH: str = path.join(ROOT_PATH, "src")

    def get_resource(self, name: str) -> str:
        return path.abspath(path.join(self.RESOURCE_PATH, name))

    def get_src(self, name: str) -> str:
        return path.abspath(path.join(self.SRC_PATH, name))
