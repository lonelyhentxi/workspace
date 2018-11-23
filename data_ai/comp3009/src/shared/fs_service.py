from injector import inject
from .path_service import PathService
import pickle


class FsService:
    @inject
    def __init__(self, path_service: PathService):
        self.path_service = path_service

    def store(self, filename: str, target: object):
        fw = open(self.path_service.get_resource(filename), "w")
        pickle.dump(target, fw)
        fw.close()

    def load(self, filename: str) -> object:
        fr = open(self.path_service.get_resource(filename))
        ret = pickle.load(fr)
        fr.close()
        return ret
