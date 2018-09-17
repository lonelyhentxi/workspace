from injector import inject
import logging
from shared.path_service import PathService


class LogService:
    @inject
    def __init__(self, path_service: PathService):
        self.path_service = path_service
        file_handle = logging.FileHandler(path_service.get_log("default.log"),mode='a')
        console_handle = logging.StreamHandler()
        logger = logging.getLogger("default")
        logger.setLevel(logging.INFO)
        formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
        file_handle.setFormatter(formatter)
        console_handle.setFormatter(formatter)
        logger.addHandler(file_handle)
        logger.addHandler(console_handle)
        logger.info('[LogService]:LogService initialized.')
        self.logger: logging.Logger = logger
