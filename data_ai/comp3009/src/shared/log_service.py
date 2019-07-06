from injector import inject
import logging
from shared.path_service import PathService
import sys


class LogService:
    @inject
    def __init__(self, path_service: PathService):
        self.path_service = path_service
        logger = logging.getLogger("default")
        logger.setLevel(logging.INFO)
        self.prepare_handler(logger)
        logger.info('[LogService]:LogService initialized.')
        self.logger: logging.Logger = logger

    def prepare_handler(self, logger: logging.Logger):
        logger.handlers.clear()
        file_handle = logging.FileHandler(self.path_service.get_log("default.log"), mode='a')
        console_handle = logging.StreamHandler(sys.stdout)
        formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
        file_handle.setFormatter(formatter)
        console_handle.setFormatter(formatter)
        logger.addHandler(file_handle)
        logger.addHandler(console_handle)
