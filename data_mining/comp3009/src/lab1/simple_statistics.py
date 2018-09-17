from lab1.user_account_service import UserAccountService
from injector import Injector, inject

injector = Injector()
user_account_service = injector.get(UserAccountService)
user_account_service.from_csv('bank.csv')
