import {NzNotificationService} from 'ng-zorro-antd';
import {ChainbankAgentService} from '@app/feature/services/chainbank-agent/chainbank-agent.service';

export async function mutationProgress<T>(taskFunc: () => AsyncIterableIterator<T>, notificationService: NzNotificationService):
  Promise<void> {
  const task = taskFunc();
  try {
    await task.next();
  } catch (e) {
    notificationService.create('error', 'Request Failed to send', e.message);
    throw e;
  }
  notificationService.create('info', 'Request Sent', `Request has sent`);
  let res;
  try {
    res = (await task.next()).value;
  } catch (e) {
    notificationService.create('error', 'Request Rejected', e.message);
    throw e;
  }
  notificationService.create('success', 'Request Accepted', res.message);
}

export async function* loginProgress(chainbank: ChainbankAgentService, notificationService: NzNotificationService,
                                     privateKey: string, apiAddress: string, contractAddress: string): AsyncIterableIterator<string> {
  const task = chainbank.login(privateKey, apiAddress, contractAddress);
  try {
    yield (await task.next()).value;
    yield (await task.next()).value;
    yield (await task.next()).value;
  } catch (e) {
    notificationService.create('error', 'Login failed', e.message);
    throw e;
  }
  try {
    yield (await task.next()).value;
  } catch (e) {
    notificationService.create('error', 'Login failed, cannot link to node', e.message);
    throw e;
  }
  try {
    yield (await task.next()).value;
  } catch (e) {
    notificationService.create('error', 'Login failed, invalid actor', e.message);
    throw e;
  }
  try {
    yield (await task.next()).value;
    return (await task.next()).value;
  } catch (e) {
    notificationService.create('error', 'Login failed, invalid actor', e.message + ' or invalid private key');
    throw e;
  }
}

export async function syncProgress(chainbank: ChainbankAgentService, notificationService: NzNotificationService):
  Promise<void> {
  try {
    await chainbank.syncContract();
  } catch (e) {
    notificationService.create('error', 'Failed to sync contract', e.message);
    throw e;
  }
}
