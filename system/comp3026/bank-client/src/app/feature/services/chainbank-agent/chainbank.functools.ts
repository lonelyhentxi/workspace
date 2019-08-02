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

export async function syncProgress(chainbank: ChainbankAgentService, notificationService: NzNotificationService):
  Promise<void> {
  try {
    await chainbank.syncContract();
  } catch (e) {
    notificationService.create('error', 'Failed to sync contract', e.message);
    throw e;
  }
}
