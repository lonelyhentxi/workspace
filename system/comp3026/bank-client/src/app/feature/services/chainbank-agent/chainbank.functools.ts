import {NzNotificationService} from 'ng-zorro-antd';

export async function requestProgress<T>(taskFunc: () => AsyncIterableIterator<T>, notificationService: NzNotificationService) {
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
