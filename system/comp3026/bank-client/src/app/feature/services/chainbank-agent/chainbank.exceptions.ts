export class ActorNotExistsException {
  name: string;
  constructor(readonly message='actor not exists') {
    this.name = 'ActorNotExists';
  }
}

export class PermissionDeniedException {
  name: string;
  constructor(readonly message='permission denied') {
    this.name = 'PermissionDenied';
  }
}

export class TimeoutException {
  name: string;
  constructor(readonly message='time out') {
    this.name = 'Timeout';
  }
}
