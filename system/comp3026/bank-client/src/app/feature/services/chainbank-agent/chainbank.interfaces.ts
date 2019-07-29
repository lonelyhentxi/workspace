export enum Privilege {
  Admin = 'Admin',
  Clerk = 'Clerk',
  Customer = 'Customer'
}

export class Actor {

  constructor(
    readonly identity: string,
    readonly privilege: Privilege,
    readonly enabled: boolean) {
  }


  static parseLog(log: string): Actor {
    const fields = log.split(',');
    const identity = fields[0];
    const privilege = Actor.parsePrivilege(fields[1]);
    const enabled = Actor.parseBool(fields[2]);
    return new Actor(identity, privilege, enabled);
  }

  static parsePrivilege(privilegeStr: string): Privilege {
    const lowerStr = privilegeStr.toLowerCase();
    if(lowerStr==='admin') {
      return Privilege.Admin;
    } else if(lowerStr==='clerk') {
      return Privilege.Clerk;
    } else if(lowerStr==='customer') {
      return Privilege.Customer;
    } else {
      throw new TypeError('error privilege');
    }
  }

  static parseBool(boolStr): boolean {
    const lowerStr = boolStr.toLowerCase();
    if(lowerStr==='true') {
      return true;
    } else if(lowerStr==='false') {
      return false;
    } else {
      throw new TypeError('error boolean');
    }
  }
}

export class Transaction {
  constructor(
    readonly requester: string,
    readonly sender: string,
    readonly receiver: string,
    readonly amount: string) {}

  static parseLog(log: string): Transaction {
    const fields = log.split(',');
    return new Transaction(fields[0], fields[1], fields[2], fields[3]);
  }
}

