import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { ApplicationDto } from '@core/eru/dtos/application.dto';

@Injectable({ providedIn: 'root' })
export class ApiService {
  public readonly protocol = 'http';
  public readonly host = '192.168.6.53:5001';
  public readonly apiBase = 'api';
  public readonly apiCache = {
     applications: [],
  };

  constructor(private httpClient: HttpClient) {
  }

  public apiPrefix() {
    return `${this.protocol}://${this.host}/${this.apiBase}`;
  }

  public urlJoin(prefix: string, postfix: string) {
    if (prefix.endsWith('/')) {
      return prefix + postfix;
    } else {
      return prefix + '/' + postfix;
    }
  }

  public apiJoin(postfix: string) {
    return this.urlJoin(this.apiPrefix(), postfix);
  }
}
