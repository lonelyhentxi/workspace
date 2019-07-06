import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { UserDto } from '@core/eru/dtos/user.dto';
import { ApplicationDto } from '@core/eru/dtos/application.dto';

@Injectable({ providedIn: 'root' })
export class ApiService {
  public readonly protocol = 'http';
  public readonly host = 'localhost:5001';
  public readonly apiBase = 'api';
  public readonly apiCache: {
    applications: ApplicationDto[],
    user?: UserDto
  } = {
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
