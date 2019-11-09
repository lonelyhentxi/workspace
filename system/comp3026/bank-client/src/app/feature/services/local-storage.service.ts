import { Injectable } from '@angular/core';

@Injectable()
export class LocalStorageService {

  constructor() { }

  getItem<T>(key: string): T  {
    return JSON.parse(window.localStorage.getItem(key)) as T;
  }

  getItemOrDefault<T>(key: string, defaultValue: T): T {
    const item = this.getItem<T>(key);
    if(item==null) {
      return defaultValue;
    } else {
      return item;
    }
  }

  setItem<T>(key: string, obj: T): void {
    const objStr = JSON.stringify(obj);
    window.localStorage.setItem(key, objStr);
  }

  removeItem(key: string): void {
    window.localStorage.removeItem(key);
  }

  clear(): void {
    window.localStorage.clear();
  }
}
