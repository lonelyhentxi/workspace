import { Injectable } from '@nestjs/common';
import {
  UploadItemCompleteValue,
  UploadItemInitOptions,
  UploadItemValue,
} from './upload.values';

@Injectable()
export class UploadService {
  uploadItems = new Map<string, UploadItemValue>();

  init(options: UploadItemInitOptions): string {
    const newUploadItem = UploadItemValue.init(options);
    this.uploadItems.set(newUploadItem.key, newUploadItem);
    return newUploadItem.key;
  }

  chunk(key: string, index: number, part: Uint8Array): boolean {
    if (!this.uploadItems.has(key)) {
      return false;
    } else {
      const item = this.uploadItems.get(key);
      return item.setChunk(index, part);
    }
  }

  async complete(key: string): Promise<UploadItemCompleteValue | null> {
    if (!this.uploadItems.has(key)) {
      return null;
    } else {
      return this.uploadItems.get(key).complete();
    }
  }
}
