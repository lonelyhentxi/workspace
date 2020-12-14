export interface UploadItemInitOptionsRaw {
  filename: string;
  mime: string;
  chunkNum: string;
}

export interface UploadItemInitOptions {
  filename: string;
  mime: string;
  chunkNum: number;
}

export interface UploadItemChunkOptionsRaw {
  key: string;
  index: string;
}

export interface UploadItemChunkOptions {
  key: string;
  index: number;
}

export interface UploadItemCompleteOptions {
  key: string;
}

export interface UploadItemCompleteValue {
  uploadItem: UploadItemValue;
  buffer: Uint8Array;
}

export class UploadItemValue {
  filename: string;
  mime: string;
  key: string;
  chunks: (Uint8Array | null)[];

  constructor(options: UploadItemInitOptions) {
    this.filename = options.filename;
    this.mime = options.mime;
    this.key = `${Date.now()}-${Math.floor(Math.random() * 100000)}`;
    this.chunks = new Array(options.chunkNum).fill(null);
  }

  static init(options: UploadItemInitOptions): UploadItemValue {
    return new this(options);
  }

  get chunkNum(): number {
    return this.chunks.length;
  }

  setChunk(index: number, chunkFile: Uint8Array): boolean {
    if (index < 1 || index > this.chunkNum) {
      return false;
    }
    this.chunks[index - 1] = chunkFile;
    return true;
  }

  async complete(): Promise<UploadItemCompleteValue | null> {
    for (const b of this.chunks) {
      if (!(b instanceof Uint8Array)) {
        return null;
      }
    }
    const allSize = this.chunks.reduce((acc, curr) => acc + curr.length, 0);
    const buffer = new Uint8Array(allSize);
    let currentIndex = 0;
    for (const chunk of this.chunks) {
      buffer.set(chunk, currentIndex);
      currentIndex += chunk.length;
    }
    return { uploadItem: this, buffer };
  }
}
