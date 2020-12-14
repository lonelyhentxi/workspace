import {
  Body,
  Controller,
  HttpException,
  HttpStatus,
  Post,
  UploadedFile,
  UseInterceptors,
} from '@nestjs/common';
import { UploadService } from './upload.service';
import {
  UploadItemCompleteOptions,
  UploadItemChunkOptionsRaw,
  UploadItemInitOptionsRaw,
} from './upload.values';
import { FileInterceptor } from '@nestjs/platform-express';
import * as path from 'path';
import * as fs from 'fs';
import * as util from 'util';
import { TextDecoder } from 'util';

@Controller('api/file/upload')
export class UploadController {
  constructor(private readonly uploadService: UploadService) {}

  @Post('init')
  init(@Body() body: UploadItemInitOptionsRaw) {
    const key = this.uploadService.init({
      filename: body.filename,
      mime: body.mime,
      chunkNum: parseInt(body.chunkNum, 10),
    });
    return { key };
  }

  @UseInterceptors(FileInterceptor('file'))
  @Post('chunk')
  chunk(
    @Body() body: UploadItemChunkOptionsRaw,
    @UploadedFile() file: Express.Multer.File,
  ) {
    let buffer: Uint8Array;
    if (file.encoding === 'base64') {
      buffer = new Uint8Array(
        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
        // @ts-ignore
        Buffer.from(
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-ignore
          Buffer.from(new Uint8Array(file.buffer.buffer), 'utf-8').toString(),
          'base64',
        ),
      );
    } else {
      buffer = new Uint8Array(file.buffer.buffer);
    }
    const result = this.uploadService.chunk(
      body.key,
      parseInt(body.index, 10),
      buffer,
    );
    if (!result) {
      throw new HttpException(
        {
          status: HttpStatus.BAD_REQUEST,
        },
        HttpStatus.BAD_REQUEST,
      );
    } else {
      return {};
    }
  }

  @Post('complete')
  async complete(@Body() body: UploadItemCompleteOptions) {
    const completeValue = await this.uploadService.complete(body.key);
    if (!completeValue) {
      throw new HttpException(
        {
          status: HttpStatus.BAD_REQUEST,
        },
        HttpStatus.BAD_REQUEST,
      );
    } else {
      const filepath = path.join(
        'static',
        body.key,
        completeValue.uploadItem.filename,
      );
      await util.promisify(fs.mkdir)(path.dirname(filepath), {
        recursive: true,
      });
      await util.promisify(fs.writeFile)(
        filepath,
        new Uint8Array(completeValue.buffer),
      );
      return { path: filepath };
    }
  }
}
