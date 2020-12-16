import { NestFactory } from '@nestjs/core';
import { NestExpressApplication } from '@nestjs/platform-express';
import { AppModule } from './app.module';
import * as path from 'path';

async function bootstrap() {
  const app = await NestFactory.create<NestExpressApplication>(AppModule, {
    cors: true,
  });
  app.useStaticAssets(path.join(__dirname, '..', 'static'), {
    prefix: '/static',
  });
  await app.listen(9999);
}

bootstrap();
