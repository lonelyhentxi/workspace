import { Test, TestingModule } from '@nestjs/testing';
import { UploadController } from './upload.controller';
import { UploadService } from './upload.service';

describe('UploadController', () => {
  let appController: UploadController;

  beforeEach(async () => {
    const app: TestingModule = await Test.createTestingModule({
      controllers: [UploadController],
      providers: [UploadService],
    }).compile();

    appController = app.get<UploadController>(UploadController);
  });

  describe('root', () => {
    it('should return "Hello World!"', () => {
      expect(appController.getHello()).toBe('Hello World!');
    });
  });
});
