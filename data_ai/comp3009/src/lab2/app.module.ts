import { Module } from '@nestjs/common';
import { ZufangCrawlerService } from './services/zufang-crawler.service';
import { CrawlerBaseService } from './services/crawler-base.service';
import { MyLogger } from './services/my-logger.service';

@Module({
  imports: [],
  controllers: [],
  providers: [MyLogger,CrawlerBaseService,ZufangCrawlerService],
})
export class AppModule {}
