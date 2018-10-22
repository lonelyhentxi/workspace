import { Injectable, OnModuleInit } from '@nestjs/common';
import { ElementHandle, launch, Page } from 'puppeteer';
import { appendFile, ensureFile } from 'fs-extra';
import * as path from 'path';
import { CrawlerBaseService } from './crawler-base.service';
import { MyLogger } from './my-logger.service';
import { HourseInfoSchema } from '../schemas/hourse-info.schema';

const resource = path.join(__dirname, '..', '..', '..', 'resource', 'lab2', 'house-info.csv');

@Injectable()
export class ZufangCrawlerService implements OnModuleInit {

  constructor(
    private readonly crawlerBaseService: CrawlerBaseService,
    private readonly logger: MyLogger,
  ) {
  }

  async onModuleInit() {
    await this.crawlRange(1, 10);
  }

  async crawlRange(from: number = 1, to: number = 10) {
    try {
      await ensureFile(resource);
      const browser = await launch({ headless: false, timeout: 20000 });
      const page = await browser.newPage();
      await page.setViewport({ width: 1440, height: 900 });
      await page.setRequestInterception(true);
      page.on('request', request => {
        if (request.resourceType() === 'image')
          request.abort();
        else
          request.continue();
      });
      await page.goto('http://sz.zu.fang.com/');
      // @TODO: fix mem leak
      for (let i = from; i <= to; i++) {
        const [next, prev] = await this.getSelectors(page, '.fanye a');
        await this.crawlerBaseService.switchToIndexNoLimit(page, i, '.fanye .pageNow', next, prev);
        const { length } = await this.crawlerBaseService.addSubSelectors(page, '.houseList .info.rel .title a');
        for (let i = 0; i < length; i++) {
          const handleLists = await this.crawlerBaseService.addSubSelectors(page, '.houseList .info.rel .title a');
          await this.linkDetail(page, handleLists[i]);
        }
      }
    } catch (e) {
      this.logger.warn(e);
    }
  }

  async linkDetail(page, handle: ElementHandle) {
    await this.crawlerBaseService.clearTarget(page, handle);
    await this.crawlerBaseService.customClickHandle(page, handle);
    const record = await this.recordRender(page);
    await appendFile(resource, record.join(';') + '\n', { encoding: 'utf-8' });
    await page.goBack();
  }

  async recordRender(page: Page): Promise<string[]> {
    let ret = [];
    for (let col of Object.keys(HourseInfoSchema)) {
      let text;
      let selector = HourseInfoSchema[col].selector;
      try {
        if (col === 'address') {
          const flag = await this.crawlerBaseService.innerText(page, 'div.trl-item2:nth-child(2) > div:nth-child(1)');
          if(/地\s*址/.test(flag)) {
            selector = HourseInfoSchema[col].orSelector;
          }
        }
        text = await this.crawlerBaseService.innerText(page, selector);
      } catch (e) {
        this.logger.warn(e.toString());
        this.logger.warn(`${selector} select fail, try alter.`);
        text = '';
      }
      ret.push(`"${text}"`);
    }
    return ret;
  }

  // @TODO: fix mem leak
  async getSelectors(page: Page, buttonSelector: string): Promise<[string, string]> {
    const { length } = await page.$$(buttonSelector);
    return [`${buttonSelector}:nth-child(${length - 1})`, `${buttonSelector}:nth-child(2)`];
  }
}
