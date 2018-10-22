import { Inject, Injectable } from '@nestjs/common';
import { ElementHandle, Page } from 'puppeteer';
import { CrawlerBadIndexException } from '../exceptions/crawler.exception';
import { MyLogger } from './my-logger.service';

@Injectable()
export class CrawlerBaseService {
  constructor(
    private readonly logger: MyLogger
  ) {
  }

  async customClickHandle(page: Page, handle: ElementHandle) {
    await page.evaluate((el) => el.click(), handle);
  }

  async customClick(page: Page, selector: string) {
    await page.waitForSelector(selector);
    const link = await page.$(selector);
    await this.customClickHandle(page,link);
    await link.dispose();
  }

  async clearTarget(page: Page,elementHandle:ElementHandle) {
    await page.evaluateHandle(a => a.target = "_self", elementHandle);
  }

  async innerText(page: Page, selector: string): Promise<string> {
    await page.waitForSelector(selector);
    const elHandle = await page.$(selector);
    const innerContent = await page.evaluate(dom => dom.innerText, elHandle);
    await elHandle.dispose();
    return innerContent as string;
  }

  async getCurrentIndex(page: Page, selector: string): Promise<number> {
    const text = await this.innerText(page, selector);
    return parseInt(text, 10);
  }

  async switchToIndexNoLimit(page: Page, target: number, currentSelector: string, nextSelector: string, prevSelector: string):Promise<void> {
    await page.waitForSelector(currentSelector);
    let current = await this.getCurrentIndex(page, currentSelector);
    let buttonSelector, direction;
    if (target > current) {
      buttonSelector = nextSelector;
      direction = "next";
    } else if (target < current) {
      buttonSelector = prevSelector;
      direction = "prev";
    }
    const times = Math.abs(target - current);
    for (let i = 0; i < times; i++) {
      await page.waitForSelector(currentSelector);
      await this.customClick(page, buttonSelector);
      this.logger.log(direction);
    }
    current = await this.getCurrentIndex(page, currentSelector);
    this.logger.log(`target page index ${target},current page index ${current}`);
    if(target!==current) {
      throw new CrawlerBadIndexException("switch to incorrect page");
    }
  }

  async addSubSelectors(page:Page,selector:string): Promise<ElementHandle[]> {
    await page.waitForSelector(selector);
    return await page.$$(selector);
  }
}