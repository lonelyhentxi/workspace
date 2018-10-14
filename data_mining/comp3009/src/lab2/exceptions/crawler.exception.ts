export class CrawlerBadIndexException extends Error {
  constructor(msg: string) {
    super(msg);
  }
}