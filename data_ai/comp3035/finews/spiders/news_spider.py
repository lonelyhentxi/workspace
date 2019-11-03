import scrapy
from os import path

INDEX_PATH = './resources/index'
RECORD_PATH = './resources/record'


class NewsSpider(scrapy.Spider):
    name = "index"
    start_urls = ['http://fund.10jqka.com.cn/smxw_list/index_1.shtml']

    def parse_record(self, response):
        filename: str = response.url.split('/')[-1]
        with open(path.join(RECORD_PATH, filename), 'wb') as f:
            f.write(response.body)
        self.log(f"Save file {filename}")

    def parse(self, response):
        page = int(response.url.split('/')[-1].lstrip('index_').rstrip('.shtml'))
        filename = f"index-{page}.shtml"
        with open(path.join(INDEX_PATH, filename), 'wb') as f:
            f.write(response.body)
        self.log(f'Saved file {filename}')
        for href in response.css('div.listNews > div > h1 > a::attr(href)'):
            yield response.follow(href, self.parse_record)
        next_page = response.css('.nextpage > a::attr(href)').get()
        if next_page is not None:
            yield scrapy.Request(next_page, callback=self.parse)
