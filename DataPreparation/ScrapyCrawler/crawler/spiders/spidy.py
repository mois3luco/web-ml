import scrapy
from scrapy.selector import Selector
from crawler.items import CrawlerItem

class SpidySpider(scrapy.Spider):
	name="spidy"
	#allowed_domains = ["googledrive.com"]
	start_urls = [
		"http://googledrive.com/host/0BxEtNlnM_Y-mXzdiQzg4dFhrU3M/parseo.html" 
	]

	def parse(self, response):
		sel = Selector(response)
		sites = sel.css('a[href$=".gz"]') 
		for site in sites:
			item = CrawlerItem()
			item['url'] = site.xpath('@href').extract()
			with open('enlaces.txt', 'a') as f:
  				f.write('{0}\n'.format(item['url'][0]))
				
		
