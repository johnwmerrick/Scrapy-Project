
from glassdoor.items import GlassdoorItem
import scrapy
import time


class glassdoor_spider(scrapy.Spider):
    name = 'glassdoor'
    allowed_urls = ['https://www.glassdoor.com/']
    start_urls = ['https://www.glassdoor.com/Job/denver-data-scientist-jobs-SRCH_IL.0,6_IC1148170_KO7,21.htm']

    def parse(self, response):
        results_url_list = ['https://www.glassdoor.com/Job/denver-data-scientist-jobs-SRCH_IL.0,6_IC1148170_KO7,21.htm']
        for i in range(2, 14):
            results_url_list.append('https://www.glassdoor.com/Job/denver-data-scientist-jobs-SRCH_IL.0,6_IC1148170_KO7,21_IP'+str(i)+'.htm')

        for url in results_url_list:
            yield scrapy.Request(url, callback=self.parse_page)



    def parse_page(self, response):
        url_list = response.xpath('//div[@class="flexbox"]/div/a/@href').extract()
        pageurl = ['https://www.glassdoor.com' + l for l in url_list]

        containers = response.xpath('//li[@class="jl"]/div[2]')

        #titles = response.xpath('//div[@class="flexbox"]/div/a/text()').extract()
        #companies = response.xpath('//div[@class="flexbox empLoc"]/div/text()').extract()
        #locations = response.xpath('//div[@class="flexbox empLoc"]/div/span/text()').extract()
        #salary_ests = response.xpath('//span[@class="green small"]/text()').extract()

        for index, url in enumerate(pageurl):
            yield scrapy.Request(url, callback=self.parse_listing, \
            	meta={'container': containers[index]}) 
                #'title': titles[index], 'company': companies[index], 
            	#'location': locations[index], 'salary_est': salary_ests[index]


    def parse_listing(self, response):
        container = response.meta['container']

        title = container.xpath('./div[@class="flexbox"]/div/a/text()').extract_first()
        company = container.xpath('./div[@class="flexbox empLoc"]/div/text()').extract_first()
        location = container.xpath('./div[@class="flexbox empLoc"]/div/span/text()').extract_first()
        salary_est = container.xpath('./div/div/span[@class="green small"]/text()').extract_first()

        #title = response.meta['title']
        #company = response.meta['company']
        #location = response.meta['location']
        #salary_est = response.meta['salary_est']
        time.sleep(2)

        rating = response.xpath('//div[@class="ratingNum"]/text()').extract_first()
        recommend_friend = response.xpath('//div[@id="EmpStats_Recommend"]/@data-percentage').extract_first()
        ceo_approve = response.xpath('//div[@class="cell middle chart ceoApprove"]/div/@data-percentage').extract_first()
        job_description = response.xpath('.//div[@id="JobDescContainer"]//text()').extract()

        item = GlassdoorItem()
        item['title'] = title
        item['company'] = company
        item['location'] = location
        item['salary_est'] = salary_est
        item['rating'] = rating
        item['recommend_friend'] = recommend_friend
        item['ceo_approve'] = ceo_approve
        item['job_description'] = job_description

        yield item

