# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class GlassdoorItem(scrapy.Item):
    # define the fields for your item here like:
    title = scrapy.Field()
    company = scrapy.Field()
    location = scrapy.Field()
    salary_est = scrapy.Field()
    rating = scrapy.Field()
    recommend_friend = scrapy.Field()
    ceo_approve = scrapy.Field()
    job_description = scrapy.Field()
