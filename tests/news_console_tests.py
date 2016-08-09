import sys
import os

import tornado
from dateutil import parser

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import base64
import boto3
import json
import requests
import settings
import test_utils
import unittest

from ConfigParser import ConfigParser
from wand.color import Color
from wand.image import Image as wImage


from common.funcs import QueryHandler

config = ConfigParser()
config.read('/home/mama20/Desktop/SportsUnityChat/config.py')

TORNADO_PORT =  int(config.get('tornado', 'listening_port'))
TORNADO_SERVER =  "http://localhost:%u" % TORNADO_PORT

amazon_access_key = str.strip(str(config.get('amazon', 'amazon_access_key')))
amazon_secret_key = str.strip(str(config.get('amazon', 'amazon_secret_key')))

class NewsConsoleLoginTests(unittest.TestCase):

    def register_user(self, username, password, role):
        print 'inside register_user'
        query = "INSERT INTO content_writers(username, password, role) VALUES (%s, %s, %s);"
        variables = (username, password, role)
        QueryHandler.execute(query, variables)

    def setUp(self):
        print 'inside setup'
        self.login_url = TORNADO_SERVER +  '/news_login'
        self.data = dict()
        self.register_user(username='test_user_1', password='test_user_1', role='admin')

    def test_get(self):
        print 'inside test_get'
        response = requests.get(self.login_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):
        print 'inside test_post'

        # case: no POST arguments provided
        response = requests.post(url=self.login_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.MISSING_ARGUMENT_ERROR.format('username'))

        # case: 'password' not provided
        self.data = {'username': 'test_user_2'}
        response = requests.post(self.login_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.MISSING_ARGUMENT_ERROR.format('password'))

        # case: user not registered
        self.data['password'] = 'test_user_2'
        response = requests.post(self.login_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_404)
        self.assertEqual(res['info'], settings.BAD_AUTHENTICATION_ERROR)

        # case: user already registered
        self.data = {'username': 'test_user_1', 'password': 'test_user_1'}
        response = requests.post(self.login_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['user_role'], 'admin')

    def tearDown(self):
        print 'inside teardown'
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_1')


class NewsConsoleAddUserTests(unittest.TestCase):

    def register_user(self, username, password, role):
        print 'inside register_user'
        query = "INSERT INTO content_writers(username, password, role) VALUES (%s, %s, %s);"
        variables = (username, password, role)
        QueryHandler.execute(query, variables)

    def setUp(self):
        print 'inside setup'
        self.add_user_url = TORNADO_SERVER + '/news_add_user'
        self.data = dict()
        self.register_user(username='test_user_1', password='test_user_1', role='admin')

    def test_get(self):
        print 'inside test_get'
        response = requests.get(self.add_user_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):
        print 'inside test_post'

        # case: POST data incomplete
        self.data = {'username': 'test_user_1', 'password': 'test_user_1'}
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.MISSING_ARGUMENT_ERROR.format('user_role'))

        # case: user already registered
        self.data['user_role'] = 'admin'
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_409)
        self.assertEqual(res['info'], settings.USER_ALEADY_EXISTS)

        # case: new user registered with wrong role type
        self.data = {'username': 'test_user_2', 'password': 'test_user_2', 'user_role': 'writer'}
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_500)

        # case: new user registered with correct role type
        self.data['user_role'] = 'author'
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)

    def tearDown(self):
        print 'inside teardown'
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_1')
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_2')


class NewsConsoleUploadS3ObjectTests(unittest.TestCase):

    def setUp(self):
        print 'inside setup'
        self.upload_object_url = TORNADO_SERVER + '/news_upload_s3_object'
        self.data = dict()
        self.content = base64.b64encode(wImage(width=640, height=640, background=Color('blue')).make_blob(format='png'))
        self.s3_client = boto3.client('s3', aws_access_key_id = amazon_access_key, aws_secret_access_key = amazon_secret_key)

    def test_post(self):
        print 'inside test_post'

        # incomplete post data
        self.data = {'name': 'test_image_blue', 'type': 'news_image'}
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.MISSING_ARGUMENT_ERROR.format('content'))

        # invalid object type
        self.data['content'] = self.content
        self.data['type'] = 'invalid'
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_404)
        self.assertEqual(res['info'], settings.INVALID_OBJECT_TYPE)

        # new object upload
        self.data['type'] = 'news_image'
        self.bucket_name = settings.CURATED_ARTICLES_BUCKETS.get(self.data['type'])
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['link'], "{}/{}/{}".format(self.s3_client.meta.endpoint_url, self.bucket_name, self.data['name']))

        # object already exists
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_409)
        self.assertEqual(res['info'], settings.OBJECT_ALREADY_EXISTS.format(self.data['type']))

    def tearDown(self):
        print 'inside teardown'
        self.s3_client.delete_object(Bucket = self.bucket_name, Key = self.data['name'])


class NewsConsoleAddCuratedArticleTests(unittest.TestCase):

    def delete_article(self, article_headline):
        query = "DELETE FROM curated_articles WHERE article_headline = %s;"
        variables = (article_headline,)
        QueryHandler.execute(query, variables)

    def upload_image(self, bucket_name, image_name, content):
        print 'inside upload_image'
        self.s3_client.put_object(Bucket=bucket_name, Key=image_name, Body=content, ACL='public-read')

    def setUp(self):
        print 'inside setup'
        self.data = dict()
        self.add_curated_article_url = TORNADO_SERVER + '/add_article'
        self.news_image_content = base64.b64encode(wImage(width=640, height=640, background=Color('blue')).make_blob(format='png'))
        self.icebreaker_content = base64.b64encode(wImage(width=640, height=640, background=Color('red')).make_blob(format='png'))
        self.s3_client = boto3.client('s3', aws_access_key_id = amazon_access_key, aws_secret_access_key = amazon_secret_key)
        self.upload_image(bucket_name=settings.CURATED_ARTICLES_BUCKETS.get('news_image'), image_name='test_image_1',
                          content=self.news_image_content)

    def test_post(self):
        print 'inside test_post'

        # incomplete required POST data
        self.data = {'headline': 'TEST_HEADLINE'}
        response = requests.post(url=self.add_curated_article_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.MISSING_ARGUMENT_ERROR.format('news_image_name'))

        # uploading already existing new image
        self.data.update({'news_image_name': 'test_image_1', 'news_image_content': self.news_image_content,
                          'news_content': 'TEST_CONTENT', 'ice_breaker_name': 'test_breaker_image_1',
                          'ice_breaker_content': self.icebreaker_content, 'poll_question': 'TEST_POLL_QUESTION',
                          'notification_content': 'TEST_HEADLINE', 'sport_type': 'cricket', 'stats': ['link_1', 'link_2'],
                          'memes': ['meme_1'], 'publish_date': '24/06/2016', 'state': 'UnPublished'})
        response = requests.post(url=self.add_curated_article_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_409)
        self.assertEqual(res['info'], settings.OBJECT_ALREADY_EXISTS.format('news_image'))

        # valid POST data
        self.data['news_image_name'] = 'test_image_2'
        self.data['news_image_content'] = self.news_image_content
        response = requests.post(url=self.add_curated_article_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)

    def tearDown(self):
        print 'inside teardown'
        self.delete_article(article_headline='TEST_HEADLINE')
        self.s3_client.delete_object(Bucket=settings.CURATED_ARTICLES_BUCKETS.get('news_image'), Key='test_image_1')
        self.s3_client.delete_object(Bucket=settings.CURATED_ARTICLES_BUCKETS.get('news_image'), Key='test_image_2')
        self.s3_client.delete_object(Bucket=settings.CURATED_ARTICLES_BUCKETS.get('ice_breaker_image'), Key='test_breaker_image_1')


class NewsConsoleFetchArticlesTests(unittest.TestCase):

    def create_articles(self):
        print 'inside create articles'
        for article in self.articles:
            query = "INSERT INTO curated_articles (article_headline, article_content, article_image, article_poll_question, " \
                    "article_ice_breaker_image, article_publish_date, article_state) VALUES (%s, %s, %s, %s, %s, %s, %s);"
            variables = (article[0], 'TEXT', 'TEXT', 'TEXT', 'TEXT', parser.parse(article[1]), 'UnPublished')
            QueryHandler.execute(query, variables)

    def delete_articles(self):
        print 'inside delete articles'
        for article in self.articles:
            query = "DELETE FROM curated_articles WHERE article_headline = %s;"
            variables = (article[0],)
            QueryHandler.execute(query, variables)

    def setUp(self):
        print 'inside setup'
        self.fetch_articles_url = TORNADO_SERVER + '/fetch_articles'
        self.articles = [('article_1', '01/03/2015'), ('article_2', '04/01/2015'), ('article_3', '07/01/2014')]
        self.create_articles()

    def test_post(self):
        print 'inside test_post'
        response = requests.post(self.fetch_articles_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_get(self):
        print 'inside test_get'

        # invalid filter_field given
        response = requests.get(self.fetch_articles_url + '?filter_field=abc')
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_500)

        # default filter_field and order used
        response = requests.get(self.fetch_articles_url)
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), len(self.articles))
        self.assertEqual([article['article_headline'] for article in articles_result], ['article_1', 'article_2', 'article_3'])

        # filter on 'article_publish_date'
        response = requests.get(self.fetch_articles_url + '?filter_field=article_publish_date')
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), len(self.articles))
        self.assertEqual([article['article_headline'] for article in articles_result], ['article_3', 'article_1', 'article_2'])

        # "DESC" order provided
        response = requests.get(self.fetch_articles_url + '?filter_field=article_publish_date&order=DESC')
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), len(self.articles))
        self.assertEqual([article['article_headline'] for article in articles_result], ['article_2', 'article_1', 'article_3'])

    def tearDown(self):
        print 'inside teardown'
        self.delete_articles()


class NewsConsoleEditArticleTests(unittest.TestCase):

    def delete_articles(self):
        print 'inside delete articles'
        for article in self.articles:
            query = "DELETE FROM curated_articles WHERE article_id = %s;"
            variables = (article[0],)
            QueryHandler.execute(query, variables)

    def create_articles(self):
        print 'inside create articles'

        for article in self.articles:
            query = "INSERT INTO curated_articles (article_id, article_headline, article_content, article_image, article_poll_question, " \
                    "article_ice_breaker_image, article_publish_date, article_state) VALUES (%s, %s, %s, %s, %s, %s, %s, %s);"
            variables = (article[0], article[1], 'TEXT', 'TEXT', 'TEXT', 'TEXT', parser.parse(article[2]), 'UnPublished')
            QueryHandler.execute(query, variables)

    def setUp(self):
        print 'inside setup'
        self.edit_article_url = TORNADO_SERVER + '/edit_article'
        query = "SELECT max(article_id) as last FROM curated_articles;"
        result = QueryHandler.execute(query)
        self.last_article_id = int(result[0]['last']) if result else 1
        self.articles = [(self.last_article_id + 1000, 'article_1', '01/03/2015'), (self.last_article_id + 1001, 'article_2', '04/01/2015')]
        self.create_articles()

    def test_get(self):
        print 'inside test_get'

        # 'article_id' not provided
        response = requests.get(self.edit_article_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.MISSING_ARGUMENT_ERROR.format('article_id'))

        # invalid 'article_id' provided
        response = requests.get(self.edit_article_url + '?article_id=1004')
        res = json.loads(response.text)
        article = json.loads(res['article'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(article, [])

        # valid 'aricle_id' provided
        response = requests.get(self.edit_article_url + '?article_id=%s' % str(self.last_article_id + 1001))
        res = json.loads(response.text)
        article = json.loads(res['article'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(article), 1)
        self.assertEqual(article[0]['article_headline'], 'article_2')

    # def test_post(self):
    #     print 'inside test_post'
        # 'article_id' not provided
        # response = requests.post(self.edit_article_url)
        # res = json.loads(response.text)
        # self.assertEqual(res['status'], settings.STATUS_400)
        # self.assertEqual(res['info'], settings.MISSING_ARGUMENT_ERROR.format('article_id'))

        # valid 'article_id'
        # self.data = {'article_id': self.last_article_id + 1001, 'article_content': 'CHANGED_TEXT', 'article_stats': ['STAT_LINK_1', 'STAT_LINK_2']}
        # response = requests.post(self.edit_article_url, self.data)


    def tearDown(self):
        print 'inside teardown'
        self.delete_articles()

if __name__ == '__main__':
    unittest.main()
