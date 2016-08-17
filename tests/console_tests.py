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
config.read('config.py')

TORNADO_PORT =  int(config.get('tornado', 'listening_port'))
TORNADO_SERVER =  "http://localhost:%u" % TORNADO_PORT

amazon_access_key = str.strip(str(config.get('amazon', 'amazon_access_key')))
amazon_secret_key = str.strip(str(config.get('amazon', 'amazon_secret_key')))


class NewsConsoleLoginTests(unittest.TestCase):

    def setUp(self):
        self.login_url = TORNADO_SERVER +  '/news_login'
        self.data = dict()
        test_utils.register_content_writer(username='test_user_1', password='test_user_1', role='admin')

    def test_get(self):
        response = requests.get(self.login_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):

        # case: no POST arguments provided
        response = requests.post(url=self.login_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], "Missing argument username")

        # case: 'password' not provided
        self.data = {'username': 'test_user_2'}
        response = requests.post(self.login_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], "Missing argument password")

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
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_1')


class NewsConsoleAddUserTests(unittest.TestCase):

    def setUp(self):
        self.add_user_url = TORNADO_SERVER + '/news_add_user'
        self.data = dict()
        test_utils.register_content_writer(username='test_user_1', password='test_user_1', role='admin')

    def test_get(self):
        response = requests.get(self.add_user_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):
        # case: user already registered
        self.data = {'username': 'test_user_1', 'password': 'test_user_1', 'user_role': 'admin'}
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_409)
        self.assertEqual(res['info'], settings.DUPLICATE_KEY_ERROR.format('username'))

        # case: new user registered with wrong role type
        self.data = {'username': 'test_user_2', 'password': 'test_user_2', 'user_role': 'writer'}
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.INTERNAL_SERVER_ERROR)

        # case: new user registered with correct role type
        self.data['user_role'] = 'author'
        response = requests.post(url=self.add_user_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT username FROM content_writers WHERE username = '%s';" % (self.data['username'],)
        self.assertEqual(len(QueryHandler.get_results(query)), 1)

    def tearDown(self):
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_1')
        test_utils.delete_user_from_table(field='username', table='content_writers', field_value='test_user_2')


class NewsConsoleUploadS3ObjectTests(unittest.TestCase):

    def setUp(self):
        self.upload_object_url = TORNADO_SERVER + '/news_upload_s3_object'
        self.data = dict()
        self.bucket_name = None
        self.s3_client = boto3.client('s3', aws_access_key_id = amazon_access_key, aws_secret_access_key = amazon_secret_key)
        self.content = base64.b64encode(wImage(width=640, height=640, background=Color('blue')).make_blob(format='png'))

    def test_post(self):

        # incomplete post data
        self.data = {'name': 'test_image_blue', 'type': 'news_image'}
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument content')

        # invalid object type
        self.data['content'] = self.content
        self.data['type'] = 'invalid'
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.INVALID_BUCKET_ERROR)

        # new object upload
        self.data['type'] = 'news_image'
        self.bucket_name = settings.articles_BUCKETS.get(self.data['type'])
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['link'], "{}/{}/{}".format(self.s3_client.meta.endpoint_url, self.bucket_name, base64.encodestring(self.data['name'])))
        self.s3_client.get_object(Bucket = self.bucket_name, Key = base64.encodestring(self.data['name']))

        # object already exists
        response = requests.post(url=self.upload_object_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_409)
        self.assertEqual(res['info'], settings.KEY_ALREADY_EXISTS)

    def tearDown(self):
        if self.bucket_name:
            self.s3_client.delete_object(Bucket = self.bucket_name, Key = base64.encodestring(self.data['name']))


class NewsConsoleAddCuratedArticleTests(unittest.TestCase):

    def setUp(self):
        self.data = dict()
        self.article_id = None
        self.add_curated_article_url = TORNADO_SERVER + '/add_article'
        self.news_image_bucket = settings.articles_BUCKETS.get('news_image')
        self.ice_breaker_bucket = settings.articles_BUCKETS.get('ice_breaker_image')
        self.news_image_content = base64.b64encode(wImage(width=640, height=640, background=Color('blue')).make_blob(format='png'))
        self.icebreaker_content = base64.b64encode(wImage(width=640, height=640, background=Color('red')).make_blob(format='png'))
        self.s3_client = boto3.client('s3', aws_access_key_id = amazon_access_key, aws_secret_access_key = amazon_secret_key)

    def test_post(self):

        # incomplete required POST data
        self.data = {'headline': 'TEST_HEADLINE'}
        response = requests.post(url=self.add_curated_article_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument article_content')

        # valid POST data
        self.data.update({'article_image_content': self.news_image_content, 'article_content': 'TEST_CONTENT',
                          'ice_breaker_content': self.icebreaker_content, 'poll_question': 'TEST_POLL_QUESTION',
                          'notification_content': 'TEST_HEADLINE', 'sport_type': 'c', 'stats': ['link_1', 'link_2'],
                          'memes': ['meme_1'], 'publish_date': '24/06/2016', 'state': 'UnPublished'})

        response = requests.post(url=self.add_curated_article_url, data=self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT article_id FROM articles WHERE article_headline = '%s';" % (self.data['headline'],)
        result = QueryHandler.get_results(query)
        self.assertEqual(len(result), 1)
        self.article_id = str(result[0]['article_id'])
        self.s3_client.get_object(Bucket = self.news_image_bucket, Key = str(self.article_id))
        self.s3_client.get_object(Bucket = self.ice_breaker_bucket, Key = str(self.article_id))

    def tearDown(self):
        test_utils.delete_field_from_table('articles', 'article_headline', 'TEST_HEADLINE')
        self.s3_client.delete_object(Bucket=self.news_image_bucket, Key=str(self.article_id))
        self.s3_client.delete_object(Bucket=self.ice_breaker_bucket, Key=str(self.article_id))


class NewsConsoleFetchArticlesTests(unittest.TestCase):

    def setUp(self):
        self.fetch_articles_url = TORNADO_SERVER + '/fetch_articles'
        self.articles = [{'headline': 'article_1', 'sport_type': 'c',
                          'publish_date': '01/03/2015', 'state': 'UnPublished'},
                         {'headline': 'article_2', 'sport_type': 'f',
                          'publish_date': '04/01/2015', 'state': 'Published'},
                         {'headline': 'article_3', 'sport_type': 'c',
                          'publish_date': '07/01/2014', 'state': 'Published'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_post(self):
        response = requests.post(self.fetch_articles_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_get(self):

        # invalid filter_field given
        response = requests.get(self.fetch_articles_url + '?filter_field=abc')
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.BAD_INFO_ERROR.format('filter_field'))

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

        # filter on 'article_state'
        response = requests.get(self.fetch_articles_url + '?filter_field=article_state&article_state=Published')
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 2)
        self.assertEqual([article['article_headline'] for article in articles_result], ['article_2', 'article_3'])

        # filter on 'sport_type'
        response = requests.get(self.fetch_articles_url + '?filter_field=article_sport_type&article_sport_type=f')
        res = json.loads(response.text)
        articles_result = json.loads(res['articles'])
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(len(articles_result), 1)
        self.assertEqual(articles_result[0]['article_headline'], 'article_2')

        # filter on 'sport_type' and 'sport_type' not provided
        response = requests.get(self.fetch_articles_url + '?filter_field=article_sport_type&article_state=Published')
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument article_sport_type')

    def tearDown(self):
        test_utils.delete_articles(self.article_ids)


class NewsConsoleEditArticleTests(unittest.TestCase):

    def setUp(self):
        self.edit_article_url = TORNADO_SERVER + '/edit_article'
        self.articles = [{'headline': 'article_1', 'sport_type': 'c',
                          'publish_date': '01/03/2015', 'state': 'UnPublished'},
                         {'headline': 'article_2', 'sport_type': 'f',
                          'publish_date': '04/01/2015', 'state': 'Published'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_get(self):

        # invalid 'article_id' provided
        response = requests.get(self.edit_article_url + '?article_id=1004')
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.BAD_INFO_ERROR.format('article_id'))

        # valid 'aricle_id' provided
        response = requests.get(self.edit_article_url + '?article_id=%s' % str(self.article_ids[1]))
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        self.assertEqual(res['article']['article_headline'], 'article_2')

    def test_post(self):

        # valid 'article_id'
        self.data = {'article_id': self.article_ids[1], 'article_content': 'CHANGED_TEXT', 'article_stats': ['STAT_LINK_1', 'STAT_LINK_2']}
        response = requests.post(self.edit_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT article_content FROM articles WHERE article_id = %s;"
        variables = (self.data['article_id'],)
        result = QueryHandler.get_results(query, variables)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['article_content'], self.data['article_content'])

        # update with new publish_date
        self.data = {'article_id': self.article_ids[1], 'article_publish_date': '05/01/2015'}
        response = requests.post(self.edit_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT to_char(article_publish_date, 'DD/MM/YYYY') as publish_date FROM articles WHERE article_id = %s;"
        variables = (self.article_ids[1],)
        result = QueryHandler.get_results(query, variables)
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['publish_date'], self.data['article_publish_date'])

    def tearDown(self):
        test_utils.delete_articles(self.article_ids)


class NewsConsoleDeleteArticleTests(unittest.TestCase):

    def setUp(self):
        self.delete_article_url = TORNADO_SERVER + '/delete_article'
        self.data = dict()
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'publish_date': '01/03/2015', 'state': 'Published'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_get(self):
        self.data = {'article_id': self.article_ids[0]}
        response = requests.get(self.delete_article_url, self.data)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):

        # article_id not provided
        self.data = {}
        response = requests.post(self.delete_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument article_id')

        # valid article_id provided
        self.data = {'article_id': self.article_ids[0]}
        response = requests.post(self.delete_article_url, self.data)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT * FROM articles WHERE article_id = %s;"
        variables = (self.data['article_id'],)
        result = QueryHandler.get_results(query, variables)
        self.assertEqual(result, [])

    def tearDown(self):
        test_utils.delete_articles(self.article_ids)


class NewsConsolePublishArticleTests(unittest.TestCase):

    def setUp(self):
        self.publish_article_url = TORNADO_SERVER + '/publish_article'
        self.articles = [{'headline': 'article_1', 'sport_type': 'c', 'publish_date': '01/03/2015', 'state': 'UnPublished'},
                         {'headline': 'article_2', 'sport_type': 'c', 'publish_date': '04/01/2015', 'state': 'UnPublished'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_post(self):

        # invalid 'article_id' provided
        response = requests.post(self.publish_article_url, {'article_id': self.article_ids[0] + 2000})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], settings.BAD_INFO_ERROR.format('article_id'))

        # valid 'article_id' provided
        response = requests.post(self.publish_article_url, {'article_id': self.article_ids[0]})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)

    def tearDown(self):
        test_utils.delete_articles(self.article_ids)


class NewsConsolePostArticlesOnCarouselTests(unittest.TestCase):

    def setUp(self):
        self.post_carousel_article_url = TORNADO_SERVER + '/post_carousel_articles'
        self.articles = [{'headline': 'article_1', 'publish_date': '01/03/2015', 'sport_type': 'c', 'state': 'Published'},
                         {'headline': 'article_2', 'publish_date': '04/01/2015', 'sport_type': 'f', 'state': 'Published'}]
        self.article_ids = test_utils.create_articles(self.articles)

    def test_get(self):
        response = requests.get(self.post_carousel_article_url)
        self.assertEqual(response.status_code, settings.STATUS_405)

    def test_post(self):
        # article_ids not provided
        response = requests.post(self.post_carousel_article_url)
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_400)
        self.assertEqual(res['info'], 'Missing argument articles')

        # valid article_ids provided
        response = requests.post(self.post_carousel_article_url,
                                 {'articles': json.dumps({'100': self.article_ids[0], '101': self.article_ids[1]})})
        res = json.loads(response.text)
        self.assertEqual(res['status'], settings.STATUS_200)
        self.assertEqual(res['info'], settings.SUCCESS_RESPONSE)
        query = "SELECT article_id FROM carousel_articles WHERE priority = 100;"
        result = QueryHandler.get_results(query)
        self.assertEqual(result[0]['article_id'], self.article_ids[0])

    def tearDown(self):
        test_utils.delete_articles(self.article_ids)


if __name__ == '__main__':
    unittest.main()
