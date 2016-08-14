from dateutil import parser
from common.funcs import QueryHandler


def delete_registered_user(phone_number):
    query = "DELETE FROM registered_users WHERE phone_number=%s;"
    variables = (phone_number,)
    QueryHandler.execute(query, variables)


def create_user(username, password, phone_number=None):
    query = "INSERT INTO users(username, password, phone_number) VALUES (%s, %s, %s);"
    variables = (username, password, phone_number)
    try:
        QueryHandler.execute(query, variables)
    except Exception as e:
        raise e


def select_user(username = None, phone_number = None):
    if username or phone_number:
        query = " SELECT * FROM users WHERE " + (" phone_number " if phone_number else " username ") + "= %s;"
        variables = (phone_number if phone_number else username, )
        return QueryHandler.get_results(query, variables)
    else:
        raise Exception


def delete_user(username = None, phone_number = None):
    if username or phone_number:
        query = " DELETE FROM users WHERE " + (" phone_number " if phone_number else " username ") + "= %s;"
        variables = (phone_number if phone_number else username, )
        QueryHandler.execute(query, variables)
    else:
        raise Exception

def delete_user_from_table(field, table, field_value):
    query = " DELETE FROM " + table + " WHERE " + field + " = %s;"
    variables = (field_value,)
    QueryHandler.execute(query, variables)

def register_user(username, password, role):
    query = "INSERT INTO content_writers(username, password, role) VALUES (%s, %s, %s);"
    variables = (username, password, role)
    QueryHandler.execute(query, variables)

def delete_field_from_table(table, field, field_value):
    query = "DELETE FROM " + table + " WHERE " + field + " = %s;"
    variables = (field_value,)
    QueryHandler.execute(query, variables)

def create_articles(articles):
    for article in articles:
        query = "INSERT INTO curated_articles (article_id, article_headline, article_content, article_image, article_poll_question, " \
                "article_ice_breaker_image, article_sport_type, article_publish_date, article_state) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s);"
        variables = (article[0], article[1], 'TEXT', 'TEXT', 'TEXT', 'TEXT', article[3], parser.parse(article[2]), article[4])
        QueryHandler.execute(query, variables)

def delete_articles(articles):
    for article in articles:
        query = "DELETE FROM curated_articles WHERE article_id = %s;"
        variables = (article[0],)
        QueryHandler.execute(query, variables)

def get_max_id(table, field_name):
    query = "SELECT max(" + field_name + ") as last FROM " + table +";"
    result = QueryHandler.execute(query)
    return int(result[0]['last']) if result else 1
