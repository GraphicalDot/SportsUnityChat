from datetime import datetime
import ConfigParser
import psycopg2
import psycopg2.extras
import time
import requests

config = ConfigParser.ConfigParser()
config.read('config.py')


class QueryHandler(object):
    """
    Parent Class to get results and execute any psycopg2 query.
    """

    @classmethod
    def get_results(cls, query, variables=None):
        connection = cls.get_connection()
        cursor = connection.cursor(cursor_factory=psycopg2.extras.RealDictCursor)
        print(cursor.mogrify(query, variables))
        cursor.execute(query, variables)
        results = cursor.fetchall()
        connection.commit()
        cursor.close()
        return results

    @classmethod
    def execute(cls, query, variables=None):
        connection = cls.get_connection()
        cursor = connection.cursor()
        print(cursor.mogrify(query, variables))
        cursor.execute(query, variables)
        connection.commit()
        cursor.close()


class MasterQueryHandler(QueryHandler):
    """
    Connection to RDS master instance.
    """

    @classmethod
    def get_connection(cls):
        connection = psycopg2.connect("dbname=%s host=%s user=%s password=%s" %
                                      ('test_database', 'db-master-1.ctb9ah0bi3rx.ap-southeast-1.rds.amazonaws.com',
                                       'aakarshi', 'aakarshi'))
        return connection


class LocalQueryHandler(QueryHandler):
    """
    Connection to local postgres instance.
    """

    @classmethod
    def get_connection(cls):
        connection = psycopg2.connect("dbname=%s host=%s user=%s password=%s" % ('test_local', 'localhost', 'test',
                                                                                 'test'))
        return connection


def register_single_number_multiple_times(ejabberd_gateway=None, registration_url=None):
    """
    Registers user entered phone number 'registration_count' times.
    :param ejabberd_gateway: url to ec2 ejabberd server
    :param registration_url: url for registration
    :return: None
    """

    # Creating temporary table on local postgres for testing purpose.
    query = " CREATE TABLE registered_users_new (users_id integer NOT NULL,authorization_code text, local_time timestamp without time zone, " \
            "server_time timestamp without time zone, username text, status text);"
    LocalQueryHandler.execute(query)

    phone_number = raw_input('Enter the phone number to test "Registration": ')
    registration_count = raw_input('Enter number of times registration to be done: ')

    try:
        assert (len(str.strip(phone_number)) == 12)
        try:
            assert (isinstance(int(registration_count), int))
        except ValueError:
            print "Please enter some Integer"
            raise AssertionError

        # Registering a phone number 'registration_count' times.
        for user in range(0, int(registration_count)):

            # Inserting the local time before hitting the server.
            query = "INSERT INTO registered_users_new (users_id, local_time) VALUES (%s, %s);"
            variables = (user, datetime.now(), )
            LocalQueryHandler.execute(query, variables)

            # Hitting ejabberd server on register url.
            response = requests.get(ejabberd_gateway + registration_url, params={'phone_number': phone_number})
            response = str.strip(str.split(str(response.text), ',')[0].split(':')[1])

            query = " SELECT * FROM registered_users WHERE username = %s;"
            variables = (str.strip(phone_number) + '@mm.io', )
            rds_results = MasterQueryHandler.get_results(query, variables)

            # Copy the result in local postgres table if registration was successful
            # else create a new entry for missed out entry.
            if rds_results:
                query = " UPDATE registered_users_new SET (authorization_code, server_time, username, status)=(%s, %s, %s, %s) " \
                        "WHERE users_id=%s;"
                variables = (rds_results[0]['authorization_code'], rds_results[0]['created_at'],
                             rds_results[0]['username'], response[1:-1], user)
            else:
                query = " UPDATE registered_users_new SET (server_time, username, status)=(%s, %s, %s) WHERE users_id=%s;"
                variables = (datetime.now(), str.strip(phone_number) + '@mm.io', response[1:-1])

            LocalQueryHandler.execute(query, variables)

    except AssertionError:
        query = " DROP TABLE registered_users_new;"
        LocalQueryHandler.execute(query)


def register_many_numbers_multiple_times(ejabberd_gateway, registration_url):
    """
    Registers 10 phone numbers 10 times after timespan of 10 seconds.
    :param ejabberd_gateway: url to ec2 ejabberd server
    :param registration_url: url for registration
    :return: None
    """
    # Creating temporary table on local postgres for testing purpose.
    query = " CREATE TABLE registered_users_new (users_id text NOT NULL,authorization_code text, local_time timestamp without time zone, " \
            "server_time timestamp without time zone, username text, status text);"
    LocalQueryHandler.execute(query)

    registration_count = 10
    phone_numbers = ['919718626363', '918447860079', '919868177790', '919899224493', '918802035676', '919953936440',
                     '919717261060', '919560488236', '919509925532', '919873503029']

    for count in range(0, registration_count):
        print '*******************************************' + str(count) + '****************************************'

        for number in phone_numbers:
            query = "INSERT INTO registered_users_new (users_id, local_time) VALUES (%s, %s);"
            variables = (number + '@@' + str(count), datetime.now(), )
            LocalQueryHandler.execute(query, variables)

            # Hitting ejabberd server on register url.
            response = requests.get(ejabberd_gateway + registration_url, params={'phone_number': number})
            response = str.strip(str.split(str(response.text), ',')[0].split(':')[1])

            query = " SELECT * FROM registered_users WHERE username = %s;"
            variables = (str.strip(number) + '@mm.io', )
            rds_results = MasterQueryHandler.get_results(query, variables)

            # Copy the result in local postgres table if registration was successful
            # else create a new entry for missed out entry.
            if rds_results:
                query = " UPDATE registered_users_new SET (authorization_code, server_time, username, status)=(%s, %s, %s, %s) " \
                        "WHERE users_id=%s;"
                variables = (rds_results[0]['authorization_code'], rds_results[0]['created_at'],
                             rds_results[0]['username'], response[1:-1], number + '@@' + str(count))
            else:
                query = " UPDATE registered_users_new SET (server_time, username, status)=(%s, %s, %s) " \
                        "WHERE users_id=%s;"
                variables = (datetime.now(), str.strip(number) + '@mm.io', response[1:-1])

            LocalQueryHandler.execute(query, variables)

        time.sleep(10)  # wait for 10 seconds

    query = " DROP TABLE registered_users_new;"
    LocalQueryHandler.execute(query)


if __name__ == "__main__":
    print 'inside main'
    ejabberd_gateway = 'http://54.169.217.88'
    registration_url = '/register'

    # register_single_number_multiple_times(ejabberd_gateway, registration_url)

    register_many_numbers_multiple_times(ejabberd_gateway, registration_url)
