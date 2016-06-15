from common.funcs import QueryHandler, S3, merge_dicts, send_message, get_random_avatar
from dp import Dp
from psycopg2 import IntegrityError
import base64
import threading
import ConfigParser
import facebook
import json
import os
import random
import requests
import time
import uuid
import settings
from requests_toolbelt import MultipartDecoder
from s3_object import S3Object
from common.custom_error import BadAuthentication, BadInfoSuppliedError
# import admin_api
config = ConfigParser.ConfigParser()
import settings
config.read('config.py')
from node import Node

class User(Node):
    """
    This class defines the user object which can be used for successive operations
    to be done on the user object.
    Attributes to be passed on the initialization:
        username -- username of the user most typically the full jid of the user 
        password [optional] -- optionally if the user has been created.
    Methods: 
        authenticate -- authenticates the user
        handle_creation -- handles the creation of user
        _is_token_correct -- checks whether the otp token given by the user is correct/expired
        _create_new -- creates a new user after verifying the authenticity of the user
        handle_registration -- starts the registration process for the user
        _delete_registered -- deletes the registered user so that new otp can be sent to the user
        _register -- registers the user
        _send_message -- send the otp token to the users phone number
    """
    def __init__(self, phone_number = None, password = None, username = None):
        self.username = username
        self.password = password
        self.name = None
        self.interests = None
        self.phone_number = phone_number
        self.status = None
        self.photo = None
        self.friends = None

    def authenticate(self):
        """
        In case of already created user, this functions authicates the user. 
        return true if authenticated
        return false if not authenticated
        """
        query = " SELECT * FROM users WHERE username = %s AND password = %s; "
        variables = (self.username, self.password,)

        record = QueryHandler.get_results(query, variables)

        if not record:
            raise BadAuthentication

    def handle_creation(self, auth_code):
        """
        Handles the creation aspect of the User class
        Parameters:- 
            auth_code - The otp token sent to the users cellphone
        Response :-
            If token correct:
                "Success",settings.STATUS_200, [password]
            If user already created
                " User already created",settings.STATUS_200, [password]
            If wrong or expired auth token
                " Wrong or Expired Token ",settings.STATUS_400, None
        """

        try:
            if self._is_token_correct(auth_code):
                query = " SELECT * FROM users WHERE phone_number = %s ;"
                variables = (self.phone_number, )
                record = QueryHandler.get_results(query, variables)
                if record:
                    self.username = record[0]['username']
                    response, status = self._reset_password_return_user_info()
                else:
                    response, status = self._create_new()
            else:
                response, status = " Wrong or Expired Token ", settings.STATUS_400
        except Exception, e:
            response, status = " Error %e " % e, settings.STATUS_500
        finally:
            return response, status, self.password, self.username, self.name, self.interests, self.status, self.photo, self.friends

    def _generate_username(self):
        self.username = self._generate_random()
    
    def _generate_password(self):
        self.password = self._generate_random()

    def _generate_random(self, n = 10):
        return (uuid.uuid4().hex)[:n]


    def _reset_password_return_user_info(self):
        """
            This functions resets the password of a user
            Response:-
                If successfully
                    Response, Status = "Success",settings.STATUS_200
                Else
                    Response, Status = "Error [Error]", 500
        """
        small_version_image_name = self.username + "/S.jpg"
        if Dp(self.username).exists('S'):
            self.photo = Dp(self.username).get_dp_version('S')
        else:
            self.photo = get_random_avatar(self.username)
            # self.photo = requests.


        try:
            self._generate_password()
            query = " WITH delete_registered AS (DELETE FROM registered_users WHERE phone_number = E'919560488236' ), "\
            +   " updates AS ( UPDATE users SET password = %s, show_location = %s WHERE username = %s ), "\
            +   " friends AS ( SELECT split_part(rosterusers.jid, '@', 1) as username FROM rosterusers WHERE username = %s AND subscription = 'B') "\
            +   " SELECT users.username, users.name, users.password, users.status, "\
            +   " json_agg(row_to_json((SELECT d FROM (SELECT users_interest.interest_id AS id, users_interest.properties) d))) AS  interests ,"\
            +   " array_to_json(array(select row_to_json(t) from ( select name as name, username as username from users where username in (select username from friends))as t)) AS friends "\
            +   " FROM users "\
            +   " LEFT OUTER JOIN users_interest on (users.username = users_interest.username) WHERE users.username = %s  GROUP BY users.username; "
            variables = (self.password, settings.SHOW_LOCATION_NONE_STATUS, self.username, self.username, self.username,)
            record = QueryHandler.get_results(query, variables)
            self.name = record[0]['name']
            self.interests = record[0]['interests']
            self.status = record[0]['status']
            self.friends = record[0]['friends']
            response, status = "Success", settings.STATUS_200
        except Exception, e:
            response, status = " Error %e " % e, settings.STATUS_500
        return response, status


    def _is_token_correct(self, auth_code):
        """
        Authenticates the otp token sent to the user
        Parameters :-
            auth_code -- The otp token sent to the user
        Response :-
            True if correct
            False if wrong token
        """
        query = " SELECT * FROM registered_users WHERE phone_number = %s AND authorization_code = %s ;"
        variables = (self.phone_number, auth_code,)

        record = QueryHandler.get_results(query, variables)
        try:
            if record and (record[0]['expiration_time'] > int(time.time())):
                is_token_correct = True
            else:
                is_token_correct = False
        except:
            is_token_correct = False
        finally:
            return is_token_correct

    def _create_new(self):
        """
        Creates a new user in the database.
        """
        try:
            while True:
                self._generate_username()
                self._generate_password()

                query = " SELECT * FROM users WHERE username = %s;"
                variables = (self.username,)
                record = QueryHandler.get_results(query, variables)

                if not record:
                    query = " WITH delete_registered AS (DELETE FROM registered_users WHERE phone_number = %s ) INSERT INTO users (username, phone_number, password) VALUES "\
                    + "(%s, %s, %s);"
                    variables = (self.phone_number, self.username, self.phone_number, self.password)
                    QueryHandler.execute(query, variables)
                    break
            self.photo = get_random_avatar(self.username)
            response, status = "Success", settings.STATUS_200
        except Exception, e:
            response, status = " %s " % e, settings.STATUS_500
        finally:
            return response, status

    def check_if_user_blocked(self):
        query = "SELECT is_banned FROM users WHERE phone_number=%s AND is_banned=True;"
        variables = (self.phone_number,)
        return True if QueryHandler.get_results(query, variables) else False

    def handle_registration(self):
        """
        Handles the registration of the user in the database
        """
        if self.check_if_user_blocked():
            return settings.USER_FORBIDDEN_ERROR, settings.STATUS_403 
        else:
            threading.Thread(group = None, target = self._register, name = None, args = ()).start()
            return settings.SUCCESS_RESPONSE, settings.STATUS_200

    def _delete_registered(self):
        """
        Deletes the registered users in the database, which have been created or 
        when new otp token has to be sent.
        """
        query = " DELETE FROM registered_users WHERE phone_number = %s ;"
        variables = (self.phone_number,)
        QueryHandler.execute(query, variables)

    def _register(self):
        """
        Registers the users in the database
        """
        if self.phone_number in settings.APP_TESTING_PHONE_NUMBERS:
            random_integer = settings.APP_TESTING_OTP[self.phone_number]
        else:
            random_integer = random.randint(1000,9999)
        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))

        try:
            status_info, status_code, gateway_response = send_message(number=self.phone_number, message=settings.OTP_MESSAGE.format(random_integer))
            query = " WITH upsert AS (UPDATE registered_users SET "\
            +    "authorization_code = %s, expiration_time = %s, gateway_response = %s WHERE phone_number= %s RETURNING * ) "\
            + " INSERT INTO registered_users (phone_number, authorization_code, expiration_time, gateway_response) SELECT  %s, %s, %s, %s "\
            + " WHERE NOT EXISTS (SELECT * FROM upsert);"
            variables = (random_integer, expiration_time, str(gateway_response), self.phone_number, self.phone_number, random_integer, expiration_time, str(gateway_response))
            QueryHandler.execute(query, variables)
            return status_info, status_code
        except Exception, e:
            return " Error while sending message : % s" % e, settings.STATUS_500

    def set_info(self, info):
        info_keys = ['name', 'status']
        set_query_part = []
        variables = []

        for key in info_keys:
            set_query_part += [" {} = %s ".format(key)]  if info[key] else []
            variables += [info[key]] if info[key] else []

        if variables:
            query = " UPDATE users SET " +  ', '.join(set_query_part)  + " WHERE username = %s ;"
            variables += [self.username]
            QueryHandler.execute(query, variables) 


        if info['photo']:
            Dp(self.username).upload_dp(info['photo'])