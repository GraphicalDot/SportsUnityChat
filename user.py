from global_func import QueryHandler, S3Handler, merge_dicts
from psycopg2 import IntegrityError
import utils
import base64
import ConfigParser
import facebook
import json
import os
import random
import requests
import time
import tornado
import tornado.ioloop
import tornado.autoreload
import tornado.escape
import tornado.web
import uuid
from requests_toolbelt import MultipartDecoder
from custom_error import BadAuthentication, BadInfoSuppliedError
import admin_api
from notification_handler import NotificationHandler
config = ConfigParser.ConfigParser()
import settings
config.read('config.py')

class User:
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
        self.phone_number = phone_number

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
                    response, status = self._reset_password()
                else:
                    response, status = self._create_new()
            else:
                response, status = " Wrong or Expired Token ", settings.STATUS_400
        except Exception, e:
            response, status = " Error %e " % e, settings.STATUS_500
        finally:
            self._delete_registered()
            return response, status, self.password, self.username

    def _generate_username(self):
        self.username = self._generate_random()
    
    def _generate_password(self):
        self.password = self._generate_random()

    def _generate_random(self, n = 10):
        return (uuid.uuid4().hex)[:n]

    def _reset_password(self):
        """
            This functions resets the password of a user
            Response:-
                If successfully
                    Response, Status = "Success",settings.STATUS_200
                Else
                    Response, Status = "Error [Error]", 500
        """
        try: 
            self._generate_password()
            query = " UPDATE users SET password = %s WHERE username = %s; "
            variables = (self.password, self.username)
            QueryHandler.execute(query, variables)
            response, status = "Success", settings.STATUS_200
        except Exception, e:
            response, status = " Error %e " % e, settings.STATUS_500
        finally:
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
                    query = " INSERT INTO users (username, phone_number, password) VALUES "\
                    + "(%s, %s, %s);"
                    variables = (self.username, self.phone_number, self.password)
                    QueryHandler.execute(query, variables)
                    break
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
        self._delete_registered()
        response, status = (settings.USER_FORBIDDEN_ERROR, settings.STATUS_403) if self.check_if_user_blocked() \
            else self._register()
        return response, status

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
        random_integer = random.randint(1000,9999)
        expiration_time = int(time.time()) + int(config.get('registration', 'expiry_period_sec'))

        try:
            status_info, status_code, gateway_response = utils.send_message(number=self.phone_number, message=settings.OTP_MESSAGE.format(random_integer))
            query = " INSERT INTO registered_users (phone_number, authorization_code, expiration_time, gateway_response) VALUES ( %s, %s, %s, %s); "
            variables = (self.phone_number, random_integer, expiration_time, str(gateway_response))
            QueryHandler.execute(query, variables)
            return status_info, status_code
        except Exception, e:
            return " Error while sending message : % s" % e, settings.STATUS_500