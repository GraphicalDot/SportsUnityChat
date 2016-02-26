import tornado
import tornado.web
import settings
from global_func import QueryHandler
from tornado.web import MissingArgumentError


class AdminRequestHandler(tornado.web.RequestHandler):
    """
    Common Request Handler for Admin pages to render appropriate template.
    """
    def get(self, template):
        try:
            self.render(template, request=self.request)
        except Exception as e:
            raise e


class AdminPage(AdminRequestHandler):
    """
    Main Admin Page
    """
    def get(self):
        super(AdminPage, self).get(settings.ADMIN_TEMPLATES_PATH + "admin.html")


class AdminSelectUsers(tornado.web.RequestHandler):
    """
    Admin Page displaying first 40 rows from "users" table.
    """
    def get(self):
        query = "SELECT username,lat,lng,fb_id,fb_name,last_seen,is_available,is_banned FROM users LIMIT 40;"
        try:
            result = QueryHandler.get_results(query)
            self.render(settings.ADMIN_TEMPLATES_PATH + "select_users.html", users=result)
        except Exception as e:
            raise e


class AdminCreateUser(AdminRequestHandler):
    """
    Admin Page to create a user in "users" table.
    """
    def get(self):
        super(AdminCreateUser, self).get(settings.ADMIN_TEMPLATES_PATH + "create_user.html")

    def post(self):
        response = {}
        try:
            username = self.get_body_argument("username")
            password = self.get_body_argument("password")
            lat = self.get_body_argument("lat", default=0.0)
            lng = self.get_body_argument("lng", default=0.0)
            fb_id = self.get_body_argument("fb_id", default=None)
            fb_name = self.get_body_argument("fb_name", default="")
            apple_udid = self.get_body_argument("apple_udid", default="")
            phone_number = self.get_argument("phone_number")

            query = "INSERT INTO users(username,password,lat,lng,fb_id,fb_name,apple_udid,phone_number) VALUES(%s, %s, %s, %s, %s, %s, %s, %s);"
            variables = (username, password, lat, lng, fb_id, fb_name, apple_udid, phone_number)
            QueryHandler.execute(query, variables)

            response['info'] = settings.SUCCESS_RESPONSE
            response['status'] = settings.STATUS_200
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception as e:
            response['info'] = "Error: %s" % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)


class AdminUpdateUser(AdminRequestHandler):
    """
    Admin page to update a user.
    """
    def get(self):
        super(AdminUpdateUser, self).get(settings.ADMIN_TEMPLATES_PATH + "update_user.html")

    def post(self):
        response = {'info': '', 'status': 0}
        try:
            query = "UPDATE users SET "
            phone_number = self.get_body_argument("phone_number")
            lat = self.get_body_argument("lat", default=None)
            lng = self.get_body_argument("lng", default=None)
            fb_id = self.get_body_argument("fb_id", default=None)
            fb_name = self.get_body_argument("fb_name", default=None)
            is_available = self.get_body_argument("is_available", default=None)
            apple_udid = self.get_body_argument("apple_udid", default=None)

            request_data = {'lat': lat, 'lng': lng, 'fb_id': fb_id, 'fb_name': fb_name, 'apple_udid': apple_udid}
            for key, value in request_data.items():
                if value:
                    append_query = "{}='{}', ".format(key, value) if key in ['fb_name', 'apple_udid'] else "{}={}, ".format(key, value)
                else:
                    append_query = ""
                query = query + append_query

            query = query + "is_available={} WHERE phone_number='{}';".format(True if is_available else False, phone_number)
            QueryHandler.execute(query)

            response['info'] = settings.SUCCESS_RESPONSE
            response['status'] = settings.STATUS_200
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception as e:
            response['info'] = "Error: %s" % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)


class AdminDeleteUser(AdminRequestHandler):
    """
    Admin Page to delete a user.
    """
    def get(self):
        super(AdminDeleteUser, self).get(settings.ADMIN_TEMPLATES_PATH + "delete_user.html")

    def post(self):
        response = {}
        try:
            phone_number = self.get_body_argument("phone_number")
            query = "DELETE FROM users WHERE phone_number=%s;"
            variables = (phone_number,)
            QueryHandler.execute(query, variables)
            response['info'] = settings.SUCCESS_RESPONSE
            response['status'] = settings.STATUS_200
        except MissingArgumentError, status:
            response["info"] = status.log_message
            response["status"] = settings.STATUS_400
        except Exception as e:
            response['info'] = "Error: %s" % e
            response['status'] = settings.STATUS_500
        finally:
            self.write(response)
