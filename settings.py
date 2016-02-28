
TORNADO_LOCALHOST = "localhost"
TORANDO_PORT = 3000

STATUS_500 = 500
STATUS_400 = 400
STATUS_200 = 200
STATUS_404 = 404
STATUS_422 = 422
STATUS_ERROR_LIST = [STATUS_400, STATUS_404, STATUS_500, STATUS_422]

SUCCESS_RESPONSE = "Success"
MISSING_APK_AND_UDID_ERROR = "Bad Request: Please provide 'apk_version' and 'udid'"
BAD_AUTHENTICATION_ERROR = " Bad Authentication Info"

# admin templates path
ADMIN_TEMPLATES_PATH = 'admin_templates/'
ADMIN_TEMPLATES = [
    'admin.html',
    'select_users.html',
    'create_user.html',
    'update_user.html',
    'delete_user.html',
    'block_unblock_user.html']
