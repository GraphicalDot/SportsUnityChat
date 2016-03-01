
TORNADO_LOCALHOST = "localhost"
TORANDO_PORT = 3000

STATUS_500 = 500
STATUS_400 = 400
STATUS_200 = 200
STATUS_404 = 404
STATUS_422 = 422
STATUS_403 = 403
STATUS_ERROR_LIST = [STATUS_400, STATUS_404, STATUS_500, STATUS_422, STATUS_403]

SUCCESS_RESPONSE = "Success"
MISSING_APK_AND_UDID_ERROR = "Bad Request: Please provide 'apk_version' and 'udid'"
BAD_AUTHENTICATION_ERROR = " Bad Authentication Info"
USER_FORBIDDEN_ERROR = "Forbidden: The user is BLOCKED!"

# admin templates path
ADMIN_TEMPLATES_PATH = 'admin_templates/'
ADMIN_TEMPLATES = [
    'admin.html',
    'select_users.html',
    'create_user.html',
    'update_user.html',
    'delete_user.html',
    'block_unblock_user.html']

# App testing
TESTING_NUMBER_1 = '918989898989'
TESTING_NUMBER_2 = '911010101010'
TESTING_NUMBER_3 = '911212121212'

APP_TESTING_PHONE_NUMBERS = [
    TESTING_NUMBER_1,
    TESTING_NUMBER_2,
    TESTING_NUMBER_3
]

APP_TESTING_OTP = {
    TESTING_NUMBER_1: 1234,
    TESTING_NUMBER_2: 4567,
    TESTING_NUMBER_3: 7890
}
