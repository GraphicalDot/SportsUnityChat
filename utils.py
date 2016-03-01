import requests
import settings


def send_message(number, message):
    """
    Sends sms on a mobile number.
    """
    payload = {
        'method': 'sms',
        'api_key': settings.SINFINI_API_KEY,
        'message': message.strip(),
        'sender': settings.SINFINI_SENDER_ID,
        'to': str.strip(number),
        'format': 'json',
        'custom': '1,2',
        'flash': '0'
    }
    response = requests.get(settings.SINFINI_MESSAGE_GATEWAY, params=payload)
    json_response = response.json()
    return (settings.SUCCESS_RESPONSE, settings.STATUS_200) if json_response['status'] == 'OK' \
        else (json_response['message'], settings.STATUS_500)
