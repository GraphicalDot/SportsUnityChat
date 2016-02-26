from global_func import QueryHandler


def delete_user(username=None, phone_number=None):
    query = "DELETE FROM users WHERE phone_number=%s or username=%s;"
    variables = (phone_number, username)
    try:
        QueryHandler.execute(query, variables)
    except Exception as e:
        raise e


def create_user(username, password, phone_number=None):
    query = "INSERT INTO users(username, password, phone_number) VALUES (%s, %s, %s);"
    variables = (username, password, phone_number)
    try:
        QueryHandler.execute(query, variables)
    except Exception as e:
        raise e
