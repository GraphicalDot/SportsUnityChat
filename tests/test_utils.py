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
