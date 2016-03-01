from global_func import QueryHandler

user_lat_lng = [[0.0,  0.0], [0.0000009, 0.0000009]]


def add_users():
    print 'inside add users'
    for user in range(1, 3):
        try:
            print 'hello 1'
            query = "INSERT INTO users(username, password, phone_number) VALUES (%s,%s, %s);"
            print 'hello 2'
            variables = ("test_" + str(user), "password", str(user))
            print 'hello 3'
            QueryHandler.execute(query, variables)
            print 'hello 4'
            print 'userrrrrrrrr:', str(user)
            # query = 'UPDATE users SET lat=%s, lng=%s, is_available=%s WHERE username=test_%s';
            # variables = (user_lat_lng[user-1][0], user_lat_lng[user-1][1], str(True), str(user))
            # QueryHandler.execute(query, variables)
        except Exception as e:
            print 'exception:::::', e
            raise e


if __name__ == "__main__":
    print 'inside main'
    add_users()

