from group import Group

class ArticleDiscussionGroup(Group):
	def __init__(self, article_id, group_id = None):
		self.article_id = article_id
		self.group_id = group_id

	def allocate_group(self, username, poll_answer):
        query = " WITH updated_user_poll AS (INSERT INTO users_poll_responses (username, article_id, poll_answer) "\
        	+ " VALUES (%s, %s, %s)),  "\
	        + " groups_info AS ( SELECT group_name, SUM(CASE WHEN users_poll_responses.poll_answer IS 'y' THEN 1 ELSE 0 END) AS yes_response, SUM(CASE WHEN users_poll_responses.poll_answer IS 'n' THEN 1 ELSE 0 END) AS no_response FROM articles_groups "\
	        +	" WHERE groups_users.group_name = articles_groups.group_name AND groups_users.username = users_poll_responses.username"\
    	    + "  AND articles_groups.article_id = %s ) "\
    	    + "  INSERT INTO groups_users (group_name, username) "\
    	    + "  SELECT COALESCE( SELECT group_name FROM groups_info WHERE SUM(yes_response, no_response) < 2 ) "
        variables = (self.username, self.article_id, self.poll_answer, self.article_id, )
        QueryHandler.execute(query, variables)

