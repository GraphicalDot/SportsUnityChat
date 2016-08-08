CREATE FUNCTION get_group_id (article_id TEXT, username TEXT, poll CHAR) RETURNS RECORD AS $$
DECLARE 
result RECORD;
min_group_users REAL;
max_group_users REAL;
max_group_ratio REAL;
min_group_users_ratio REAL;
group_id TEXT;
BEGIN
	IF EXISTS (SELECT poll_answer FROM users_poll_response WHERE users_poll_response.username = username) THEN
		UPDATE users_poll_response SET poll_answer = poll_answer;
	ELSE
		INSERT INTO users_poll_response (username, poll_answer, article_id) VALUES (username, poll_answer, article_id);
	END IF;
	min_group_users := (SELECT min_group_users.count FROM min_group_users LIMIT 1);
	max_group_users := (SELECT max_group_users.count FROM max_group_users LIMIT 1);
	max_group_ratio := (SELECT max_group_ratio.count FROM max_group_ratio LIMIT 1);
	min_group_users_ratio := (SELECT min_group_users_ratio.count FROM min_group_users_ratio LIMIT 1);
	LOCK users_groups;
	LOCK articles_groups;

	IF  EXISTS (SELECT username from users_poll_response WHERE users_poll_response.article_id = article_id AND username NOT IN (SELECT DISTINCT username FROM groups_users, articles_groups WHERE articles_groups.article_id = article_id AND groups_users.username = articles_groups.group_id)) < min_group_users) THEN  
		group_id := article_id || (EXTRACT(epoch from now())::float*1000)::text;

		INSERT INTO articles_groups (article_id, group_id) VALUES (article_id, group_id));

		WITH group_users AS (SELCT username FROM users_poll_response WHERE article_id = article_id AND username NOT IN (SELECT DISTINCT username FROM groups_users, articles_groups WHERE articles_groups.article_id = article_id AND groups_users.username = articles_groups.group_id)) INSERT INTO groups_users VALUES ( SELECT group_id, username from groups_users) RETURNING 'new_group' AS action_taken, group_id AS group_id, ARRAY_AGG(SELECT username FROM groups_users) AS users INTO result;

	ELSIF EXISTS ( SELECT articles_groups.group_id FROM groups_users.group_id WHERE groups_users.group_id = articles_groups.group_id AND articles_groups.article_id = articles.article_id AND articles.article_id = users_poll_response.article_id AND COUNT(users_poll_responses.poll_answer) < max_group_users AND SUM(CASE WHEN users_poll_responses.poll_answer IS poll_answer THEN 1 ELSE 0 END) / COUNT(users_poll_responses.poll_answer) > min_group_users_ratio) AND (SUM(CASE WHEN users_poll_responses.poll_answer IS poll_answer THEN 1 ELSE 0 END) + 1) / COUNT(users_poll_responses.poll_answer) < max_group_ratio)  THEN
	
	WITH group_id AS ( SELECT articles_groups.group_id AS group_id FROM groups_users.group_id WHERE groups_users.group_id = articles_groups.group_id AND articles_groups.article_id = articles.article_id AND articles.article_id = users_poll_response.article_id AND COUNT(users_poll_responses.poll_answer) < max_group_users AND SUM(CASE WHEN users_poll_responses.poll_answer IS poll_answer THEN 1 ELSE 0 END) / COUNT(users_poll_responses.poll_answer) > min_group_users_ratio AND (SUM(CASE WHEN users_poll_responses.poll_answer IS poll_answer THEN 1 ELSE 0 END) + 1) / COUNT(users_poll_responses.poll_answer) < max_group_ratio ORDER BY SUM(CASE WHEN users_poll_responses.poll_answer IS poll_answer THEN 1 ELSE 0 END) / COUNT(users_poll_responses.poll_answer) LIMIT 1) INSERT INTO groups_users (group_id, username) VALUES (group_id, username) RETURNING 'existing_group' AS action_taken, group_id.group_id AS group_id INTO result;

	ELSE

		SELECT 'stored_preferene' AS action_taken INTO result; 
	
	END IF; 
	RETURN result;
END;
$$ LANGUAGE plpgsql;