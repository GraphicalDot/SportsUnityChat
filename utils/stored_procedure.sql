CREATE FUNCTION assign_group(_article_id INT, _username TEXT, _poll_answer CHAR) RETURNS   TABLE (
 action_taken TEXT,
 group_id TEXT,
 username TEXT
)  AS $$
DECLARE 
	min_group_users REAL;
	max_group_users REAL;
	max_group_ratio REAL;
	min_group_users_ratio REAL;
	_group_id TEXT;
BEGIN
	IF EXISTS (SELECT users_poll_responses.poll_answer FROM users_poll_responses WHERE users_poll_responses.username = _username) THEN
		UPDATE users_poll_responses SET poll_answer = _poll_answer;
	ELSE
		INSERT INTO users_poll_responses (username, poll_answer, article_id) VALUES (_username, _poll_answer, _article_id);
	END IF;
	min_group_users := (SELECT min_group_users.count FROM min_group_users LIMIT 1);
	max_group_users := (SELECT max_group_users.count FROM max_group_users LIMIT 1);
	max_group_ratio := (SELECT max_group_ratio.count FROM max_group_ratio LIMIT 1);
	min_group_users_ratio := (SELECT min_group_users_ratio.count FROM min_group_users_ratio LIMIT 1);
	LOCK groups_users;
	LOCK articles_groups;

	IF  (SELECT COUNT(users_poll_responses.username) from users_poll_responses WHERE users_poll_responses.article_id = _article_id AND users_poll_responses.username NOT IN (SELECT DISTINCT groups_users.username FROM groups_users, articles_groups WHERE articles_groups.article_id = _article_id AND groups_users.group_id = articles_groups.group_id)) = min_group_users THEN  

		_group_id := _article_id::text || ((EXTRACT(epoch from now())::float*1000)::int8)::text;
		INSERT INTO articles_groups (article_id, group_id) VALUES (_article_id, _group_id);

		WITH group_users AS (SELECT DISTINCT users_poll_responses.username FROM users_poll_responses WHERE users_poll_responses.article_id = _article_id AND users_poll_responses.username NOT IN (SELECT DISTINCT groups_users.username FROM groups_users, articles_groups WHERE articles_groups.article_id = _article_id AND groups_users.group_id = articles_groups.group_id)) INSERT INTO groups_users ( SELECT _group_id, group_users.username FROM group_users);
		
		CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS SELECT  'new_group'::TEXT AS action_taken, _group_id::TEXT AS group_id, groups_users.username::TEXT AS username FROM groups_users WHERE groups_users.group_id = _group_id;
		
		RAISE NOTICE 'creating new_group';	

	ELSIF EXISTS (SELECT g.id, COUNT(g.poll_answer) FROM 
		( SELECT DISTINCT users_poll_responses.username, articles_groups.group_id AS id, users_poll_responses.poll_answer AS poll_answer FROM articles_groups, groups_users, users_poll_responses, articles WHERE groups_users.group_id = articles_groups.group_id AND articles_groups.article_id = _article_id AND articles.article_id = users_poll_responses.article_id GROUP BY users_poll_responses.username ,  articles_groups.group_id, users_poll_responses.poll_answer 
		) g
		GROUP BY g.id HAVING SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) / ((CASE WHEN COUNT(g.poll_answer) = 0 THEN 1 ELSE COUNT(g.poll_answer) END)::float) > min_group_users_ratio AND ((SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) + 1)::float) / GREATEST(SUM(CASE WHEN g.poll_answer != _poll_answer THEN 1 ELSE 0 END ), 0.01) < max_group_ratio AND COUNT(g.poll_answer)  < max_group_users 
		) THEN
	
		RAISE NOTICE 'adding to existing_group';	

		_group_id = 
		(
			WITH group_info AS (
				SELECT g.id AS id, COUNT(g.poll_answer) FROM ( 
					SELECT DISTINCT users_poll_responses.username, articles_groups.group_id AS id, users_poll_responses.poll_answer AS poll_answer FROM articles_groups, groups_users, users_poll_responses, articles WHERE groups_users.group_id = articles_groups.group_id AND articles_groups.article_id = _article_id AND articles.article_id = users_poll_responses.article_id GROUP BY users_poll_responses.username ,  articles_groups.group_id, users_poll_responses.poll_answer 
				) g
			GROUP BY g.id 
			HAVING SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) / ((CASE WHEN COUNT(g.poll_answer) = 0 THEN 1 ELSE COUNT(g.poll_answer) END)::float) > min_group_users_ratio 
				AND ((SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) + 1)::float) / GREATEST(SUM(CASE WHEN g.poll_answer != _poll_answer THEN 1 ELSE 0 END ), 0.01) < max_group_ratio 
			ORDER BY SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) / ((CASE WHEN COUNT(g.poll_answer) = 0 THEN 1 ELSE COUNT(g.poll_answer) END)::float ) 
			LIMIT 1
		)
		SELECT group_info.id FROM group_info);
		INSERT INTO groups_users (group_id, username) VALUES (_group_id, _username);
			
		CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS  SELECT  'existing_group'::TEXT AS action_taken, _group_id AS group_id, 'null'::TEXT AS username; 
	
	ELSE

		CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS SELECT 'stored_preference'::TEXT AS action_taken, 'null'::TEXT AS group_id, 'null'::TEXT AS username; 
		RAISE NOTICE 'storing prefernces';	
	END IF;
	RETURN QUERY SELECT * FROM tmp_container ;
END;
$$ LANGUAGE plpgsql;