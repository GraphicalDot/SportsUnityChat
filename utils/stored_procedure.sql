CREATE OR REPLACE FUNCTION assign_discussion(_article_id INT, _username TEXT, _poll_answer CHAR) RETURNS   TABLE (
 action_taken TEXT,
 discussion_id TEXT,
 username TEXT
)  AS $$
DECLARE 
	min_discussion_users REAL;
	max_discussion_users REAL;
	max_discussion_ratio REAL;
	min_discussion_users_ratio REAL;
	_discussion_id TEXT;
	_article_name TEXT;
BEGIN
	IF EXISTS (SELECT users_poll_responses.poll_answer FROM users_poll_responses WHERE users_poll_responses.username = _username) THEN
		UPDATE users_poll_responses SET poll_answer = _poll_answer;
	ELSE
		INSERT INTO users_poll_responses (username, poll_answer, article_id) VALUES (_username, _poll_answer, _article_id);
	END IF;
	min_discussion_users := (SELECT min_discussion_users.count FROM min_discussion_users LIMIT 1);
	max_discussion_users := (SELECT max_discussion_users.count FROM max_discussion_users LIMIT 1);
	max_discussion_ratio := (SELECT max_discussion_ratio.count FROM max_discussion_ratio LIMIT 1);
	min_discussion_users_ratio := (SELECT min_discussion_users_ratio.count FROM min_discussion_users_ratio LIMIT 1);
	LOCK discussions_users;
	LOCK articles_discussions;

	IF  (SELECT COUNT(users_poll_responses.username) from users_poll_responses WHERE users_poll_responses.article_id = _article_id AND users_poll_responses.username NOT IN (SELECT DISTINCT discussions_users.username FROM discussions_users, articles_discussions WHERE articles_discussions.article_id = _article_id AND discussions_users.discussion_id = articles_discussions.discussion_id)) = min_discussion_users THEN  

		_discussion_id := 'DIS_' || ((EXTRACT(epoch from now())*100)::int8)::text || '%' || _article_id::text || '%%';
		INSERT INTO articles_discussions (article_id, discussion_id) VALUES (_article_id, _discussion_id);

		WITH queued_users AS (SELECT DISTINCT users_poll_responses.username FROM users_poll_responses WHERE users_poll_responses.article_id = _article_id AND users_poll_responses.username NOT IN (SELECT DISTINCT discussions_users.username FROM discussions_users, articles_discussions WHERE articles_discussions.article_id = _article_id AND discussions_users.discussion_id = articles_discussions.discussion_id)) INSERT INTO discussions_users ( SELECT _discussion_id, queued_users.username FROM queued_users);
		
		CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS SELECT  'new_discussion'::TEXT AS action_taken, _discussion_id::TEXT AS discussion_id, discussions_users.username::TEXT AS username FROM discussions_users WHERE discussions_users.discussion_id = _discussion_id;
		
		RAISE NOTICE 'creating new_discussion';	

	ELSIF EXISTS (SELECT g.id, COUNT(g.poll_answer) FROM 
		( SELECT DISTINCT users_poll_responses.username, articles_discussions.discussion_id AS id, users_poll_responses.poll_answer AS poll_answer FROM articles_discussions, discussions_users, users_poll_responses, articles WHERE discussions_users.discussion_id = articles_discussions.discussion_id AND articles_discussions.article_id = _article_id AND articles.article_id = users_poll_responses.article_id GROUP BY users_poll_responses.username ,  articles_discussions.discussion_id, users_poll_responses.poll_answer 
		) g
		GROUP BY g.id HAVING SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) / ((CASE WHEN COUNT(g.poll_answer) = 0 THEN 1 ELSE COUNT(g.poll_answer) END)::float) > min_discussion_users_ratio AND ((SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) + 1)::float) / GREATEST(SUM(CASE WHEN g.poll_answer != _poll_answer THEN 1 ELSE 0 END ), 0.01) < max_discussion_ratio AND COUNT(g.poll_answer)  < max_discussion_users 
		) THEN
	
		RAISE NOTICE 'adding to existing_discussion';	

		_discussion_id = 
		(
			WITH discussion_info AS (
				SELECT g.id AS id, COUNT(g.poll_answer) FROM ( 
					SELECT DISTINCT users_poll_responses.username, articles_discussions.discussion_id AS id, users_poll_responses.poll_answer AS poll_answer FROM articles_discussions, discussions_users, users_poll_responses, articles WHERE discussions_users.discussion_id = articles_discussions.discussion_id AND articles_discussions.article_id = _article_id AND articles.article_id = users_poll_responses.article_id GROUP BY users_poll_responses.username ,  articles_discussions.discussion_id, users_poll_responses.poll_answer 
				) g
			GROUP BY g.id 
			HAVING SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) / ((CASE WHEN COUNT(g.poll_answer) = 0 THEN 1 ELSE COUNT(g.poll_answer) END)::float) > min_discussion_users_ratio 
				AND ((SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) + 1)::float) / GREATEST(SUM(CASE WHEN g.poll_answer != _poll_answer THEN 1 ELSE 0 END ), 0.01) < max_discussion_ratio 
			ORDER BY SUM(CASE WHEN g.poll_answer::char = _poll_answer THEN 1 ELSE 0 END) / ((CASE WHEN COUNT(g.poll_answer) = 0 THEN 1 ELSE COUNT(g.poll_answer) END)::float ) 
			LIMIT 1
		)
		SELECT discussion_info.id FROM discussion_info);
		INSERT INTO discussions_users (discussion_id, username) VALUES (_discussion_id, _username);
			
		CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS  SELECT  'existing_discussion'::TEXT AS action_taken, _discussion_id AS discussion_id, 'null'::TEXT AS username; 
	
	ELSE

		CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS SELECT 'stored_preference'::TEXT AS action_taken, 'null'::TEXT AS discussion_id, 'null'::TEXT AS username; 
		RAISE NOTICE 'storing prefernces';	
	END IF;
	RETURN QUERY SELECT * FROM tmp_container ;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION exit_discussion(_discussion_id TEXT, _username TEXT, _article_id INT) RETURNS   TABLE (
 action_taken TEXT,
 username TEXT
)  AS $$
	DECLARE
		_user_count_in_discussion INT;
	BEGIN
		LOCK discussions_users;
		LOCK articles_discussions;
		DELETE FROM discussions_users WHERE discussions_users.discussion_id = _discussion_id AND discussions_users.username = _username;
		DELETE FROM users_poll_responses WHERE users_poll_responses.article_id = _article_id::INT AND users_poll_responses.username = _username;


		_user_count_in_discussion := (SELECT COUNT(*) FROM discussions_users WHERE discussion_id = _discussion_id);

		IF _user_count_in_discussion > 0 THEN
			CREATE TEMPORARY TABLE _new_users_in_discussion ON COMMIT DROP AS SELECT DISTINCT users_poll_responses.username AS username FROM users_poll_responses WHERE users_poll_responses.article_id = _article_id AND users_poll_responses.username NOT IN (SELECT DISTINCT discussions_users.username FROM discussions_users, articles_discussions WHERE articles_discussions.article_id = _article_id AND discussions_users.discussion_id = articles_discussions.discussion_id);

			IF EXISTS (SELECT * FROM _new_users_in_discussion) THEN
				INSERT INTO discussions_users ( SELECT _discussion_id, _new_users_in_discussion.username FROM _new_users_in_discussion);
				CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS SELECT 'deleted_user_added_queued_users_to_discussion'::TEXT AS action_taken, _new_users_in_discussion.username::TEXT AS username FROM _new_users_in_discussion;
			ELSE
				CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS SELECT 'deleted_user'::TEXT AS action_taken, 'null'::TEXT AS username;
			END IF;
			
		ELSE
			CREATE TEMPORARY TABLE tmp_container ON COMMIT DROP AS SELECT 'deleted_discussion'::TEXT AS action_taken, 'null'::TEXT AS username;
			DELETE FROM articles_discussions WHERE discussion_id = _discussion_id;
		END IF;
		RETURN QUERY SELECT * FROM tmp_container ;
END;
$$ LANGUAGE plpgsql;