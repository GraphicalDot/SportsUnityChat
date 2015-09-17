** All web requests are GET unless otherwise mentioned ** 

# SportsUnityChat
Apis:- 
1> Store locations
2> Get Facebook friends 
2> Register
3> Create
4> Store profile pictures
5> Publish Football Notifcations 

To Do :-
1> Retrieve nearby users 
2> Publish Football Notifcations 
3> Publish Tennis Notifcations 
4> Publish Cricket Notifcations 



Store location:- 

Store location is a GET REST api which stores the lat long of the users, so that the radius queries can be run.
Format :- <ip>/location?user=ashwin@mm.io&lat=<latitude coordinates>&lng=<longtitude coordinates>
Result:- Status message and info 

Get Facebook Friends:-

Get facebook friends using the auth token provided at the time of facebook login.
Format:-  <ip>/fb_friends?fb_id=<fb_id>&token=<auth_token>
Result:- A json of facebook friends 

Register:-
Register a new number for authorization
Format:- <ip>/register?number=<phone_number>
Result:- Status Message and info

Create:-
Create an existing number for xmpp account
Format:- <ip>/authorize?phone_number=<phone_number>&auth_code=<auth_code>
Result:- Json message having user password for authenticating in ejabberd services and/or appropriate status code and message 

Store Profile Picture:
This api gives the functionality of storing picture in amazon s3
Format:- POST request to <ip>/profile_pic
Params are :-
file : The standard file parameter while posting files 
username: username of the user
password: password of the user


Publish Football Notification:
Publish football notifcation to the relevant subsctibers in the xmpp network
Format:- POST request to <ip>/football_notifications
Params are:- 
Json encoded body having the following parameters :
	[
		"league_id" ,
		"home_team" ,
		"away_team" ,
		"match_id" ,
		"home_team_score" ,
		"away_team_score" ,
		"match_status" ,
		"match_time"
	] 
