# SportsUnityChat
Apis:- 
1> Store locations
2> Get Facebook friends 

To Do :-
1> Retrieve nearby users 
2> Register
3> Authorize


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

Authorize:-
Authorize an existing number for xmpp account
Format:- <ip>/authorize?number=<phone_number>&password=<password>
Result:- Json message having user password for authenticating in ejabberd services and/or appropriate status code and message 



