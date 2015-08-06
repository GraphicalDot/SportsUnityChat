# SportsUnityChat
Apis:- 
1> Store locations
2> Get Facebook friends 
2> Register
3> Create

To Do :-
1> Retrieve nearby users 


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
