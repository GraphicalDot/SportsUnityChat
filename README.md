** All web requests are GET unless otherwise mentioned ** 

# SportsUnityChat
Apis:- 
1> Store locations
2> Get Facebook friends 
2> Register
3> Create
4> Store profile pictures
5> Publish Football Notifications

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

Store Profile Picture:
This api gives the functionality of storing picture in amazon s3
Format:- POST request to <ip>/profile_pic
Params are :-
file : The standard file parameter while posting files 
username: username of the user
password: password of the user

Publish Football Notification:
Publish football notification to the relevant subscribers in the xmpp network
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


Check for present media: 
Format:- GET request to <ip>/media_present?name=<md5 name>
Response:- JSON {"status": STATUS, "info": INFO}
STATUS = 200 if present
INFO = 'Present' if present

** INSTALLATION STEPS **

1. Project Setup
a. Clone repository:
>> git clone git@github.com:kaali-python/SportsUnityChat.git

b. Install local packages:
>> sudo apt-get install python-pip
>> sudo apt-get install erlang=18.1

* If faced problem in installing erlang, refer following steps:
    * sudo apt-get -y install build-essential m4 libncurses5-dev libssh-dev unixodbc-dev libgmp3-dev libwxgtk2.8-dev libglu1-mesa-dev fop xsltproc default-jdk
     * wget http://www.erlang.org/download/otp_src_18.1.tar.gz
     * tar -xvzf otp_src_18.1.tar.gz
     * chmod -R 777 otp_src_18.1
     * cd otp_src_18.1
     * ./configure
     * sudo make
     * sudo make install

>> Install Postgres following steps as described on : https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04
>> Install Tornado as described in : http://www.tornadoweb.org/en/stable/#installation
>> pip install requests
>> pip install requests-toolbelt
>> pip install python-magic

Install 'ejabberd' (version: 15.06) in following steps:
i) git clone https://github.com/processone/ejabberd.git
ii) cd ejabberd
iii) ./autogen.sh
iv) ./configure --enable-pgsql

* If found any problem with SQLite3 packages, follow below steps:
 >> wget http://www.sqlite.org/sqlite-autoconf-3070603.tar.gz
 >> tar xvfz sqlite-autoconf-3070603.tar.gz
 >> cd sqlite-autoconf-3070603
 >> ./configure
 >> make
 >> make install

v) [sudo] make

* If finds problem with pam, install pam separately following below steps:
 >> sudo apt-get install libpam0g-dev

* If finds problem with yaml.h file:
  >> wget http://pyyaml.org/download/libyaml/yaml-0.1.5.tar.gz
  >> tar -xvf yaml-0.1.5.tar.gz
  >> cd yaml-0.1.5
  >> ./configure
  >> sudo make
  >> sudo make installs

vi) [sudo] make install
vii) Start ejabberd service using this command: ejabberdctl start

Note: Logs for ejabberd could be seen using: vi /var/logs/ejabberd/ejabberd.log or tail -f /var/logs/ejabberd/ejabberd.log




** TESTING **
Tests are written in api_test.py and notification_test.py 
api_test.py requires a large media file to test which should be named as big.mp4