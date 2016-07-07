#!/usr/bin/python
 
from time import gmtime, strftime
import subprocess
import os
import glob
import time
import boto3 
# change these as appropriate for your platform/environment :
USER = "madmachines"
PASS = "password_production_db"
HOST = "localhost"

DATABASE_NAME = "production_db"
BACKUP_DIR = "/home/ubuntu/"
dumper = """ pg_dump -U %s -h %s -Z 9 -f %s -b -F c -d %s  """                  
 
def log(string):
    print time.strftime("%Y-%m-%d-%H-%M-%S", time.gmtime()) + ": " + str(string)
 
# Change the value in brackets to keep more/fewer files. time.time() returns seconds since 1970...
# currently set to 2 days ago from when this script starts to run.
 
x_days_ago = time.time() - ( 60 * 60 * 24 * 2 )
 
os.putenv('PGPASSWORD', PASS)
 
log("dump started for %s" % DATABASE_NAME)
thetime = str(strftime("%Y-%m-%d-%H-%M")) 
file_name = DATABASE_NAME + '_' + thetime + ".sql"
file_loc = BACKUP_DIR + file_name
#Run the pg_dump command to the right directory
command = dumper % (USER, HOST, BACKUP_DIR + file_name, DATABASE_NAME)
log(command)
subprocess.call(command,shell = True)
log("%s dump finished" % DATABASE_NAME)
 
log("Backup job complete.")

# from IPython import embedls

# embed()
boto3.client(
            's3',
            aws_access_key_id = "AKIAJQ4YKSYEDJGYH7ZA",
            aws_secret_access_key = "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"
).upload_file(file_loc, "sports.unity.database.backup", file_name)