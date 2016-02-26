import datetime
import dateutil.relativedelta
import os
import sys
import time


def remove_old_media():
    media_path = os.path.dirname(os.path.abspath(__file__)) + '/media/'
    for file in os.listdir(media_path):
        file_path = media_path + file
        (mode, ino, dev, nlink, uid, gid, size, atime, mtime, ctime) = os.stat(file_path)
        file_creation_time = datetime.datetime.fromtimestamp(ctime)
        current_time = datetime.datetime.now()
        relativde_td = dateutil.relativedelta.relativedelta (current_time, file_creation_time)
        if relativde_td.hours >= 6:
            os.remove(file_path)


if __name__ == "__main__":
    remove_old_media()
