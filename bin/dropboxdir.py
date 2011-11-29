#!/usr/bin/env python

# Script for showing or setting the dropbox folder.
#
# Execute without options to show current dropbox folder (if non-default).
# Execute with --setfolder=/foo/bar to set new dropbox folder.
#
# Written by Wim Coenen (wcoenen@gmail.com).

import base64
import optparse
import os
import os.path
import sqlite3

# parse command line options
cmdparser = optparse.OptionParser()
cmdparser.add_option("-s","--setfolder", dest="folder",
  help="set dropbox folder")
(options, args) = cmdparser.parse_args()

db_path = os.path.expanduser("~/.dropbox/config.db")
db = sqlite3.connect(db_path)
cursor = db.cursor()

# get dropbox_path
cursor.execute("select value from config where key='dropbox_path'")
dropbox_path = "<default>"
for entry in cursor:
   dropbox_path = entry[0]
print "current dropbox path: %s" % dropbox_path

if not options.folder is None:
   new_path = os.path.abspath(options.folder)
   cursor.execute("delete from config where key='dropbox_path'")
   cursor.execute("insert into config (key,value) values (?,?)", \
      ("dropbox_path", new_path))
   db.commit()
   print "new dropbox path: %s" % options.folder

db.close()
