#!/usr/bin/env python

"""
Plugin for PyOSDd which facilitates changing volume
"""
import mpdclient2
import pyosd, pyosd.daemon as pyd
import socket
import os

class mpc(object):
	def __init__(self, osd):
		self.osd = osd
		self.mpd = mpdclient2.connect()

	def blurb(self):
		song = self.mpd.currentsong()
		try:
			artist = song['artist']
			title  = song['title']
			self.osd.display('%s - %s' % (artist, title))
		except KeyError:
			self.osd.display(os.path.basename(song['file']))
	
	def __call__(self, cmd, *args):
		method = getattr(self, cmd)
		ok = False

		for x in xrange(10):
			if ok: break
			try:
				method(*args)
				ok = True
			except EOFError:
				self.mpd = mpdclient2.connect()
			except socket.error:
				self.mpd = mpdclient2.connect()

		if not ok:
			self.osd.display("error in sending mpc command")

	def toggle(self):
		state = self.mpd.status()['state']
		if state == 'play':
			self.mpd.pause(1)
			self.osd.display("Paused")
		elif state == 'stop':
			self.mpd.play()
			self.blurb()
		elif state == 'pause':
			self.mpd.pause(0)
			self.blurb()

	def next(self):
		self.mpd.next()
		self.blurb()

	def prev(self):
		self.mpd.previous()
		self.blurb()

	def stop(self):
		self.mpd.stop()
		self.osd.display('Stopped')



class plugin:
	def __init__(self):
		self.plugin_name = "MPC"
		self.plugin_desc = "Control MPD"
		self.plugin_keys = ["mpc"]
		osd = pyosd.osd()
		#osd.set_font('-bitstream-charter-*-r-*-*-*-240-100-*-*-*-*-*')
		osd.set_pos(pyosd.POS_BOT)
		osd.set_align(pyosd.ALIGN_CENTER)
		osd.set_colour("#aaffaa")
		osd.set_timeout(3)
		self.mpc = mpc(osd)


