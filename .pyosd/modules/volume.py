#!/usr/bin/env python

"""
Plugin for PyOSDd which facilitates changing volume
"""

from alsaaudio import Mixer
from sys import argv, stdin
import pyosd, pyosd.daemon as pyd

print "pants"

class VCon(object):
	def __init__(self, osd):
		self.pcm = Mixer('PCM')
		self.osd = osd

	def set(self, vol):
		if self.pcm.getmute()[0]: return
		self.pcm.setvolume(int(vol))
		nvol = self.pcm.getvolume()[0]
		self.display(nvol)

	def up(self, level=10):
		if self.pcm.getmute()[0]: return
		vol = self.pcm.getvolume()[1]
		vol = vol + int(level)
		if vol > 100: vol = 100
		self.pcm.setvolume(vol)
		self.display(vol)

	def down(self, level=10):
		if self.pcm.getmute()[0]: return
		vol = self.pcm.getvolume()[1]
		vol = vol - int(level)
		if vol < 0: vol = 0
		self.pcm.setvolume(vol)
		self.display(vol)

	def mute(self):
		if self.pcm.getmute()[0]:
			self.pcm.setmute(False)
			self.osd.display('Unmuted')
			vol = self.pcm.getvolume()[0]
			self.osd.display(vol, type=pyosd.TYPE_PERCENT, line=1)
		else:
			self.pcm.setmute(True)
			self.osd.display('Muted')
			self.osd.display(0, type=pyosd.TYPE_PERCENT, line=1)

	def display(self, vol):
		self.osd.display("Volume (%d%%)" % vol)
		self.osd.display(vol, type=pyosd.TYPE_PERCENT, line=1)


class plugin:
	def __init__(self):
		self.plugin_name = "volume"
		self.plugin_desc = "Alter main system volume"
		self.plugin_keys = ["vol"]
		osd = pyosd.osd()
		#osd.set_font('-bitstream-charter-*-r-*-*-*-240-100-*-*-*-*-*')
		osd.set_pos(pyosd.POS_MID)
		osd.set_align(pyosd.ALIGN_CENTER)
		osd.set_colour("#bbbbFF")
		osd.set_timeout(3)
		self.vcon = VCon(osd)

	def vol(self, *args):
		args = list(args)
		cmd = args[0]
		try:
			method = getattr(self.vcon, cmd)
			method(*args[1:])
		except AttributeError:
			print "Unknown command: %s" % cmd


