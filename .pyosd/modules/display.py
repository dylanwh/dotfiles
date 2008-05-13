#!/usr/bin/env python
#
# $Id$
#
# Time-stamp: <2003-12-16 13:45:51 resolve>
#
# Copyright (C) Damien Elmes <resolve@repose.cx>, 2001.
# This file is licensed under the GPL. Please see COPYING for more details.
#


"""
Plugin for displaying arbitrary messages.
"""

import pyosd, pyosd.daemon as pyd
import string

class plugin:

    def __init__(self):
        self.plugin_name = "display"
        self.plugin_desc = "display an arbitrary message"
        self.plugin_keys = ["display"]

    def display(self, *args):
        pyd.top.set_colour("#bbbbFF")
        pyd.top.set_timeout(3)
        pyd.top.display(string.join(args))
