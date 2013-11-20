#!/usr/bin/env python2
"""
pr0nit:
    Completely standalone, 1-file, cross platform wallpaper switcher. Obtains
    wallpaper from reddit. No dependencies other than python and stdlib.

Suggested subreddits: 
    /r/earthporn, /r/usaporn, /r/cityporn. Works on any

Author: Tom Dignan <tom@tomdignan.com>
Mac OS X Support: James Clarke 

License: 
    Public Domain

Disclaimer: 
    Neither the original author nor any contributor is responsible
    for the results of your use of this program.
"""

import md5 
import argparse 
import uuid
import os
import json
import urllib2
import time
import sys
import re

from BaseHTTPServer import HTTPServer, BaseHTTPRequestHandler
from SimpleHTTPServer import SimpleHTTPRequestHandler

import webbrowser

if sys.platform == 'darwin': 
    from Foundation import NSAppleScript
    from Cocoa import NSApplication

if sys.platform == 'win32':
    from ctypes import windll
    SPI_SETDESKWALLPAPER = 0x14
    SPIF_UPDATEINIFILE = 0x1
    SPIF_SENDWININICHANGE = 0x2

DEBUG = False
WALLPAPER_CACHE_DIR = "%s/.cache/wallpaper" % os.getenv("HOME")
DEFAULT_SUBREDDIT = "earthporn" # try also, r/usaporn
DEFAULT_WALLPAPER_CMD = "feh --bg-scale"
DEFAULT_FRAME_SPEED = 300
WEB_INTERFACE_PORT = 9090
STATIC_FILE_PORT = 9092


class WebInterface(BaseHTTPRequestHandler):

    def _make_wallpaper_html(self):
        html = []
        
        wallpaper_map = wallpaper_setter.all_wallpaper_by_subreddit
        all_subreddits = wallpaper_map.keys()
        all_subreddits.sort()

        for subreddit in all_subreddits:
            wallpaper = wallpaper_map[subreddit]
            html.append('<section id="%s"><h2>%s</h2><br />' % (subreddit, subreddit))
            i = 0
            for w in wallpaper:
                if i % 3 == 0:
                    html.append('<div class="row-fluid">')
                html.append("""
<div class="span4 frame">
    <div class="row-fluid">
        <div class="span12">
""")                
                thumbnail_url = "http://localhost:%d/%s/%s" % (STATIC_FILE_PORT, subreddit,
                    w['thumbnail'])
                wallpaper_url = "http://localhost:%d/%s/%s" % (STATIC_FILE_PORT, subreddit,
                    w['filename'])
            
                html.append('<a class="fancybox_preview" href="%s"><img src="%s"/></a>' % 
                    (wallpaper_url, thumbnail_url))

                html.append("""
</div></div>
<br />
<div class="row-fluid">
    <div class="span2"></div>
    <div class="span3">
        <a class="btn preview">Preview</a>
    </div>
    <div class="span2"></div>
    <div class="span3">
        <a href="http://localhost:%d?set=%s" class="btn">Set</a>
    </div>
    <div class="span2"></div>
</div>
""" % (WEB_INTERFACE_PORT, w['path']))
                html.append('</div>')

                if i % 3 == 2:
                    html.append('</div><br />')

                i += 1
            html.append("</section>")

        return "\n".join(html)


    def _make_wallpaper_nav_html(self):
        html = []
        clazz = "active"
        for subreddit in wallpaper_setter.all_subreddits:
            html.append('<li class="%s navlink"><a href="#%s">%s</a></li>' % (clazz, subreddit, subreddit))
            clazz = ""
        return "\n".join(html)

    
    def _render_index_html(self):
        return """<!doctype html>
<html lang="en">
<head>
    <title>Pr0nit: a Reddit-Powered Wallpaper Switcher</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link
        href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"
        rel="stylesheet">
    <link
        href="//cdnjs.cloudflare.com/ajax/libs/fancybox/2.1.5/jquery.fancybox.min.css"
        rel="stylesheet">
    <style type="text/css">
#content {
    position: relative;
    margin-top: 70px;
}

section {
    padding-top: 70px;
    margin-top: -70px;
}

.frame {
    text-align: center;
    padding: 10px;
    border: 1px solid #eee;
}
    </style>

</head>
<body>
    <div class="navbar navbar-inverse navbar-fixed-top">
        <div class="navbar-inner">
            <div class="container">
                <button type="button" class="btn btn-navbar" data-toggle="collapse" 
                         data-target=".nav-collapse">
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
                <a class="brand" href="#">Pr0nit</a>
                <div class="nav-collapse collapse">
                    <ul class="nav">
                        %s
                    </ul>
                </div>
            </div>
        </div>
    </div>
    <div class="container" id="content">
        %s
    </div>
    <script src="http://code.jquery.com/jquery.js"></script>
    <script src="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"></script>
    <script src="//cdnjs.cloudflare.com/ajax/libs/fancybox/2.1.5/jquery.fancybox.min.js"></script>
    <script>
$(document).ready(function() {
    $(".navlink").click(function() {
        $(".navlink").removeClass("active");
        $(this).addClass("active");
    });

    $(".preview").click(function() {
        $(this).closest(".row-fluid").siblings().find(".fancybox_preview").click();
    });
    $(".fancybox_preview").fancybox();
})
    </script>
</body>
</html>
""" % (self._make_wallpaper_nav_html(), self._make_wallpaper_html())


    def show_index(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.end_headers()
        self.wfile.write(self._render_index_html())
        self.wfile.close()


    def do_GET(self):
        m = re.match("/\?set=(.*)", self.path)
        if m is None:
            self.show_index()
        else:
            # set wallpaper
            wallpaper_path = m.group(1)
            self.wallpaper_setter.set_wallpaper(wallpaper_path)
            self.show_index()


class RedditWallpaperSetter(object):

    def __init__(self, subreddit, cache_dir, frame_speed, monitors=1,
                 **kwargs):
        """ 
        Called every time the program is started. Ensures that the directory
        structure is intact. 
        """
   
        self._create_directory_if_not_exists(cache_dir) 
        self.cache_root_dir = cache_dir
        self.cache_dir = os.path.join(cache_dir, subreddit)
        self._create_directory_if_not_exists(self.cache_dir)
        self.subreddit = "r/" + subreddit
        self.frame_speed = frame_speed
        self.monitors = monitors


    @property
    def all_subreddits(self):
        subreddits = []
        for filename in os.listdir(self.cache_root_dir):
            full_path = os.path.join(self.cache_root_dir, filename)
            if os.path.isdir(full_path):
                subreddits.append(filename)
        subreddits.sort()
        return subreddits

    
    @property
    def all_wallpaper_by_subreddit(self):
        wallpaper = {}
        for subreddit in self.all_subreddits:
            wallpaper[subreddit] = [] 
            path = os.path.join(self.cache_root_dir, subreddit)
            for filename in os.listdir(path):
                # Don't try to generate thumbnails for thumbnails
                if len(filename) == 37:
                    continue
                
                thumbnail = filename.replace(".jpg", "m.jpg")
                wallpaper[subreddit].append(dict(filename=filename,
                    thumbnail=thumbnail, path=os.path.join(path,filename)))
                    

        return wallpaper


    def _create_directory_if_not_exists(self, path):
        if not os.path.isdir(path):
            os.mkdir(path) 


    def update_wallpaper(self):
        """
        Downloads latest wallpaper and displays one at a time, sleeping
        at the specified frame speed.
        """
        try:
            wallpaper_urls = self._get_wallpaper_urls()
        except urllib2.HTTPError as e:
            print "Retrying in 10 seconds: %r %d" % (e, e.code)
            time.sleep(10)
            self.run()
            return

        for url in wallpaper_urls:
            try:
                self._cache_wallpaper(url, is_thumb=True)
                self._cache_wallpaper(url)            
            except urllib2.HTTPError, e:
                print "%r %r skipping %s" % (e, e.code, url)


    def slideshow(self):
        for filename in os.listdir(self.cache_dir): 
            self.set_wallpaper(os.path.join(self.cache_dir, filename))
            time.sleep(self.frame_speed)


    def _get_wallpaper_urls(self):
        """
        Downloads the current URLs from the first page of results for
        the target subreddit.
        """
        url = "http://www.reddit.com/%s/.json" % self.subreddit
        headers = { 'User-Agent' : 'Pr0nit/1.0' }
        req = urllib2.Request(url, None, headers)
        f = urllib2.urlopen(req)
        data = json.loads(f.read())['data']

        wallpaper_urls = []
        for child in data['children']:
            url = child['data']['url']

            # If it doesn't end in jpg, it probably goes to another HTML page.
            # Easiest to just exclude them.
            #
            # If it's not hosted on imgur, we don't get a free thumbnail. Just
            # skip it.
            if url.lower().endswith(".jpg") and "imgur.com" in url:
                wallpaper_urls.append(url)

        return wallpaper_urls


    def _cache_wallpaper(self, url, is_thumb=False):
        """ 
        Saves the wallpaper at the target `url` in `cache_dir` but only if
        there is no file in the cache which appears to be the target wallpaper
        already. The heuristic used is an md5sum of the target wallpaper's url.
        """ 
        if is_thumb:
            file_in = urllib2.urlopen(url.replace(".jpg", "m.jpg"))
        else:
            file_in = urllib2.urlopen(url)

        summer = md5.new()
        summer.update(url)
        filename = summer.hexdigest() + ".jpg"

        if is_thumb:
            path = os.path.join(self.cache_dir, filename.replace(".jpg",
                                "m.jpg"))
        else:
            path = os.path.join(self.cache_dir,  filename)

        # only create the file if it does not already exist in the cache.

        if not os.path.isfile(path): 
            file_out = open(path, "wb")
            file_out.write(file_in.read())
            file_out.close()
            file_in.close()
        else:
            print "cache hit: %r %r" % (url, path)

        return path


    def set_wallpaper(self, path):
        raise Exception("Not implemented")


class RedditWallpaperSetterLinux(RedditWallpaperSetter):

    def set_wallpaper(self, path):
        if os.environ.get('DESKTOP_SESSION') == "cinnamon":
            os.system("gsettings set org.gnome.desktop.background picture-options scaled") #Change to preference
            os.system("gsettings set org.gnome.desktop.background picture-uri 'file://%s'" % path)
        else:
            os.system("%s %s" % (DEFAULT_WALLPAPER_CMD, path))


class RedditWallpaperSetterXFCE4(RedditWallpaperSetter):

    def set_wallpaper(self, path):
        for i in range(self.monitors):
            os.system("xfconf-query -c xfce4-desktop -p "  
               " /backdrop/screen0/monitor%d/image-path -s %s" % (i, path))
                

class RedditWallpaperSetterOSX(RedditWallpaperSetter):
    
    def set_wallpaper(self, path):
        script = NSAppleScript.alloc().initWithSource_("""
            tell app "Finder" to set desktop picture to POSIX file "%s"
        """ % path)
        script.executeAndReturnError_(None)


class RedditWallpaperSetterWindows(RedditWallpaperSetter):

    def set_wallpaper(self, path):
        windll.user32.SystemParametersInfoA(SPI_SETDESKWALLPAPER, 0, path,
                                            SPIF_UPDATEINIFILE | SPIF_SENDWININICHANGE)


if __name__ == "__main__":
    
    parser = argparse.ArgumentParser(description="""Set the wallpaper from
 reddit""")

    parser.add_argument("--subreddit", metavar="<name>", type=str, nargs=1,
        default=DEFAULT_SUBREDDIT, help="Target subreddit to scrape for "
        " wallpaper.")

    parser.add_argument("--monitors", metavar="<number>", type=int, nargs=1,
        default=1, help="Number of monitors in your setup (XFCE4 only)")

    parser.add_argument("--frame-speed", metavar="<seconds>", type=int,
        default=DEFAULT_FRAME_SPEED, nargs=1, 
        help="Number of seconds to elapse between switching wallpaper.")
    
    parser.add_argument("--wallpaper-dir", metavar="<path>", type=str, nargs=1,
        default=WALLPAPER_CACHE_DIR, help="Directory to read/write wallpaper to"
        " and from")

    parser.add_argument("--platform", metavar="<string>", type=str, nargs=1,
        default=sys.platform, help="Target platform, defaults to %r." %
        sys.platform)

    parser.add_argument("--use-web-interface", action="store_true", 
                        help="Enable local web interface.")

    args = parser.parse_args()
    
    get_arg = lambda x : x[0] if not x is None and not isinstance(x, int) \
                        and not isinstance(x, basestring) else x
    use_web_interface = get_arg(args.use_web_interface)
    monitors = get_arg(args.monitors)
    platform = get_arg(args.platform)
    frame_speed = get_arg(args.frame_speed)
    subreddit =  get_arg(args.subreddit)
    wallpaper_dir = get_arg(args.wallpaper_dir)

    if platform == "darwin":
        # Set the activation policy to NSApplicationActivationPolicyAccessory
        # so we don't show the Python dock icon when using PyObjC.
        NSApplication.sharedApplication().setActivationPolicy_(2)
        wallpaper_setter = RedditWallpaperSetterOSX(subreddit, wallpaper_dir,
                                                    frame_speed, monitors=monitors)
    elif platform == "xfce4":
        wallpaper_setter = RedditWallpaperSetterXFCE4(subreddit, wallpaper_dir,
                                                      frame_speed, monitors=monitors)
    elif platform == "win32":
        wallpaper_setter = RedditWallpaperSetterWindows(subreddit, wallpaper_dir,
                                                        frame_speed, monitors=monitors)
    else:
        wallpaper_setter = RedditWallpaperSetterLinux(subreddit, wallpaper_dir,
                                                      frame_speed, monitors=monitors)
    
    print "Updating wallpaper selection, please wait..."
    wallpaper_setter.update_wallpaper()
    print "Done!"

    if use_web_interface:
        # Use a separate process to kick off the web browser so that it can be
        # delayed.
        pid2 = os.fork()
        if pid2 == 0:
            time.sleep(1)
            webbrowser.open("http://localhost:%d" % WEB_INTERFACE_PORT)
            sys.exit(0)
    
        print "Launching web interface...."
        pid = os.fork()
        if pid == 0:
            # Serve static files in separate process.
            os.chdir(wallpaper_dir)
            file_server = HTTPServer(("127.0.0.1", STATIC_FILE_PORT), SimpleHTTPRequestHandler)
            sa = file_server.socket.getsockname()
            print "Serving static files on:", sa[0], "port", sa[1], "..."
            file_server.serve_forever()
        else:
            WebInterface.wallpaper_setter = wallpaper_setter
            server = HTTPServer(("127.0.0.1", WEB_INTERFACE_PORT), WebInterface)
            server.serve_forever()
            # kill the child so it doesn't run away.
            os.kill(pid)
            os.kill(pid2)

        sys.exit(0)
    else:
        print "Running infinite slideshow..."
        # runs here until cancelled.
        while True:
            wallpaper_setter.slideshow()
