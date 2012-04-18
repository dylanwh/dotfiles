import sys
import gtk
import gnomekeyring as gkey

prioritized = ['inbox', 'lists']

def unfair_cmp(x, y):
   for prefix in prioritized:
       xsw = x.startswith(prefix)
       ysw = y.startswith(prefix)
       if xsw and ysw:
          return cmp(x, y)
       elif xsw:
          return -1
       elif ysw:
          return +1
   return cmp(x, y)

class Keyring(object):
    def __init__(self, name, server, protocol):
        self._name = name
        self._server = server
        self._protocol = protocol
        self._keyring = gkey.get_default_keyring_sync()

    def has_credentials(self):
        try:
            attrs = {"server": self._server, "protocol": self._protocol}
            items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
            return len(items) > 0
        except gkey.DeniedError:
            return False

    def get_credentials(self):
        attrs = {"server": self._server, "protocol": self._protocol}
        items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
        return (items[0].attributes["user"], items[0].secret)

    def set_credentials(self, (user, pw)):
        attrs = {
                "user": user,
                "server": self._server,
                "protocol": self._protocol,
            }
        gkey.item_create_sync(gkey.get_default_keyring_sync(),
                gkey.ITEM_NETWORK_PASSWORD, self._name, attrs, pw, True)

def get_username(server):
    keyring = Keyring("offlineimap", server, "imap")
    (username, password) = keyring.get_credentials()
    return username

def get_password(server):
	keyring = Keyring("offlineimap", server, "imap")
	(username, password) = keyring.get_credentials()
	return password
