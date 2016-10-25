
function FindProxyForURL(url, host) {
    if (shExpMatch(host, "*.vm")
        || shExpMatch(host, "*.lan")
        || shExpMatch(host, "*.local")
        || shExpMatch(host, "hardison.net")) {
        return "DIRECT";
    }
    return "SOCKS5 127.0.0.1:8888; DIRECT";
}
