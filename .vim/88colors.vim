" Foo:0:
" Foo:1:
" Foo:2:
" Foo:3:
" Foo:4:
" Foo:5:
" Foo:6:
" Foo:7:
" Foo:8:
" Foo:9:
" Foo:10:
" Foo:11:
" Foo:12:
" Foo:13:
" Foo:14:
" Foo:15:
" Foo:16:
" Foo:17:
" Foo:18:
" Foo:19:
" Foo:20:
" Foo:21:
" Foo:22:
" Foo:23:
" Foo:24:
" Foo:25:
" Foo:26:
" Foo:27:
" Foo:28:
" Foo:29:
" Foo:30:
" Foo:31:
" Foo:32:
" Foo:33:
" Foo:34:
" Foo:35:
" Foo:36:
" Foo:37:
" Foo:38:
" Foo:39:
" Foo:40:
" Foo:41:
" Foo:42:
" Foo:43:
" Foo:44:
" Foo:45:
" Foo:46:
" Foo:47:
" Foo:48:
" Foo:49:
" Foo:50:
" Foo:51:
" Foo:52:
" Foo:53:
" Foo:54:
" Foo:55:
" Foo:56:
" Foo:57:
" Foo:58:
" Foo:59:
" Foo:60:
" Foo:61:
" Foo:62:
" Foo:63:
" Foo:64:
" Foo:65:
" Foo:66:
" Foo:67:
" Foo:68:
" Foo:69:
" Foo:70:
" Foo:71:
" Foo:72:
" Foo:73:
" Foo:74:
" Foo:75:
" Foo:76:
" Foo:77:
" Foo:78:
" Foo:79:
" Foo:80:
" Foo:81:
" Foo:82:
" Foo:83:
" Foo:84:
" Foo:85:
" Foo:86:
" Foo:87:
" Foo:88:

let index = 0
while index < 88
	exec "syn match User" . index . " /\" Foo:" . index . ":/"
	exec "hi User" . index . " ctermfg=" . index
	let index = index + 1
endwhile
