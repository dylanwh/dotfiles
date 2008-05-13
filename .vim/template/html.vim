1append
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
	<head>
		<meta name="author" content="@AUTHOR@" />
		<title>Untitled</title>
	</head>
	<body>
	@CURSOR@
	</body>
</html>
.

%substitute/@AUTHOR@/\=$REALNAME/g
nohlsearch
%substitute/@CURSOR@//g
