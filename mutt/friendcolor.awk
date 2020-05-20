#!/usr/bin/awk -f

BEGIN {
	FGCOLOR="yellow";
	BGCOLOR="default"
	FS=": ";
}

/^email: / {
	split($2, a, ",");
	for (i in a)
		printf("color index %s %s '~f %s'\n", FGCOLOR, BGCOLOR, a[i]);

}
