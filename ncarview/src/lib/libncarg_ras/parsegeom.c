#include <stdio.h>

main()
{
	int	w, h, x, y;

	status = parsegeom("640x480+10+20", &w, &h, &x, &y);
}

parsegeom(geom, w, h, x, y)
	char	*geom;
	int	*w, *h, *x, *y;
{
	char	*p;
	int	width = 0;
	int	height = 0;
	int	x = 0;
	int	y = 0;

	p = geom;

	while(*p != '\0') {
		if (*p == 'x') {
			if (
