/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

#include	<stdio.h>

extern	char	*malloc();
extern	char	*realloc();

char	*icMalloc(size)
	unsigned	size;
{
	char	*ptr;

	if (! size) return (NULL);

	if ((ptr = malloc (size)) == NULL) {

		(void) fprintf(stderr, "libncarv: Error allocating memory\n");
		exit(1);
	}

	return (ptr);
}

char	*icRealloc(ptr, size)
	char		*ptr;
	unsigned	size;
{

	if (! size) return (NULL);

	if ((ptr = realloc (ptr, size)) == NULL) {

		(void) fprintf(stderr, "libncarv: Error allocating memory\n");
		exit(1);
	}

	return (ptr);
}
