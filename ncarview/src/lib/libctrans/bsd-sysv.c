/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.01                             *
*                                                                      *
***********************************************************************/

#include <stdio.h>

/*
 * don't compile on bsd machines
 */
#ifdef	SYSV
char *
strchr(s, c)
	char	*s;
	int	c;
{
	while(*s != '\0')
	{
		if (*s == c)
			return s;
		else
			s++;
	}
	return( (char *)NULL );
}

char *
strrchr(s, c)
	char	*s;
	int	c;
{
	char	*mark = (char *) NULL;

	while (*s != '\0')
	{
		if (*s == c)
			mark = s++;
		else
			s++;
	}
	return(mark);
}

#ifndef	CRAY
bcopy(b1, b2, length)
	char	*b1;
	char	*b2;
	int	length;
{
	unsigned int	b1start, b1end, b2start, b2end;
	char		*copy, *malloc(), *p1, *p2;
	int		i;

	if (length <= 0) return(0);

	b1start = (unsigned int) b1;
	b1end = b1start + length - 1;
	b2start = (unsigned int) b2;
	b2end = b2start + length - 1;

	if (b1start >= b2start && b1start <= b2end ||
	b1end >= b2start && b1end <= b2end)
	{
		copy = malloc((unsigned) length);

		for(p1=b1,p2=copy,i=0; i<length; i++)
		{
			*p2++ = *p1++;
		}

		for(p1=copy,p2=b2,i=0; i<length; i++)
		{
			*p2++ = *p1++;
		}

		if (copy) cfree((char * ) copy);
	}
	else
	{
		while(length--) *b2++ = *b1++;
	}

	return(1);
}

bzero(b1, length)
	char	*b1;
	int	length;
{
	if (length > 0)
		while (length--)
		{
			*b1++ = '\0';
		}
}


int
bcmp(b1, b2, length)
	char	*b1, *b2;
	int	length;
{
	if (length > 0) {
		while (length--)
		{
			if (*b1++ != *b2++) return(1);
		}
	}
	return(0);
}
#endif CRAY

#endif	SYSV
