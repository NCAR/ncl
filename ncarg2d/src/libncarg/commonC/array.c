/*
 *	$Id: array.c,v 1.4 2008-07-23 16:16:42 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

void Pad_char_array(str1,str2,n,maxlen)
    char *str1[], *str2;
    int n, maxlen;
{
    int i, j;

    str2[0] = '\0';
    for( i = 0; i < n; i++ ) {
        strncat(str2,str1[i],strlen(str1[i]));
        for( j = strlen(str1[i]); j < maxlen; j++ ) strncat(str2," ",1);
    }
    str2[n*maxlen] = '\0';
}
