/*
 *	$Id: array.c,v 1.1 1997-04-11 17:40:53 haley Exp $
 */
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
