/*
 *	$Id: c_anotat.c,v 1.7 2008-07-23 16:16:41 haley Exp $
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

#include <stdlib.h>
#include <ncarg/ncargC.h>

extern void NGCALLF(anotat,ANOTAT)(NGstring,NGstring,int*,int*,int*,
                                   NGstring,int,int,int);

void c_anotat
#ifdef NeedFuncProto
(
    char *labx,
    char *laby,
    int lbac,
    int lset,
    int ndsh,
    char *dshl[]
)
#else
(labx,laby,lbac,lset,ndsh,dshl)
    char *labx;
    char *laby;
    int lbac;
    int lset;
    int ndsh;
    char *dshl[];
#endif
{
    char *dshl2;
    int i, len, lenx, leny, maxlen;
    NGstring labx2;
    NGstring laby2;
    NGstring dshl3;
    maxlen = 0;
/*
 *  dshl may only be one string
 */
    if( ndsh == 1 && dshl != NULL ) {
        maxlen = strlen((char *)dshl);
        dshl2 = (char *)malloc(maxlen*sizeof(char));
        if( dshl2 == NULL ) {
            (void)fprintf( stderr, "\nc_anotat: Unable to create memory for array dshl2\n" );
            return;
        }
        strncpy(dshl2,(char *)dshl,maxlen);
    }
/*
 *  dshl may be multiple strings
 */
    else if( ndsh > 1 && dshl != NULL ) {
        for( i = 0; i < ndsh; i++ ) {
/*
 * Get the maximum string length
 */
            if( dshl[i] != NULL ) { 
                len = strlen(dshl[i]);
                maxlen = len > maxlen ? len : maxlen;
            }
            else {
                (void)fprintf( stderr, "\nc_anotat:  Warning:  string is NULL\n" );
            }
        }
        if( maxlen ) {
            dshl2 = (char *)malloc(maxlen*ndsh*sizeof(char));
            if( dshl2 == NULL ) {
                printf( "\nc_anotat: Unable to create memory for array dshl2\n" );
                return;
            }
/*
 *  Need to pad out with blanks since Fortran routine expects it this way
 */
            Pad_char_array(dshl,dshl2,ndsh,maxlen);
        }
    }
/*
 * dshl may be NULL
 */
    else {
        dshl2 = NULL;
    }
    lenx = NGSTRLEN(labx);
    leny = NGSTRLEN(laby);
    labx2 = NGCstrToFstr(labx,lenx);
    laby2 = NGCstrToFstr(laby,leny);
    dshl3 = NGCstrToFstr(dshl2,maxlen);
    NGCALLF(anotat,ANOTAT)(labx2,laby2,&lbac,&lset,&ndsh,dshl3,lenx,leny,maxlen);
    if( dshl2 != NULL ) free((char *) dshl2);
}
