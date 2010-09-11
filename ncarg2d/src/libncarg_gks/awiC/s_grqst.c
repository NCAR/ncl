/*
 *	$Id: s_grqst.c,v 1.5 2008-07-23 17:24:23 haley Exp $
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

/*
 *  Request string
 */

#include <ncarg/gks.h>
#include <string.h>

extern void NGCALLF(grqst,GRQST)(Gint*,Gint*,Gin_status*,int*,NGstring,int);

void greq_string
#ifdef NeedFuncProto
(
    Gint       ws_id,      /* workstation identifier */
    Gint       string_num, /* string device number   */
    Gin_status *in_status, /* OUT [input] status     */
    char       *string     /* OUT requested string   */
)
#else
( ws_id, string_num, in_status, string )
    Gint       ws_id;
    Gint       string_num;
    Gin_status *in_status;
    char       *string;
#endif
{
    int idum = 0, len;
    NGstring str2;
    len = NGSTRLEN(string);
    str2 = NGCstrToFstr(string,len);
    NGCALLF(grqst,GRQST)(&ws_id,&string_num,in_status,&idum,str2,len);
}
