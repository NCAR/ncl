/*
 *  $Id: s_gerhnd.c,v 1.5 2008-07-23 17:24:19 haley Exp $
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
 *  Error handling  
 */

#include <ncarg/gks.h>

extern void NGCALLF(gerhnd,GERHND)(Gint*,Gint*,int*);

void gerr_hand
#ifdef NeedFuncProto
(
    Gint err_num,        /* error number                               */
    Gint func_num,       /* number of function that detected the error */
    const char *err_f    /* name of error file                         */
)
#else
( err_num, func_num, err_f )
    Gint err_num;
    Gint func_num;
    char *err_f;
#endif
{
    int iunit;
/*
 * For now, always write error messages to FORTRAN unit 6
 */
    iunit = 6; 
    NGCALLF(gerhnd,GERHND)(&err_num,&func_num,&iunit);
}
