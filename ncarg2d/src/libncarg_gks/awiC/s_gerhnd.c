/*
 *  $Id: s_gerhnd.c,v 1.2 2000-07-12 17:06:07 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *  Error handling  
 */

#include <ncarg/gks.h>

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
