/*
 *	$Id: s_gopks.c,v 1.2 2000-07-12 17:06:09 haley Exp $
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
 *  Open GKS
 */

#include <ncarg/gks.h>

void gopen_gks
#ifdef NeedFuncProto
(
    const char   *err_file,   /* name of error file                */
    size_t        mem_unit    /* size_t units of memory available
                                 for buffer space                  */
)
#else
(err_file,mem_unit)
    char   *err_file;
    Gint   mem_unit;
#endif
{
    int unit;
/*
 *  If the err_file is "stderr" then pass unit 6 (which is  stdout in 
 *  Fortran) to "gopks".
 *
 *  Otherwise, get the file descriptor from the list and pass that to 
 *  "gopks".
 *
 */
    if( err_file ) {
        if( !strcmp( err_file, "stderr" ) || !strcmp( err_file, "stdout" ) ) {
           unit = 6;       
        }
        else {
            get_conn_id(&unit);
        }
    }
    else {
        get_conn_id(&unit);
	}
    NGCALLF(gopks,GOPKS)(&unit,&mem_unit);
}
