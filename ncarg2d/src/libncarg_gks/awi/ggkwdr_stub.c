#include <ncarg/c.h>
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
 *	ggkwdr_()
 *
 *	This function is a stub for the ggkwdr_() interface used by the 
 *	NCAR GKS package. The stub sets the status parameter to error 
 *	condition 23 ("Specified workstation type does not exist") for
 *	all invocations. 
 */
void
NGCALLF(ggkwdr,GGKWDR)(wk_id, gks_opcode, continuation,
        total_i, num_i_sent, ints,
        total_x, num_x_sent, indexes,
        total_f, num_f_sent, fxs, fys,
        total_c, num_c_sent, chars,
        status, err_msg)

        unsigned int    *wk_id;
        int     *gks_opcode, *continuation;

        int     *total_i, *num_i_sent, *ints;

        int     *total_x, *num_x_sent, *indexes;

        int     *total_f, *num_f_sent;
        float   *fxs, *fys;

        int     *total_c, *chars, *num_c_sent;

        int     *status;
        char    *err_msg;
{

	*status = 23;
}
