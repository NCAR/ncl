/*
 * $Id: ftuvars.h,v 1.4 2003-08-11 22:44:01 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <stdio.h>
#include <ncarg/gks.h>

#include "fttypes.h"

/*
 *  Define all global variables (they all begin with the sentinel
 *  characters "ft_").
 */
extern double    ft_sigma, ft_slp1, ft_slpn, ft_s, ft_eps,
                 ft_z11, ft_zm1, ft_z1n, ft_zmn;

extern int       ft_islp, ft_sms, ft_err,
                 ft_df1, ft_df2, ft_df3, ft_df4, 
                 ft_df5, ft_df6, ft_df7, ft_df8;

extern char      ft_cdum[];

extern FTdata    ft_zx1, ft_zxm, ft_zy1, ft_zyn;
