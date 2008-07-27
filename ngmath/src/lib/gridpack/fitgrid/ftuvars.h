/*
 * $Id: ftuvars.h,v 1.5 2008-07-27 04:02:36 haley Exp $
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
