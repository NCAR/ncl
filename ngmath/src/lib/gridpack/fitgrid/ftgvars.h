/*
 * $Id: ftgvars.h,v 1.6 2008-07-27 04:02:36 haley Exp $
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
 *
 *    User      Internal  Variable
 *  Parameter   Variable    Type    Description
 *  ---------   --------  --------  -----------
 *     sig      ft_sigma   float    the value of the tension factor.
 *     sl1      ft_slp1    float    the slope of the curve at the first X 
 *                                  coordinate, for ftcurv (use depends on 
 *                                  the value of ft_islp).
 *     sln      ft_slpn    float    the slope of the curve at the last X 
 *                                  coordinate, for ftcurv (use depends on 
 *                                  the value of ft_islp).
 *     sf1      ft_islp    int      controls the usage of ft_slp1 and 
 *                                  ft_slp2 as per:
 *                                    = 0 use both ft_slp1 and ft_slp2
 *                                    = 1 use ft_slp1 but not ft_slp2
 *                                    = 2 do not use ft_slp1 but use ft_slp2
 *                                    = 3 use neither ft_slp1 nor ft_slp2 
 *                                        (compute them internally).
 *     sf2      ft_sms     int      controls the usage of the smoothing 
 *                                  parameters ft_s and ft_eps as per:
 *                                    = 0 default both ft_s and ft_eps
 *                                    = 1 use both ft_s and ft_eps
 *                                    = 2 use ft_s but default ft_eps
 *                                    = 3 default ft_s but use ft_eps
 *                                    I1 = 1 if ft_zmn defaulted (else = 0)
 *     smt     ft_s        float    smoothing parameter for ftcurvs.
 *     eps     ft_eps      float    smoothing parameter for ftcurvs.
 *     dum     ft_cdum     char     dummy character variable for future use.
 *     zx1     ft_zx1      FTdata   array of X partials for surf.
 *     zxm     ft_zxm      FTdata   array of X partials for surf.
 *     zy1     ft_zy1      FTdata   array of Y partials for surf.
 *     zyn     ft_zyn      FTdata   array of Y partials for surf.
 *     z11     ft_z11      float    XY partial at coordinate (X(1),Y(1)).
 *     zm1     ft_zm1      float    XY partial at coordinate (X(M),Y(1)).
 *     z1n     ft_z1n      float    XY partial at coordinate (X(1),Y(N)).
 *     zmn     ft_zmn      float    XY partial at coordinate (X(M),Y(N)).
 *      -      ft_err      int      internal error status flag.
 *     df1     ft_df1      int      flags if zx1 is user supplied (0 if so).
 *     df2     ft_df2      int      flags if zxm is user supplied (0 if so).
 *     df3     ft_df3      int      flags if zy1 is user supplied (0 if so).
 *     df4     ft_df4      int      flags if zyn is user supplied (0 if so).
 *     df5     ft_df5      int      flags if z11 is user supplied (0 if so).
 *     df6     ft_df6      int      flags if zm1 is user supplied (0 if so).
 *     df7     ft_df7      int      flags if z1n is user supplied (0 if so).
 *     df8     ft_df8      int      flags if zmn is user supplied (0 if so).
 */

double    ft_sigma = 1., ft_slp1 = 0., ft_slpn = 0., ft_s, ft_eps,
          ft_z11 = 0., ft_zm1 = 0., ft_z1n = 0., ft_zmn = 0.;

int       ft_islp = 3, ft_sms = 0, ft_df1 = 1, ft_df2 = 1, ft_df3 = 1,
          ft_df4 = 1, ft_df5 = 1, ft_df6 = 1, ft_df7 = 1, ft_df8 = 1,
          ft_err = 0;

char      ft_cdum[13] = {"dummy_string"};

FTdata    ft_zx1, ft_zxm, ft_zy1, ft_zyn;
