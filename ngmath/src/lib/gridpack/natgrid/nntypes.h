/*
 * $Id: nntypes.h,v 1.4 2008-07-27 04:02:37 haley Exp $
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

struct datum
{  double       values[3];
   struct datum *nextdat;
};

struct simp
{  int          vert[3];
   double       cent[3];
   struct simp  *nextsimp;
};

struct temp
{  int          end[2];
   struct temp  *nexttemp;
};

struct neig
{  int          neinum;
   double       narea;
   double       coord;
   struct neig  *nextneig;
};

struct asinfo
{  int          crows;
   int          ccols;
   float        **aspect_out;
   float        **slope_out;
};
