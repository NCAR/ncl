/* 
 * $Id: fttypes.h,v 1.6 2008-07-27 04:02:36 haley Exp $
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

typedef enum FtDType {
  ft_notype = 0,
  ft_float  = 1, 
  ft_double = 2
} FtDataTypes;

typedef struct {
      size_t      size;
      void        *data;
      FtDataTypes type;
} FTdata;

