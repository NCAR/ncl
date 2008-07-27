C
C	$Id: shcomn.h,v 1.4 2008-07-27 04:02:37 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C  SHCOMI contains the values for all settable INTEGER parameters:
C
C    Var.    Name  Def.
C    ----    ----  ----------------------------------------------------------
C    NMLSTQ  NLS   Number of data points used in least squares fit.
C    NMINFL  NFL   Number of nodes within the radius of influence.
C    NMCELS  NCL   Number of rows, columns, planes in the cell grid.
C
      COMMON /SHCOMI/ NMLSTQ, NMINFL, NMCELS
