C
C	$Id: shcomn.h,v 1.2 2000-07-13 02:49:29 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
