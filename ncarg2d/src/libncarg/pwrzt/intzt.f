C
C	$Id: intzt.f,v 1.3 2000-08-22 15:05:56 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE INTZT (XX,YY,ZZ,LIN3,ITOP)
C
C FORCE STORAGE OF X, Y, AND Z INTO COMMON BLOCK
C
      COMMON /PWRZ2T/ X, Y, Z
      DATA IDUMX,IDUMY,IDUMZ /0, 0, 0/
      X = XX
      Y = YY
      Z = ZZ
      CALL INITZT (IDUMX,IDUMY,IDUMZ,LIN3,ITOP,1)
      RETURN
      END
