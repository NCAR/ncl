C
C	$Id: pcffme.f,v 1.3 2000-07-12 16:24:56 haley Exp $
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
      SUBROUTINE PCFFME (CHGT)
C
C Extract the informational part of the fontcap.
C
      include 'pcffme.h'
      include 'pcffdx.h'
C
C  Type flag.
C
      CALL GBYTES(IBFC,TYPFLG,0,16,0,1)
C
C  Other fields.
C
      CALL GBYTES(IBFC,FNINFO(2),336,16,0,NUMNTR-1)
C
C  Generate negative numbers if required.
C
      DO 80 I=2,NUMNTR
        CALL PCGNEG (FNINFO(I),ITMP)
        FNINFO(I) = ITMP
   80 CONTINUE
C
C  Get the byte pointers for the characters.
C
      NUMCHR = CHREND-CHRSTR+1
      CALL GBYTES(IBFC,CHRPNT,8*TABPNT,16,0,NUMCHR)
C
      RETURN
      END
