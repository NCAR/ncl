C
C	$Id: pwrzgi.f,v 1.2 2000-07-12 16:25:20 haley Exp $
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
      SUBROUTINE PWRZGI (KCHAR,JCHAR,INDEX,NSIZE,IPOINT)
C
C THIS ROUTINE FINDS WHERE KCHAR IS IN JCHAR AND RETURNS THE CORRES-
C PONDING INDEX IN IPOINT.  BINARY HALVING IS USED.
C
      SAVE
      CHARACTER*1     JCHAR(NSIZE)     ,KCHAR
      DIMENSION       INDEX(NSIZE)
C
C IT IS ASSUMED THAT JCHAR IS LESS THAT 2**9 IN LENGTH, SO IF KCHAR IS
C NOT FOUND IN 10 STEPS, THE SEARCH IS STOPPED.
C
      KOUNT = 0
      IBOT = 1
      ITOP = NSIZE
      I = ITOP
      GO TO 102
  101 I = (IBOT+ITOP)/2
      KOUNT = KOUNT+1
      IF (KOUNT .GT. 10) GO TO 106
  102 IF (ICHAR(JCHAR(I))-ICHAR(KCHAR)) 103,105,104
  103 IBOT = I
      GO TO 101
  104 ITOP = I
      GO TO 101
  105 IPOINT = INDEX(I)
      RETURN
C
C IPOINT=-1 MEANS THAT KCHAR WAS NOT IN THE TABLE.
C
  106 IPOINT = -1
      RETURN
C
C
C
C REVISION HISTORY----------
C
C MARCH    1980    FIRST ADDED TO ULIB AS A SEPARATE FILE TO BE
C                  USED IN CONJUNCTION WITH THE ULIB ROUTINE
C                  ISOSRF
C
C JULY     1984    CONVERTED TO GKS AND FORTRAN 77
C------------------------------------------------------------------
      END
