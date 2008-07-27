C
C	$Id: pwrzgs.f,v 1.4 2008-07-27 00:17:22 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWRZGS (KCHAR,JCHAR,INDEX,NSIZE,IPOINT)
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
C                  SRFACE
C
C JULY     1984    CONVERTED TO GKS AND FORTRAN 77
C
C MARCH    1990    FIXED HANDLING OF SET CALLS
C
C------------------------------------------------------------------
      END
