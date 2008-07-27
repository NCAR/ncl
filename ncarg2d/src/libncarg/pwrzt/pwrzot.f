C
C	$Id: pwrzot.f,v 1.4 2008-07-27 00:17:23 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWRZOT (JCHAR,INDEX,NSIZE)
C
C THIS ROUTINE SORTS JCHAR WHICH IS NSIZE IN LENGTH.  THE RELATIONSHIP
C BETWEEN JCHAR AND INDEX IS MAINTAINED.  A BUBBLE SORT IS USED.
C JCHAR IS SORTED IN ASCENDING ORDER.
C
      SAVE
      CHARACTER*1     JCHAR(NSIZE)     ,JTEMP     ,KTEMP
      DIMENSION       INDEX(NSIZE)
      LOGICAL         LDONE
C
      ISTART = 1
      ISTOP = NSIZE
      ISTEP = 1
C
C AT MOST NSIZE PASSES ARE NEEDED.
C
      DO 104 NPASS=1,NSIZE
       LDONE = .TRUE.
       I = ISTART
  101  ISUB = I+ISTEP
       IF (ISTEP*(ICHAR(JCHAR(I))-ICHAR(JCHAR(ISUB)))) 103,103,102
C
C THEY NEED TO BE SWITCHED.
C
  102  LDONE = .FALSE.
       JTEMP = JCHAR(I)
       KTEMP = JCHAR(ISUB)
       JCHAR(I) = KTEMP
       JCHAR(ISUB) = JTEMP
       ITEMP = INDEX(I)
       INDEX(I) = INDEX(ISUB)
       INDEX(ISUB) = ITEMP
C
C THEY DO NOT NEED TO BE SWITCHED.
C
  103  I = I+ISTEP
       IF (I .NE. ISTOP) GO TO 101
C
C IF NONE WERE SWITCHED DURING THIS PASS, WE CAN QUIT.
C
       IF (LDONE) RETURN
C
C SET UP FOR THE NEXT PASS IN THE OTHER DIRECTION.
C
       ISTEP = -ISTEP
       ITEMP = ISTART
       ISTART = ISTOP+ISTEP
       ISTOP = ITEMP
  104 CONTINUE
      RETURN
      END
