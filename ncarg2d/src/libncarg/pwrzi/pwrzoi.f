C
C	$Id: pwrzoi.f,v 1.2 2000-07-12 16:25:20 haley Exp $
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
      SUBROUTINE PWRZOI (JCHAR,INDEX,NSIZE)
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
