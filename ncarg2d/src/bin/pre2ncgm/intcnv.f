C
C	$Id: intcnv.f,v 1.3 2000-08-22 04:34:21 haley Exp $
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
      SUBROUTINE INTCNV(CHRSTR,INTVAL,IOS,STATUS)
C
C  DECODE A CHARACTER ARRAY STRING INTO AN INTEGER ARRAY
C
C  INPUT
C       CHRSTR-CHARACTER ARRAY STRING CONTAINING CHARACTERS TO BE CONVERTED
C               INTO INTEGER ARRAY 80
C  OUTPUT
C       INTVAL-INTEGER VALUE GENERATED
C       IOS-THE I/O STATUS OF THE REQUEST ONLY VALID IF STATUS NOT ZERO
C       STATUS-THE ERROR STATUS
C               ZERO ALL OK
C               NOT ZERO THEN ERROR
C
      INTEGER INTVAL,IOS,STATUS,II,ISTRT,KK
      CHARACTER*1 CHRSTR(80)
      CHARACTER*1 NUMS(10)
C
      DATA NUMS/'0','1','2','3','4','5','6','7','8','9'/
C
      INTVAL = 0
C
C  DECODE THE STRING
C
C  FIRST SKIP LEADING BLANKS
C
      DO 10 II = 1,80
        ISTRT = II
        IF (CHRSTR(II) .NE. ' ') GO TO 20
 10   CONTINUE
C
C   NOTHING IN THE STRING SO RETURN
C
      RETURN
C
 20   CONTINUE
C
C  NON BLANK SO START DECODING
C
      DO 30 II = ISTRT,80
        DO 25 KK = 1,10
                IF ( CHRSTR(II) .EQ. NUMS(KK)) THEN
C
C                       INTEGER FOUND SO MODIFY THE INTVAL
C
                        INTVAL = INTVAL * 10 + (KK-1)
                        GO TO 30
                END IF
 25     CONTINUE
C
C       IF YOU REACH HERE THERE WAS A NON INTEGER CHARACTER SO RETURN
C
        RETURN
 30   CONTINUE
C
      RETURN
      END
