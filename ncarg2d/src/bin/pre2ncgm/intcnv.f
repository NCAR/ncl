C
C	$Id: intcnv.f,v 1.4 2008-07-27 00:59:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
