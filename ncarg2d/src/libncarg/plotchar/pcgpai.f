C
C $Id: pcgpai.f,v 1.7 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCGPAI (CHRS,IBEG,IVAL)
C
C This routine looks for a subscript of the form "(n)" in positions IBEG
C and following of the character string CHRS.  It returns the value of
C "n" (which may be negative) in IVAL.  If no subscript is found, IVAL
C is zeroed.
C
        CHARACTER*(*) CHRS
        CHARACTER*1 ICHR
C
        IVAL=0
        ISGN=1
C
        ISTA=0
C
        DO 101 ICHS=IBEG,LEN(CHRS)
          ICHR=CHRS(ICHS:ICHS)
          IF (ICHR.NE.' ') THEN
            IF (ISTA.EQ.0) THEN
              IF (ICHR.NE.'(') GO TO 102
              ISTA=1
            ELSE IF (ISTA.EQ.1) THEN
              IF (ICHAR(CHRS(ICHS:ICHS)).GE.ICHAR('0').AND.
     +            ICHAR(CHRS(ICHS:ICHS)).LE.ICHAR('9')) THEN
                IVAL=ICHAR(CHRS(ICHS:ICHS))-ICHAR('0')
              ELSE IF (ICHR.EQ.'-') THEN
                ISGN=-1
              ELSE IF (ICHR.NE.'+') THEN
                GO TO 102
              END IF
              ISTA=2
            ELSE IF (ISTA.EQ.2) THEN
              IF (ICHAR(CHRS(ICHS:ICHS)).GE.ICHAR('0').AND.
     +            ICHAR(CHRS(ICHS:ICHS)).LE.ICHAR('9')) THEN
                IVAL=IVAL*10+ICHAR(CHRS(ICHS:ICHS))-ICHAR('0')
              ELSE IF (ICHR.NE.')') THEN
                GO TO 102
              ELSE
                GO TO 103
              END IF
            END IF
          END IF
  101   CONTINUE
C
  102   IVAL=0
        RETURN
C
  103   IVAL=ISGN*IVAL
        RETURN
C
      END
