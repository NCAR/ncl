C
C $Id: slgpai.f,v 1.3 2000-08-22 15:06:33 haley Exp $
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
      SUBROUTINE SLGPAI (CHRS,IBEG,IVAL)
C
C This routine looks for a subscript of the form "(n)" in positions IBEG
C and following of the character string CHRS.  It returns the value of
C "n" (which may be negative) in IVAL.  If no subscript is found, IVAL
C is zeroed.
C
        CHARACTER*(*) CHRS
C
C Define a temporary variable to hold one character at a time from the
C string CHRS.
C
        CHARACTER*1 ICHR
C
C Initially, the value is zero, with "sign" "+1".
C
        IVAL=0
        ISGN=1
C
C ISTA is a "state" variable.  ISTA = 0 implies that we're looking for
C the initial "(", ISTA = 1 that we're looking for a sign or for the
C first digit of the subscript, ISTA = 2 that we're looking for the
C next digit of the subscript or for the terminating ")".
C
        ISTA=0
C
C Loop through the characters of the string.
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
C No subscript found.  Return a zero value.
C
  102   IVAL=0
        RETURN
C
C Subscript found.  Return a signed value.
C
  103   IVAL=ISGN*IVAL
        RETURN
C
      END
