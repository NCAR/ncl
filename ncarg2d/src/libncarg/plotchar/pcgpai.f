C
C $Id: pcgpai.f,v 1.5 2000-07-12 16:24:57 haley Exp $
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
