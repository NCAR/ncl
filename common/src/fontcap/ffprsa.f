C
C	$Id: ffprsa.f,v 1.3 2000-08-22 03:53:13 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
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

      SUBROUTINE FFPRSA(IER)
C
C  Parse the ASCII fontcap.
C
      include 'fntcom.h'
C
      DATA IFRST/0/
      PARAMETER (NLEN=40)
      INTEGER ITMP(NLEN)
C
C  Retrieve the font name.
C
      IF (IFRST .EQ. 0) THEN
        IF (LINE(1:9) .NE. 'FONT_NAME') THEN
          IER = EORD
          IFRST = 1
          RETURN
        ELSE
          CALL CFRDLN(IDUM1,IDUM2,IER)
          DO 20 I=1,NUMFTS
            IF (LINE(1:NLEN) .EQ. FNTNMS(I)) THEN
              FNAME = FNTNMS(I)
              IFRST = 1
              GO TO 30
            ENDIF
   20     CONTINUE
          IER = EINM
          RETURN
        ENDIF
      ENDIF
   30 CONTINUE
C
C  Retrieve all keyword values except for the character outlines.
C
      INDX = 1
      DO 40 I=2,NUMKYS
        CALL CFRDLN(IDUM1,IDUM2,IER)
        IF (LINE(1:20) .NE. KEYLST(I)) THEN
          IER = EORD
          RETURN
        ELSE
          CALL CFRDLN(IDUM1,IDUM2,IER)
          CALL FFTKIN(LINE,NUM,ITMP,IER)
          DO 60 J=1,NUM
            INDX = INDX+1
            INTARR(INDX) = ITMP(J)
   60     CONTINUE
        ENDIF 
   40 CONTINUE
C
C  Fill in all of the values for the technical information part
C  of the fontcap and write them to the buffer.
C
      CALL FFINFO(IER)
      IF (IER .NE. 0) RETURN
C
C  Type flag.
C
      CALL SBYTES(BUFFER,TYPFLG,0,16,0,1)
C
C  Font name.
C
      DO 70 I=1,NLEN
        ITMP(I) = ICHAR(FNAME(I:I))
   70 CONTINUE
      CALL SBYTES(BUFFER,ITMP,16,8,0,NLEN)
C
C  Remaining information fields.
C
      CALL SBYTES(BUFFER,INTARR(2),336,16,0,NUMNTR-1)
C
C  Process the characters.
C
      CALL FFPRCF(IER)
C
      RETURN
      END
