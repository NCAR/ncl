C
C	$Id: ffphol.f,v 1.3 2000-08-22 03:53:12 haley Exp $
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

      SUBROUTINE FFPHOL(IER)
C
C  Process holes.
C
      include 'fntcom.h'
C
      DIMENSION IVALS(10)
C
C  Put out the begin hole flag.
C
      CALL FFPPKT(BITPNT,BEGINH,0,0)
      BITPNT = BITPNT+PKWID
   40 CONTINUE
      CALL CFRDLN(IDUM1,IDUM2,IER)
      IF (LINE(1:3) .NE. 'END') THEN
C
C  Has to be coordinate data or Bezier control points to get here.
C
        CALL FFTKIN(LINE,NUMI,IVALS,IER)
        IF (IER .NE. 0) GO TO 30
        IF (NUMI .EQ. 2) THEN
C
C  Coordinate data.
C
          IVALS(1) = IVALS(1)+XBIAS
          IVALS(2) = IVALS(2)+YBIAS
          CALL FFPPKT(BITPNT,COORD,IVALS(1),IVALS(2))
          BITPNT = BITPNT+PKWID
          GO TO 40
        ELSE IF (NUMI .EQ. 6) THEN
C
C  Bezier control points.
C

          IVALS(1) = IVALS(1)+XBIAS
          IVALS(2) = IVALS(2)+YBIAS
          CALL FFPPKT(BITPNT,BEZIER,IVALS(1),IVALS(2))
          BITPNT = BITPNT+PKWID
          DO 60 J=2,3
            IX = 2*J-1
            IY = IX+1
            IVALS(IX) = IVALS(IX)+XBIAS
            IVALS(IY) = IVALS(IY)+YBIAS
            CALL FFPPKT(BITPNT,COORD,IVALS(IX),IVALS(IY))
            BITPNT = BITPNT+PKWID
   60     CONTINUE
          GO TO 40
        ELSE
          IER = EORD
          GO TO 30
        ENDIF
      ELSE IF (LINE(1:10) .EQ. 'END_HOLE') THEN
C
C  Put out end hole flag.
C
        CALL FFPPKT(BITPNT,ENDH,0,0)
        BITPNT = BITPNT+PKWID
        RETURN
      ELSE
        IER = EORD
        GO TO 30
      ENDIF
C
   30 CONTINUE
C
      RETURN
      END
