C
C	$Id: ffphol.f,v 1.4 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
