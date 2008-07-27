C
C	$Id: ffprcf.f,v 1.4 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE FFPRCF(IER)
C
C  Process the character entries in the ASCII fontcap.
C
      include 'fntcom.h'
C
      INTEGER BYTPNT,STATE,IVALS(6)
C
C  Initialize the byte and bit pointers (start after the table of indices
C  and the pointer to the last byte).
C
      NUMCHR = CHREND-CHRSTR+1
      BYTPNT = TABPNT+NBYPWD*((2*NUMCHR-1)/NBYPWD)+NBYPWD
      BITPNT = 8*BYTPNT
      STATE  = 0
C
      DO 10 I=1,NUMCHR
   20   CONTINUE
        CALL CFRDLN(INUM,IWID,IER)
        IF (IER .NE. 0) GO TO 30
        IF (LINE(1:10) .EQ. 'BEGIN_CHAR') THEN
          IF (STATE .EQ. 0) THEN
C
C  Initialize --
C    Flag the state and put the byte address for this charater in 
C    the index array.  Put out the character number and width
C    packet.
C           
            STATE = 1
C
C  Round the bit pointer to the nearest byte.
C
            M8 = MOD(BITPNT,8)
            IF (M8 .NE. 0) BITPNT = BITPNT+8-M8
C
C  Round the byte pointer to the nearest word and adjust the bit pointer.
C
            BYTPNT = BITPNT/8
            BYTPNT = NBYPWD*((BYTPNT-1)/NBYPWD)+NBYPWD
            BITPNT = 8*BYTPNT
C
C  Byte address to index table.
C
            CALL SBYTES(BUFFER,BYTPNT,8*(TABPNT+2*(I-1)),16,0,1) 
C
C  Number/width packet.
C
            CALL FFPPKT(BITPNT,BEGINC,INUM,IWID)
            BITPNT = BITPNT+PKWID
            GO TO 20
          ELSE
            IER = EORD
            GO TO 30
          ENDIF
        ELSE
          IF (STATE .EQ. 1) THEN
            IF (LINE(1:12).EQ.'BEGIN_REGION' .OR. 
     +          LINE(1:10).EQ.'BEGIN_LINE') THEN
   40         CONTINUE
              CALL CFRDLN(IDUM1,IDUM2,IER)
              IF (LINE(1:3).NE.'BEG' .AND. LINE(1:3).NE.'END') THEN
C
C  Has to be coordinate data or Bezier control points to get here.
C
                CALL FFTKIN(LINE,NUMI,IVALS,IER)
                IF (IER .NE. 0) GO TO 30
                IF (NUMI .EQ. 2) THEN
C
C  Coordinate data, add biases before writing.
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
   60             CONTINUE
                  GO TO 40
                ELSE
                  IER = EORD
                  GO TO 30
                ENDIF
              ELSE IF (LINE(1:10) .EQ. 'BEGIN_HOLE') THEN
C
C  Process hole.
C
                CALL FFPHOL(IER)
                IF (IER .NE. 0) THEN
                  RETURN
                ELSE
                  GO TO 40
                ENDIF
              ELSE IF (LINE(1:10) .EQ. 'END_REGION') THEN
                CALL FFPPKT(BITPNT,ENDR,0,0)
                BITPNT = BITPNT+PKWID
                GO TO 20
              ELSE IF (LINE(1:8) .EQ. 'END_LINE') THEN
                CALL FFPPKT(BITPNT,ENDL,0,0)
                BITPNT = BITPNT+PKWID
                GO TO 20
              ELSE
                IER = EORD
                GO TO 30
              ENDIF 
            ELSE
              IF (LINE(1:8) .EQ. 'END_CHAR') THEN
                STATE = 0
                CALL FFPPKT(BITPNT,ENDC,0,0)
                BITPNT = BITPNT+PKWID
                GO TO 10
              ELSE
                IER = EORD
                GO TO 30
              ENDIF
            ENDIF
          ELSE
            IER = EORD
            GO TO 30
          ENDIF
        ENDIF
   10 CONTINUE
C
C  Put out the pointer to the last byte.
C
      M8 = MOD(BITPNT,8)
      IF (M8 .NE. 0) BITPNT = BITPNT+8-M8
      LSTPNT = BITPNT/8
C
C  Get the number of words to write out.
C
      NWRDS = (NBYPWD*((LSTPNT-1)/NBYPWD)+NBYPWD)/NBYPWD
C
C  The offset is computed using two bytes for each of the internals
C  in the internals array INTARR and 40 bytes for the font name.
C
      CALL SBYTES(BUFFER,LSTPNT,8*(2*NUMNTR+40),16,0,1)
   30 CONTINUE
C
      RETURN
      END
