C
C	$Id: gwiclp.f,v 1.1 1993-01-09 02:09:07 fred Exp $
C
        SUBROUTINE GWICLP
C
C  Process clipping parameters.
C
      include 'gksin.h'
      include 'gwiwsl.h'
      include 'gwiins.h'
      include 'gwiopc.h'
C
      INTEGER  NBYTES, I, ICRLOC(4)
      LOGICAL  CHANGE
C
      SAVE
C
C  If clipping indicator has changed, send it and store it in WSL.
C
      IF (ID(1).NE.MRCLIP)  THEN
        MRCLIP = ID(1)
C
C  Put out opcode (class and id) and length.
C
        NBYTES = 1+(MEFW-1)/8
        CALL GWPTNI (CLCLIN, IDCLIN, NBYTES, RERR)
        IF (RERR.NE.0)  RETURN
C
C  Put out clipping indicator parameter.
C
        CALL GWPTPR (MRCLIP, MEFW,     1, RERR)
        IF (RERR.NE.0)  RETURN
      END IF
C
C  Normalize clipping rectangle, send and store it if changed.
C
C  Normalize NDC limits of rectangle (assume bounds check above WSI), 
C  store as rectangle corner points.
C
      ICRLOC(1) = MXOFF + MXSCAL*RX(1)
      ICRLOC(3) = MXOFF + MXSCAL*RX(2)
      ICRLOC(2) = MYOFF + MYSCAL*RY(1)
      ICRLOC(4) = MYOFF + MYSCAL*RY(2)
      CHANGE = .FALSE.
      DO 10 I=1,4
        IF (ICRLOC(I).NE.MRCREC(I))  THEN
          CHANGE = .TRUE.
          MRCREC(I) = ICRLOC(I)
        END IF
   10 CONTINUE
      IF (CHANGE)  THEN
C
C  Total byte length, based on VDC bit precision.
C
        NBYTES = 1 + (4*MVDCFW-1)/8
C
C  Put out opcode (class and id) and length.
C
        CALL GWPTNI (CLCREC, IDCREC, NBYTES, RERR)
        IF (RERR.NE.0)  RETURN
C
C  Put out clipping rectangle corner points.
C
        CALL GWPTPR (MRCREC, MVDCFW,     4, RERR)
      END IF
C
      RETURN
C
      END
