C
C	$Id: g01clp.f,v 1.2 1993-01-09 02:05:42 fred Exp $
C
      SUBROUTINE G01CLP
C
C  Process clipping parameters.
C
      include 'g01prm.h'
      include 'gksin.h'
      include 'g01wsl.h'
      include 'g01ins.h'
      include 'g01opc.h'
C
      INTEGER  NBYTES, I, ICRLOC(4)
      LOGICAL  CHANGE
C
C  If clipping indicator has changed, send it and store it in WSL.
C
      IF (ID(1).NE.MRCLIP)  THEN
        MRCLIP = ID(1)
C
C  Put out opcode (CLASS and ID) and LENGTH
C
        NBYTES = 1+(MEFW-1)/8
        CALL GPUTNI (CLCLIN, IDCLIN, NBYTES, RERR)
        IF (RERR.NE.0)  RETURN
C
C  Put out clipping indicator parameter (DATA, PRECIS, COUNT).
C
        CALL GPUTPR (MRCLIP, MEFW,     1, RERR)
        IF (RERR.NE.0)  RETURN
      END IF
C
C  Normalize clipping rectangle, send and store it if changed.
C
C  Normalize NDC limits of rectangle (assume bounds check
C  above WSI), store as rectangle corner points.
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
      IF (CHANGE) THEN
C
C  Total byte length, based on VDC bit precision.
C
        NBYTES = 1 + (4*MVDCFW-1)/8
C
C  Put out opcode (CLASS and ID) and LENGTH.
C
        CALL GPUTNI (CLCREC, IDCREC, NBYTES, RERR)
        IF (RERR.NE.0)  RETURN
C
C  Put out clipping rectangle corner points (DATA, PRECIS, COUNT).
C
        CALL GPUTPR (MRCREC, MVDCFW,     4, RERR)
      END IF
C
      RETURN
      END
