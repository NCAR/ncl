C
C	$Id: gstxal.f,v 1.2 1993-01-09 02:03:18 fred Exp $
C
      SUBROUTINE GSTXAL (TXALH,TXALV)
C
C  SET TEXT ALIGNMENT
C
      INTEGER ESTXAL
      PARAMETER (ESTXAL=34)
C
      include 'gkscom.h'
C
      INTEGER TXALH,TXALV
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESTXAL,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the arguments are valid.
C
      IF (TXALH.LT.0 .OR. TXALH.GT.3 .OR. TXALV.LT.0 .OR.
     +    TXALV.GT.5) THEN
        ERS = 1
        CALL GERHND(2000,ESTXAL,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current text alignment variables in the GKS state list.
C
      CTXAL(1) = TXALH
      CTXAL(2) = TXALV
C
C  Invoke the workstation interface.
C
      FCODE = 36
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = TXALH
      ID(2) = TXALV
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESTXAL,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
