C
C	$Id: gsfais.f,v 1.2 1993-01-09 02:02:43 fred Exp $
C
      SUBROUTINE GSFAIS (INTS)
C
C  SET FILL AREA INTERIOR STYLE
C
      INTEGER ESFAIS
      PARAMETER (ESFAIS=36)
C
      include 'gkscom.h'
C
      INTEGER INTS
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESFAIS,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that INTS is in range.
C
      IF (INTS.LT.0 .OR. INTS.GT.3) THEN
        ERS = 1
        CALL GERHND(2000,ESFAIS,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current index in the GKS state list.
C
      CFAIS = INTS
C
C  Invoke the workstation interface.
C
      FCODE = 38
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = INTS
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESFAIS,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
