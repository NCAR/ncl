C
C	$Id: gsplci.f,v 1.2 1993-01-09 02:03:04 fred Exp $
C
      SUBROUTINE GSPLCI (COLI)
C
C  SET POLYLINE COLOUR INDEX
C
      INTEGER ESPLCI
      PARAMETER (ESPLCI=21)
C
      include 'gkscom.h'
C
      INTEGER COLI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,ESPLCI,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the index is valid.
C
      IF (COLI .LT. 0) THEN
        ERS = 1
        CALL GERHND(92,ESPLCI,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set the current color index in the GKS state list.
C
      CPLCI = COLI
C
C  Invoke the workstation interface.
C
      FCODE = 24
      CONT  = 0
      CALL GZROI(0)
      IC1   = 1
      IC2   = 1
      IC(1) = COLI
      CALL GZTOWK
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESPLCI,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
