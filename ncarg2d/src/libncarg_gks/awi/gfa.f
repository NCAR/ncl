C
C	$Id: gfa.f,v 1.2 1993-01-09 01:58:36 fred Exp $
C
      SUBROUTINE GFA(N,PX,PY)
C
C  FILL AREA
C
      INTEGER EFA
      PARAMETER (EFA=15)
C
      include 'gkscom.h'
C
      REAL PX(N),PY(N)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,EFA,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the number of points is valid.
C
      IF (.NOT.(N.GE.3)) THEN
        ERS = 1
        CALL GERHND(100,EFA,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set function code and put out the real arrays across the
C  workstation interface.  Flag conversion to NDC space.
C
      FCODE = 14
      CALL GZROI(0)
      NPTOT = N
      CALL GZPUTR(NPTOT,N,PX,PY,1,IER)
      RERR = IER
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EFA,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
