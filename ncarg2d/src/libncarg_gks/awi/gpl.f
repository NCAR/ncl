C
C	$Id: gpl.f,v 1.2 1993-01-09 01:59:13 fred Exp $
C
      SUBROUTINE GPL(N,PX,PY)
C
C  POLYLINE
C
      INTEGER EPL
      PARAMETER (EPL=12)
C
      include 'gkscom.h'
C
      INTEGER N
      REAL PX(N),PY(N)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,EPL,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the number of points is valid.
C
      IF (.NOT.(N.GE.2)) THEN
        ERS = 1
        CALL GERHND(100,EPL,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set function code and put out the real arrays across the
C  workstation interface.  Flag conversion to NDC space (no
C  conversion is necessary for transformation 0).
C
      FCODE = 11
      CALL GZROI(0)
      NPTOT = N
      CALL GZPUTR(NPTOT,N,PX,PY,MIN0(CNT,1),IER)
      RERR = IER
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EPL,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
