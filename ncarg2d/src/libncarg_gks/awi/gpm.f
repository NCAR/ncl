C
C	$Id: gpm.f,v 1.2 1993-01-09 01:59:16 fred Exp $
C
      SUBROUTINE GPM(N,PX,PY)
C
C  POLYMARKER
C
      INTEGER EPM
      PARAMETER (EPM=13)
C
      include 'gkscom.h'
C
      INTEGER N
      REAL PX(N),PY(N)
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(5,EPM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check that the number of points is valid.
C
      IF (.NOT.(N.GE.1)) THEN
        ERS = 1
        CALL GERHND(100,EPM,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set function code and put out the real arrays across the
C  workstation interface.  Flag conversion to NDC space.
C
      FCODE = 12
      CALL GZROI(0)
      NPTOT = N
      CALL GZPUTR(NPTOT,N,PX,PY,1,IER)
      RERR = IER
      IF (RERR.NE.0) THEN
        ERS = 1
        CALL GERHND(RERR,EPM,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
