C
C	$Id: gpl.f,v 1.3 1994-06-17 22:05:00 fred Exp $
C
      SUBROUTINE GPL(N,PX,PY)
C
C  POLYLINE
C
      INTEGER EPL
      PARAMETER (EPL=12)
C
C  Specify the maximum sized polygon that will be clipped to the NDC
C  viewport.
C
      PARAMETER (MAXCLP=250)
C
      include 'gkscom.h'
C
      INTEGER N
      REAL PX(N),PY(N)
C
C  Set up the real workspace for clipping call.
C
      DIMENSION RWKSP(IWDIM)
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
C
C  Clip the polyline to the NDC viewport if the polylines are small 
C  enough and clipping is on.
C
      IF (N .LE. MAXCLP .AND. GKSCLP.NE.0) THEN
        ICNT = CNT+1
        CALL GZCLLI (WRLDCP(ICNT,1),WRLDCP(ICNT,2),WRLDCP(ICNT,3),
     +               WRLDCP(ICNT,4),PX,PY,N,RWKSP,IWDIM,IERR)
C
C  If error, abandon any clipping and just put the line out.
C
        IF (IERR .NE. 0) GO TO 10
        RERR = 0
        RETURN
      ENDIF
C
   10 CONTINUE
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
