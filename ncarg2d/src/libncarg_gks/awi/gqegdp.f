C
C	$Id: gqegdp.f,v 1.2 1993-01-09 02:00:00 fred Exp $
C
      SUBROUTINE GQEGDP(WTYPE,N,ERRIND,NGDP,GDPL)
C
C  INQUIRE LIST element OF AVAILABLE GENERALIZED DRAWING PRIMITIVES.
C
      include 'gkscom.h'
C
      INTEGER WTYPE,N,ERRIND,NGDP,GDPL
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Check that requested element number is non-negative.
C
      IF (N .LT. 0) THEN
        ERRIND = 2002
        GO TO 100
      ENDIF
C
C  Currently no supported workstation type has any GDPs.
C
      NGDP = 0
      GDPL = 0
      RETURN
C
  100 CONTINUE
      NGDP = -1
      GDPL = -1
      RETURN
      END
