C
C	$Id: gqwkcl.f,v 1.2 1993-01-09 02:02:00 fred Exp $
C
      SUBROUTINE GQWKCL(WTYPE,ERRIND,VRTYPE)
C
C  INQUIRE WORKSTATION CLASSIFICATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,VRTYPE
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
C  Invoke interface.
C
      FCODE = -128
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = WTYPE
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      VRTYPE = ID(2)
      RETURN
C
  100 CONTINUE
      VRTYPE = -1
      RETURN
      END
