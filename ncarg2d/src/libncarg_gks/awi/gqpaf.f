C
C	$Id: gqpaf.f,v 1.2 1993-01-09 02:00:54 fred Exp $
C
      SUBROUTINE GQPAF(WTYPE,ERRIND,NPPAI)
C
C  INQUIRE PATTERN FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,NPPAI
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
C
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
      IF (ERRIND .NE. 0) GO TO 100
      FCODE = -115
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = WTYPE
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NPPAI = ID(2)
      RETURN
C
  100 CONTINUE
      NPPAI = -1
      RETURN
      END
