C
C	$Id: gqfaf.f,v 1.2 1993-01-09 02:00:10 fred Exp $
C
      SUBROUTINE GQFAF(WTYPE,NI,NH,ERRIND,NIS,IS,NHS,HS,NPFAI)
C
C  INQUIRE FILL AREA FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,NI,NH,ERRIND,NIS,IS,NHS,HS,NPFAI
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
C  Check on bounds for NI and NH.
C
      IF (NI.LT.0 .OR. NI.GT.3 .OR. NH.LT.0) THEN
        ERRIND = 2000
        GO TO 100
      ENDIF
C
C  Check if the workstation category is OUTPUT or OUTIN.
C
      CALL GQWKCA(WTYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -112
      CONT  = 0
      CALL GZROI(0)
      IL1   = 3
      IL2   = 3
      ID(1) = WTYPE
      ID(2) = NI
      ID(3) = NH
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NIS   = ID(4)
      IS    = ID(5)
      NHS   = ID(6)
      HS    = ID(7)
      NPFAI = ID(8)
      RETURN
C
  100 CONTINUE
      NIS   = -1
      IS    = -1
      NHS   = -1
      HS    = -1
      NPFAI = -1
      RETURN
      END
