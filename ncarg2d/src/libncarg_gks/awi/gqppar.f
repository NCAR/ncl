C
C	$Id: gqppar.f,v 1.2 1993-01-09 02:01:18 fred Exp $
C
      SUBROUTINE GQPPAR(WTYPE,PPAI,NMX,MMX,ERRIND,N,M,PARRAY)
C
C  INQUIRE PREDEFINED PATTERN REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,PPAI,NMX,MMX,ERRIND,N,M,PARRAY(NMX,MMX)
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
C  Check if index is positive.
C
      IF (PPAI.LT.1) THEN
        ERRIND = 85
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -120
      CONT  = 0
      CALL GZROI(0)
      IL1   = 4
      IL2   = 4
      ID(1) = WTYPE
      ID(2) = PPAI
      ID(3) = NMX
      ID(4) = MMX
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR.NE.0) THEN
      ERRIND = RERR
      GOTO 100
      ENDIF
      N = ID(5)
      M = ID(6)
C
C  Bring over the pattern array.
C
      INDX = (N*M-1)/128
      IF (INDX.EQ.0) THEN
        CALL GZFMWK
        INDX = 0
        DO 200 J=1,M
          DO 201 I=1,N
            INDX = INDX+1
            PARRAY(I,J) = ID(INDX)
  201     CONTINUE
  200   CONTINUE
      ELSE
        CALL GZFMWK
        INDX = 0
        DO 202 J=1,M
          DO 203 I=1,N
            INDX = INDX+1
            PARRAY(I,J) = ID(INDX)
            JMD = MOD(INDX,128)
            IF (JMD.EQ.0.AND.CONT.EQ.1) THEN
             CALL GZFMWK
             INDX = 0
            ENDIF
  203     CONTINUE
  202   CONTINUE
      ENDIF
      RETURN
C
  100 CONTINUE
      N = -1
      M = -1
      PARRAY(1,1) = -1
C
      RETURN
      END
