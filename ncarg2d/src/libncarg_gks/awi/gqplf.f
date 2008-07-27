C
C	$Id: gqplf.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPLF(WTYPE,N,ERRIND,NLT,LT,NLW,NOMLW,
     +                 RLWMIN,RLWMAX,NPPLI)
C
C  INQUIRE POLYLINE FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,N,ERRIND,NLT,LT,NLW,NPPLI
      REAL    NOMLW,RLWMIN,RLWMAX
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
C  Check if index non-negative.
C
      IF (N .LT. 0) THEN
        ERRIND = 2002
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -118
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = N
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NLT    = ID(3)
      LT     = ID(4)
      NLW    = ID(5)
      NPPLI  = ID(6)
      NOMLW  = RX(1)
      RLWMIN = RX(2)
      RLWMAX = RX(3)
      RETURN
C
  100 CONTINUE
      NLT    = -1
      LT     = -1
      NLW    = -1
      NPPLI  = -1
      NOMLW  = -1.
      RLWMIN = -1.
      RLWMAX = -1.
      RETURN
      END
