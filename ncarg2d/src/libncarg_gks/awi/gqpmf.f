C
C	$Id: gqpmf.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPMF(WTYPE,N,ERRIND,NMT,MT,NMS,NOMMS,
     +                 RMSMIN,RMSMAX,NPPMI)
C
C  INQUIRE POLYMARKER FACILITIES
C
      include 'gkscom.h'
C
      INTEGER WTYPE,N,ERRIND,NMT,MT,NMS,NPPMI
      REAL    NOMMS,RMSMIN,RMSMAX
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
c  Check that the workstation type is valid.
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
C  Check if index is non-negative.
C
      IF (N .LT. 0) THEN
        ERRIND = 2002
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -119
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = N
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NMT    = ID(3)
      MT     = ID(4)
      NMS    = ID(5)
      NPPMI  = ID(6)
      NOMMS  = RX(1)
      RMSMIN = RX(2)
      RMSMAX = RX(3)
      RETURN
C
  100 CONTINUE
      NMT    = -1
      MT     = -1
      NMS    = -1
      NPPMI  = -1
      NOMMS  = -1.
      RMSMIN = -1.
      RMSMAX = -1.
      RETURN
      END
