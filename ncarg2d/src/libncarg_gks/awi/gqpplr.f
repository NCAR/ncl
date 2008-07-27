C
C	$Id: gqpplr.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPPLR(WTYPE,PLI,ERRIND,LNTYPE,LWIDTH,COLI)
C
C  INQUIRE PREDEFINED POLYLINE REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,PLI,ERRIND,LNTYPE,COLI
      REAL    LWIDTH
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
C  Check that index is valid.
C
      IF (PLI.LT.1) THEN
        ERRIND = 60
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -121
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = PLI
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      LNTYPE = ID(3)
      COLI   = ID(4)
      LWIDTH = RX(1)
      RETURN
C
  100 CONTINUE
      LNTYPE = -1
      COLI   = -1
      LWIDTH = -1.
      RETURN
      END
