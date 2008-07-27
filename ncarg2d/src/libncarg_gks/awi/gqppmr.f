C
C	$Id: gqppmr.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPPMR(WTYPE,PMI,ERRIND,MKTYPE,MKSSCF,COLI)
C
C  INQUIRE PREDEFINED POLYMARKER REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,PMI,ERRIND,MKTYPE,COLI
      REAL    MKSSCF
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
      IF (PMI .LE. 0) THEN
        ERRIND = 66
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -122
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = PMI
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      MKTYPE = ID(3)
      COLI   = ID(4)
      MKSSCF = RX(1)
      RETURN
C
  100 CONTINUE
      MKTYPE = -1
      COLI   = -1
      MKSSCF = -1.
      RETURN
      END
