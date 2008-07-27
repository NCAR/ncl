C
C	$Id: gqpfar.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPFAR(WTYPE,PFAI,ERRIND,STYLE,STYLID,COLI)
C
C  INQUIRE PREDEFINED FILL AREA REPRESENTATION
C
      include 'gkscom.h'
C
      INTEGER WTYPE,PFAI,ERRIND,STYLE,STYLID,COLI
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
      IF (PFAI .LT. 0) THEN
        ERRIND = 80
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -117
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = PFAI
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      STYLE  = ID(3)
      STYLID = ID(4)
      COLI   = ID(5)
      RETURN
C
  100 CONTINUE
      STYLE  = -1
      STYLID = -1
      COLI   = -1
      RETURN
      END
