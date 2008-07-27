C
C	$Id: gqpaf.f,v 1.6 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C  Set NPPAI to zero and return.  If and when any driver supports
C  pattern fill, uncomment the remaining section of code.
C
      NPPAI = 0
      RETURN
C     
C     IF (ERRIND .NE. 0) GO TO 100
C     FCODE = -115
C     CONT  = 0
C     CALL GZROI(0)
C     IL1   = 1
C     IL2   = 1
C     ID(1) = WTYPE
C     IWK   = -1
C     CALL GZIQWK(WTYPE,IWK)
C     IF (RERR.NE.0) THEN
C       ERRIND = RERR
C       GOTO 100
C     ENDIF
C     NPPAI = ID(2)
C     RETURN
C
  100 CONTINUE
      NPPAI = -1
      RETURN
      END
