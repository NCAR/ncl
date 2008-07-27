C
C	$Id: gqgdp.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQGDP(WTYPE,GDP,ERRIND,NBND,BNDL)
C
C  INQUIRE GENERALIZED DRAWING PRIMITIVE
C
      include 'gkscom.h'
C
      INTEGER WTYPE,GDP,ERRIND,NBND,BNDL(4)
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
C  Invoke interface.
C
      FCODE = -113
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WTYPE
      ID(2) = GDP
      IWK   = -1
      CALL GZIQWK(WTYPE,IWK)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      NBND    = ID(3)
      BNDL(1) = ID(4)
      BNDL(2) = ID(5)
      BNDL(3) = ID(6)
      BNDL(4) = ID(7)
      RETURN
C
  100 CONTINUE
      NBND    = -1
      BNDL(1) = -1
      BNDL(2) = -1
      BNDL(3) = -1
      BNDL(4) = -1
      RETURN
      END
