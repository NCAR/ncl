C
C	$Id: gqpx.f,v 1.5 2008-07-27 00:21:00 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQPX(WKID,PX,PY,ERRIND,COLI)
C
C  INQUIRE PIXEL
C
      include 'gkscom.h'
C
      INTEGER WKID,ERRIND,COLI
      REAL    PX,PY
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check that the workstation identifier is valid.
C
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check that the workstation is open.
C
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check for invalid workstation categories.
C
      CALL GQWKC(WKID,ERRIND,ICONID,ITYPE)
      IF (ERRIND .NE. 0) GO TO 100
      CALL GQWKCA(ITYPE,ERRIND,ICAT)
      IF (ICAT.NE.GOUTPT .AND. ICAT.NE.GOUTIN) THEN
        ERRIND = 39
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -290
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      RL1   = 2
      RL2   = 2
      ID(1) = WKID
      CALL GZW2NX(1,PX,PXD)
      CALL GZW2NY(1,PY,PYD)
      RX(1) = PXD
      RX(2) = PYD
      CALL GZIQWK(ITYPE,WKID)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      COLI   = ID(2)
      RETURN
C
  100 CONTINUE
      COLI   = -1
      RETURN
      END
