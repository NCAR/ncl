C
C	$Id: gqwkt.f,v 1.5 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQWKT(WKID,ERRIND,TUS,RWINDO,CWINDO,
     +                 RVIEWP,CVIEWP)
C
C  INQUIRE WORKSTATION TRANSFORMATION
C
      include 'gkscom.h'
C
      INTEGER WKID,ERRIND,TUS
      REAL    RWINDO(4),CWINDO(4),RVIEWP(4),CVIEWP(4)
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
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check for invalid workstation categories.
C
      CALL GQWKC(WKID,ERRIND,ICONID,ITYPE)
      IF (ERRIND .NE. 0) GO TO 100
      CALL GQWKCA(ITYPE,ERRIND,ICAT)
      IF (ERRIND .NE. 0) GO TO 100
      IF (ICAT .EQ. GMI) THEN
        ERRIND = 33
        GO TO 100
      ELSE IF (ICAT .EQ. GWISS) THEN
        ERRIND = 36
        GO TO 100
      ENDIF
C
C  Invoke interface.
C
      FCODE = -202
      CONT  = 0
      CALL GZROI(0)
      IL1   = 1
      IL2   = 1
      ID(1) = WKID
      CALL GZIQWK(ITYPE,WKID)
      IF (RERR .NE. 0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      TUS = ID(2)
      RWINDO(1) = RX(1)
      RWINDO(2) = RX(2)
      RWINDO(3) = RX(3)
      RWINDO(4) = RX(4)
      CWINDO(1) = RX(5)
      CWINDO(2) = RX(6)
      CWINDO(3) = RX(7)
      CWINDO(4) = RX(8)
      RVIEWP(1) = RY(1)
      RVIEWP(2) = RY(2)
      RVIEWP(3) = RY(3)
      RVIEWP(4) = RY(4)
      CVIEWP(1) = RY(5)
      CVIEWP(2) = RY(6)
      CVIEWP(3) = RY(7)
      CVIEWP(4) = RY(8)
      RETURN
C
  100 CONTINUE
      TUS = -1
      RWINDO(1) = 0.
      RWINDO(2) = 0.
      RWINDO(3) = 0.
      RWINDO(4) = 0.
      CWINDO(1) = 0.
      CWINDO(2) = 0.
      CWINDO(3) = 0.
      CWINDO(4) = 0.
      RVIEWP(1) = 0.
      RVIEWP(2) = 0.
      RVIEWP(3) = 0.
      RVIEWP(4) = 0.
      CVIEWP(1) = 0.
      CVIEWP(2) = 0.
      CVIEWP(3) = 0.
      CVIEWP(4) = 0.
C
      RETURN
      END
