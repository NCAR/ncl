C
C	$Id: gqeci.f,v 1.5 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQECI(WKID,N,ERRIND,OL,COLIND)
C
C  INQUIRE LIST element OF COLOUR INDICES
C
      include 'gkscom.h'
C
      INTEGER WKID,N,ERRIND,OL,COLIND
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,-1,ERRIND)
      IF (ERRIND .NE. 0) GOTO 100
C
C  Check if the workstation ID is valid.
C
      CALL GZCKWK(20,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the specified workstation is open.
C
      CALL GZCKWK(25,-1,WKID,IDUM,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check if the list element is non-negative.
C
      IF (N.LT.0) THEN
       ERRIND = 2002
       GOTO 100
      ENDIF
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
      ELSE IF (ICAT .EQ. GINPUT) THEN
        ERRIND = 35
        GO TO 100
      ELSE IF (ICAT .EQ. GWISS) THEN
        ERRIND = 36
        GO TO 100
      ENDIF
C
C  Invoke the workstation interface routine.
C
      FCODE = -257
      CONT  = 0
      CALL GZROI(0)
      IL1   = 2
      IL2   = 2
      ID(1) = WKID
      ID(2) = N
      CALL GZIQWK(ITYPE,WKID)
      IF (RERR.NE.0) THEN
        ERRIND = RERR
        GOTO 100
      ENDIF
      OL     = ID(3)
      COLIND = ID(4)
      RETURN
C
  100 CONTINUE
      OL     = -1
      COLIND = -1
      RETURN
      END
