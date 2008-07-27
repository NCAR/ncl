C
C	$Id: gqwks.f,v 1.5 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQWKS(WKID,ERRIND,STATE)
C
C  INQUIRE WORKSTATION STATE
C
      include 'gkscom.h'
C
      INTEGER WKID,ERRIND,STATE
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
      ELSE IF (ICAT .EQ. GINPUT) THEN
        ERRIND = 35
        GO TO 100
      ENDIF
C
      STATE = 0
      DO 10 I=1,NACWK
        IF (WKID .EQ. SACWK(I)) THEN
          STATE =1
          RETURN
        ENDIF
   10 CONTINUE
      RETURN
C
  100 CONTINUE
      STATE = -1
      RETURN
      END
