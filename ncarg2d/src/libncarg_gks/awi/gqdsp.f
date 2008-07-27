C
C	$Id: gqdsp.f,v 1.8 2008-07-27 00:20:59 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GQDSP(WTYPE,ERRIND,DCUNIT,RXP,RYP,LX,LY)
C
C  INQUIRE DISPLAY SPACE SIZE
C
      include 'gkscom.h'
C
      INTEGER WTYPE,ERRIND,DCUNIT,LX,LY
      REAL    RXP,RYP
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(8,-1,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check that the workstation type is valid.
C
      CALL GZCKWK(22,-1,IDUM,WTYPE,ERRIND)
      IF (ERRIND .NE. 0) GO TO 100
C
C  Check on workstation category.
C
      CALL GQWKCA(WTYPE,IERR,ICAT)
      IF (IERR .NE. 0) THEN
        ERRIND = IERR
        GO TO 100
      ENDIF
C
      IF (ICAT .EQ. GMO) THEN
        ERRIND = 31
        GO TO 100
      ELSE IF (ICAT .EQ. GMI) THEN
        ERRIND = 33
        GO TO 100
      ELSE IF (ICAT .EQ. GWISS) THEN
        ERRIND = 36
        GO TO 100
      ENDIF
C
      IF (WTYPE.EQ.GXWC .OR. WTYPE.EQ.GXWE .OR. WTYPE.EQ.GDMP .OR.
     +    WTYPE.EQ.GPIX) THEN
        DCUNIT = 1
        RXP = 1.
        RYP = 1.
        LX  = 1280
        LY  = 1024
        RETURN
      ENDIF
C
      IF ((WTYPE.GE.GPSMIN .AND. WTYPE.LE.GPSMAX) .OR.
     +     WTYPE.EQ.GPDFP .OR. WTYPE.EQ.GPDFL) THEN
        DCUNIT = 1
        RXP = 1.
        RYP = 1.
        LX  = 13335
        LY  = 13335
        RETURN
      ENDIF
C   
  100 CONTINUE
      DCUNIT  = -1
      RXP     = -1.
      RYP     = -1.
      LX      = -1
      LY      = -1
C
      RETURN
      END
