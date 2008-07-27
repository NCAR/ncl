C
C	$Id: gscr.f,v 1.6 2008-07-27 00:21:01 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GSCR (WKID,CI,CR,CG,CB)
C
C  SET COLOUR REPRESENTATION
C
      INTEGER WKID,CI,ESCR
      REAL CR,CG,CB
      PARAMETER (ESCR=48)
C
      include 'gkscom.h'
C
C  Check if GKS is in the proper state.
C
      CALL GZCKST(7,ESCR,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if workstation ID is valid.
C
      CALL GZCKWK(20,ESCR,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check for invalid workstation categories.
C
      CALL GQWKC(WKID,IER,ICON,ITYP)
      CALL GQWKCA(ITYP,IER,ICAT)
      IF (ICAT .EQ. GMI) THEN
        ERS = 1
        CALL GERHND(33,ESCR,ERF)
        ERS = 0
        RETURN
      ELSE IF (ICAT .EQ. GINPUT) THEN
        ERS = 1
        CALL GERHND(35,ESCR,ERF)
        ERS = 0
        RETURN
      ELSE IF (ICAT .EQ. GWISS) THEN
        ERS = 1
        CALL GERHND(36,ESCR,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if the workstation is currently open.
C
      CALL GZCKWK(25,ESCR,WKID,IDUM,IER)
      IF (IER .NE. 0) RETURN
C
C  Check if color index is non-negative.
C
      IF (CI .LT. 0) THEN
        ERS = 1
        CALL GERHND(92,ESCR,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Check if colors are in range.
C
      IF (CR.LT.0. .OR. CR.GT.1. .OR. CG.LT.0. .OR.
     +    CG.GT.1. .OR. CB.LT.0. .OR. CB.GT.1.) THEN
        ERS = 1
        CALL GERHND(96,ESCR,ERF)
        ERS = 0
        RETURN
      ENDIF
C
C  Set function code and put out WKID, CI, and colors.
C
C  Set the flag CUFLAG to indicate that the interface call should go
C  only to the specifically designated workstation.
C
      CUFLAG = WKID
      FCODE = 56
      CONT  = 0
      CALL GZROI(0)
      IL1 = 2
      IL2 = 2
      ID(1) = WKID
      ID(2) = NOPICT
      IC1 = 1
      IC2 = 1
      IC(1) = CI
      RL1 = 3
      RL2 = 3
      RX(1) = CR
      RX(2) = CG
      RX(3) = CB
      CALL GZTOWK
      CUFLAG = -1
      IF (RERR .NE. 0) THEN
        ERS = 1
        CALL GERHND(RERR,ESCR,ERF)
        ERS = 0
      ENDIF
C
      RETURN
      END
