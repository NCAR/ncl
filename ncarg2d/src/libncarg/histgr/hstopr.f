C
C $Id: hstopr.f,v 1.7 2008-07-27 00:17:15 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C *************************************************************
C
      SUBROUTINE HSTOPR (IOPT,ARRAY,ISIZE)
C
C *************************************************************
C
C SET THE HSTOGRAM OPTIONS FOR WINDOW AND/OR COLOR
C
C INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
C     ARRAY-   REAL ARRAY OF DIMENSION ISIZE
C     ISIZE-   SIZE OF ARRAY
C
C  SET COMMON DATA EQUAL TO INPUT DATA
C
C
      COMMON /HSTGC1/ HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM,
     -       HFRAME, LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL,
     -       FREQNC, HWIND(4), COLSHA, COLREC, COLAXI, COLMED, COLTEX,
     -       COLTIT, COLPER, DRAWL, SPACE, LABMAX, CHARL, HEIGHT,
     -       ORIENT, COLSH2, SETSPA, SETSP2, MVALU, SETMVA, SETEPS,
     -       NMVAL, PMVAL, PERTIT
      LOGICAL HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM, HFRAME,
     -        LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL, FREQNC,
     -        DRAWL, SPACE, CHARL, MVALU, NMVAL, PMVAL, PERTIT
      COMMON /HSTGC2/ STRFOR, STRTIT, STRLAB, STRFRE, STRPER, LABTEX
      CHARACTER*96  STRTIT
      CHARACTER*55  STRFOR, STRLAB, STRFRE, STRPER
      CHARACTER*15 LABTEX(30)
      CHARACTER*7  IOPT
      CHARACTER*2  TAG, OPT
      INTEGER  COLSHA, COLREC, COLAXI, COLMED, COLTEX
      INTEGER  COLTIT, COLPER, COLSH2, HEIGHT, ORIENT
      REAL ARRAY(ISIZE)
C
      NERR = 0
C
C  DETERMINE THE OPTION DESIRED
C
      TAG = IOPT(1:2)
      IF (IOPT(3:3) .EQ. '=') THEN
          OPT = IOPT(4:5)
      ELSE
          OPT = IOPT(5:6)
      ENDIF
C
C  WINDOWING FLAG
C
      IF (TAG .EQ. 'WI') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
              WINDOW = .TRUE.
              HWIND(1) = ARRAY(1)
              HWIND(2) = ARRAY(2)
              HWIND(3) = ARRAY(3)
              HWIND(4) = ARRAY(4)
              RETURN
C
C  SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
                WINDOW = .FALSE.
              RETURN
          ELSE
                GOTO 120
          ENDIF
C
C Set Spacing FLAG and parameters.
C
      ELSEIF (TAG .EQ. 'SP') THEN
        IF (OPT .EQ. 'ON') THEN
          SPACE = .TRUE.
          SETSPA = ARRAY(1)
          SETSP2 = ARRAY(2)
          IF(SETSPA .GT. 4.0 .OR. SETSPA .LT. 0.0) GOTO 130
          IF(SETSP2 .GT. 4.0 .OR. SETSP2 .LT. -4.0) GOTO 130
          RETURN
        ELSEIF (OPT .EQ. 'OF') THEN
          SPACE = .FALSE.
          RETURN
        ELSE
          GOTO 120
        ENDIF
C
C Set special value flag.
C
      ELSEIF (TAG .EQ. 'MV') THEN
        IF (OPT .EQ. 'ON') THEN
	  MVALU = .TRUE.
	  SETMVA = ARRAY(1)
	  SETEPS = ARRAY(2)
          RETURN
        ELSEIF (OPT .EQ. 'OF') THEN
	  MVALU = .FALSE.
          RETURN
        ELSE
          GOTO 120
        ENDIF
      ELSE
        GOTO 120
      ENDIF
C
C  ERROR UNDEFINED OPTION DETECTED
C
  120 NERR = NERR + 1
      CALL SETER (' HSTOPR -- UNDEFINED OPTION',NERR,IREC)
      RETURN
  130 NERR = NERR + 1
      CALL SETER (' HSTOPR -- INVALID SPACING PARAMETER ',NERR,IREC)
      RETURN
      END
