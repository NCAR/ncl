C
C $Id: hstopc.f,v 1.6 2008-07-27 00:17:15 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C *************************************************************
C
      SUBROUTINE HSTOPC (IOPT,STRING,NUMBER,ILCH)
C
C *************************************************************
C
C SET THE HISTOGRAM OPTIONS
C
C INPUT
C     IOPT-CHARACTER STRING OF OPTION VALUE
C     ARRAY-   CHARACTER INPUT DATA
C
C  SET COMMON DATA EQUAL TO INPUT DATA
C
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
      CHARACTER*(*)  STRING
      CHARACTER*15 LABTEX(30)
      CHARACTER*7  IOPT
      CHARACTER*2  TAG, OPT
      INTEGER  COLSHA, COLREC, COLAXI, COLMED, COLTEX
      INTEGER  COLTIT, COLPER, NUMBER, COLSH2, HEIGHT, ORIENT
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
C  TITLE OPTION
C
      IF (TAG .EQ. 'TI') THEN
C
C  SWITCH ON GET TITLE AND COUNT FROM INPUT
C
          IF (OPT .EQ. 'ON') THEN
              TITLE = .TRUE.
              STRTIT = STRING
              RETURN
C
C  SWITCH OFF OPTION DEACTIVATED
C
          ELSEIF (OPT .EQ. 'OF') THEN
            TITLE = .FALSE.
            RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  FORMAT FLAG
C
      ELSEIF (TAG .EQ. 'FO') THEN
C
C  SWITCH ON GET USER FORMAT
C
        IF (OPT .EQ. 'ON') THEN
            HSTFOR = .TRUE.
            STRFOR = STRING
	    LABMAX = NUMBER
            RETURN
C
C SWITCH OFF SET FORMAT TO DEFAULT
C
          ELSEIF (OPT .EQ. 'OF') THEN
            HSTFOR = .FALSE.
            STRFOR = '(G10.3)'
            RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  LABEL FLAG
C
      ELSEIF (TAG .EQ. 'LA') THEN
C
C  SWITCH ON GET USER LABEL
C
        IF (OPT .EQ. 'ON') THEN
            LABEL = .TRUE.
            STRLAB = STRING
            RETURN
C
C SWITCH OFF
C
        ELSEIF (OPT .EQ. 'OF') THEN
            LABEL = .FALSE.
            RETURN
        ELSE
                GOTO 120
        ENDIF
C
C  FREQUENCY FLAG
C
      ELSEIF (TAG .EQ. 'FQ') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            FREQNC = .TRUE.
            STRFRE = STRING
            RETURN
C
C SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            FREQNC = .FALSE.
            RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  PERCENT TITLE FLAG.
C
      ELSEIF (TAG .EQ. 'PT') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
	    PERTIT = .TRUE.
	    STRPER = STRING
            RETURN
C
C SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
	    PERTIT = .FALSE.
            RETURN
          ELSE
                GOTO 120
          ENDIF
C
C  CHARACTER FLAG
C
      ELSEIF (TAG .EQ. 'CH') THEN
C
C  SWITCH ON
C
          IF (OPT .EQ. 'ON') THEN
            CHARL = .TRUE.
            DO 100 I = 1,NUMBER
              J = I + (I-1)*(ILCH-1)
              K = J + ILCH-1
              LABTEX(I) = STRING(J:K)
  100       CONTINUE
            RETURN
C
C SWITCH OFF
C
          ELSEIF (OPT .EQ. 'OF') THEN
            CHARL = .FALSE.
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
 120  NERR = NERR + 1
      CALL SETER (' HSTOPC -- UNDEFINED OPTION',NERR,IREC)
      RETURN
      END
