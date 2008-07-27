C
C	$Id: bndcls.f,v 1.4 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE BNDCLS(WHICH, IOS, STATUS)
C
C  Process the BUNDLE class keywords.
C
C  INPUT
C       WHICH   --  the encoded path flags.
C
C  OUTPUT
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
C
      COMMON /PARTB1/ PART1, KEYSEP, KEYTER, FRMCOM
      COMMON /PARTB2/ PART2, PART3, PART4, PART5, CTSTR, CTLOC
C
C  THE NUMBER OF WORDS IN THE SEARCH PATH MUST BE BIG ENOUGH TO HOLD
C       THE NUMBER OF BITS PER PATH TIMES THE NUMBER OF LEVELS
C
      INTEGER WHSIZE
      PARAMETER (WHSIZE=20)
C
      INTEGER PARTSZ, OTHSZ, NTABLE
      PARAMETER(PARTSZ=3000, OTHSZ=150, NTABLE=50)
      CHARACTER*1 KEYSEP,KEYTER
      CHARACTER*1 FRMCOM(2)
      INTEGER PART2(OTHSZ), PART3(OTHSZ), PART4(NTABLE), PART5(NTABLE*2)
      CHARACTER*1 PART1(PARTSZ)
      INTEGER CTSTR, CTLOC
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
      COMMON/CAPBND/PLBTEC, PLIBTB, PLTBTB, PLWBTB, PLCBTB,
     1              PMBTEC, PMIBTB, PMTBTB, PMSBTB, PMCBTB,
     2              TXBTEC, TXIBTB, TXFBTB, TXPBTB, TCXBTB,
     3              TCSBTB, TXCBTB, FABTEC, FAIBTB, FISBTB,
     4              FASBTB, FACBTB
      INTEGER       TBLSIZ
      PARAMETER (TBLSIZ=6)
      INTEGER PLBTEC,PLIBTB(TBLSIZ),PLTBTB(TBLSIZ),PLCBTB(TBLSIZ),
     1        PMBTEC,PMIBTB(TBLSIZ),PMTBTB(TBLSIZ),PMCBTB(TBLSIZ),
     2        TXBTEC,TXIBTB(TBLSIZ),TXFBTB(TBLSIZ),TXPBTB(TBLSIZ),
     3        TXCBTB(TBLSIZ),FACBTB(TBLSIZ),
     4        FABTEC,FAIBTB(TBLSIZ),FISBTB(TBLSIZ),FASBTB(TBLSIZ)
      REAL PLWBTB(TBLSIZ),PMSBTB(TBLSIZ),TCXBTB(TBLSIZ),TCSBTB(TBLSIZ)
      INTEGER LENBND
      PARAMETER (LENBND=4+18*TBLSIZ)
C
      INTEGER WHICH(WHSIZE), IOS, STATUS
      INTEGER ROW1, ROW2
      INTEGER DUMMY
C
C  Branch to the proper level 2 processing.
C
      ROW1 = WHICH(2)
      ROW2 = WHICH(3)
C
C  BUNDLE processing--
C
C      ROW1    ROW2     Keyword
C      ----    ----     -------------------------
C       1       1       BUNDLE_LINE_INDEX
C       1       2       BUNDLE_LINE_TYPE
C       1       3       BUNDLE_LINE_WIDTH
C       1       4       BUNDLE_LINE_COLOR
C       2       1       BUNDLE_MARKER_INDEX
C       2       2       BUNDLE_MARKER_TYPE
C       2       3       BUNDLE_MARKER_SIZE
C       2       4       BUNDLE_MARKER_COLOR
C       3       1       BUNDLE_POLYGON_INDEX
C       3       2       BUNDLE_POLYGON_INTERIOR
C       3       3       BUNDLE_POLYGON_STYLE
C       3       4       BUNDLE_POLYGON_COLOR
C       4       1       BUNDLE_TEXT_INDEX
C       4       2       BUNDLE_TEXT_FONT
C       4       3       BUNDLE_TEXT_PRECISION
C       4       4       BUNDLE_TEXT_CEXPN
C       4       5       BUNDLE_TEXT_CSPACE
C       4       6       BUNDLE_TEXT_COLOR
C
C  Branch to proper level 2 .
C
      GO TO (100,200,300,400),ROW1
C
C  BUNDLE_LINE class.
C
 100  GO TO (110,120,130,140),ROW2
 110    CONTINUE
        CALL GTINT(PLIBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 190
 120    CONTINUE
        CALL GTINT(PLTBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 190
 130    CONTINUE
        CALL GTFLT(PLWBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 190
 140    CONTINUE
        CALL GTINT(PLCBTB, TBLSIZ, DUMMY, IOS, STATUS)
 190    CONTINUE
        IF (STATUS.NE.ALLOK) RETURN
C
C  Check if the lengths of all the tables are the same.
C
        IF (PLBTEC .EQ. -1) THEN
                PLBTEC = DUMMY
        ELSE
                IF (PLBTEC.NE.DUMMY) STATUS = PLBERR
        END IF
        RETURN
C
C  BUNDLE_MARKER class.
C
 200  GO TO (210,220,230,240),ROW2
 210    CONTINUE
        CALL GTINT(PMIBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 290
 220    CONTINUE
        CALL GTINT(PMTBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 290
 230    CONTINUE
        CALL GTFLT(PMSBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 290
 240    CONTINUE
        CALL GTINT(PMCBTB, TBLSIZ, DUMMY, IOS, STATUS)
 290    CONTINUE
        IF (STATUS.NE.ALLOK) RETURN
C
C  Check if the lengths of all the tables are the same.
C
        IF (PMBTEC .EQ. -1) THEN
                PMBTEC = DUMMY
        ELSE
                IF (PMBTEC.NE.DUMMY) STATUS = PMBERR
        END IF
        RETURN
C
C  BUNDLE_POLYGON class.
C
 300  GO TO (310,320,330,340),ROW2
 310    CONTINUE
        CALL GTINT(FAIBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 390
 320    CONTINUE
        CALL GTINT(FISBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 390
 330    CONTINUE
        CALL GTINT(FASBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 390
 340    CONTINUE
        CALL GTINT(FACBTB, TBLSIZ, DUMMY, IOS, STATUS)
 390    CONTINUE
        IF (STATUS.NE.ALLOK) RETURN
C
C  Check if the lengths of all the tables are the same.
C
        IF (FABTEC .EQ. -1) THEN
                FABTEC = DUMMY
        ELSE
                IF (FABTEC.NE.DUMMY) STATUS = FABERR
        END IF
        RETURN
C
C  BUNDLE_TEXT class.
C
 400  GO TO (410,420,430,440,450,460),ROW2
 410    CONTINUE
        CALL GTINT(TXIBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 490
 420    CONTINUE
        CALL GTINT(TXFBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 490
 430    CONTINUE
        CALL GTINT(TXPBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 490
 440    CONTINUE
        CALL GTFLT(TCXBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 490
 450    CONTINUE
        CALL GTFLT(TCSBTB, TBLSIZ, DUMMY, IOS, STATUS)
        GO TO 490
 460    CONTINUE
        CALL GTINT(TXCBTB, TBLSIZ, DUMMY, IOS, STATUS)
 490    CONTINUE
        IF (STATUS.NE.ALLOK) RETURN
C
C  Check if the lengths of all the tables are the same.
C
        IF (TXBTEC .EQ. -1) THEN
                TXBTEC = DUMMY
        ELSE
                IF (TXBTEC.NE.DUMMY) STATUS = TXBERR
        END IF
        RETURN
C
      END
