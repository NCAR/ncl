C
C	$Id: lincls.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE LINCLS(WHICH, IOS, STATUS)
C
C  Level two line class keyword parsing.
C
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
      COMMON /CAPLIN/ PLAVBL, LDSSTR, LDSSIZ, LDTSTR, LDTSIZ,
     1                LMSSTR, LMSSIZ, LMTSTR, LMTSIZ, LCSSTR,
     2                LCSSIZ, LCTSTR, LCTSIZ, LINFIN, LINFMT,
     3                LWSSTR, LWSSIZ, LWTSTR, LWTSIZ, LWTFIN,
     4                LWTFMT, LWTRNG, LWTSCF, LBSSTR, LBSSIZ,
     5                LBTSTR, LBTSIZ, LPSSTR, LPSSIZ, LPTSTR,
     6                LPTSIZ
      INTEGER         LDSMAX, LDTMAX, LMSMAX, LMTMAX, LCSMAX,
     1                LCTMAX, LVCFMX, LWSMAX, LWTMAX, LWTFMX,
     2                LBSMAX, LBTMAX, LPSMAX, LPTMAX
      PARAMETER   (LDSMAX=30, LDTMAX=15, LMSMAX=30, LMTMAX=15,
     1             LCSMAX=30, LCTMAX=15, LVCFMX=8,  LWSMAX=30,
     2             LWTMAX=15, LWTFMX=8 , LBSMAX=30, LBTMAX=15,
     3             LPSMAX=20, LPTMAX=20)
      INTEGER         LDSSTR(LDSMAX), LDSSIZ, LDTSTR(LDTMAX),
     1                LDTSIZ, LMSSTR(LMSMAX), LMSSIZ,
     2                LMTSTR(LMTMAX), LMTSIZ, LCSSTR(LCSMAX),
     3                LCSSIZ, LCTSTR(LCTMAX), LCTSIZ, LINFIN(8),
     4                LINFMT(LVCFMX,4)      , LWSSTR(LWSMAX),
     5                LWSSIZ, LWTSTR(LWTMAX), LWTSIZ, LWTFIN(8),
     6                LWTFMT(LWTFMX,4)      , LWTRNG(2)     ,
     7                LBSSTR(LBSMAX), LBSSIZ, LBTSTR(LBTMAX),
     8                LBTSIZ, LPSSTR(LPSMAX), LPSSIZ,
     9                LPTSTR(LPTMAX), LPTSIZ
      LOGICAL         PLAVBL
      REAL            LWTSCF,LINRIN(8),LWTRIN(8)
      INTEGER         LENLIN
      PARAMETER   (LENLIN=1+LDSMAX+1+LDTMAX+1+LMSMAX+1+
     1             LMTMAX+1+LCSMAX+1+LCTMAX+1+8+LVCFMX*4+
     2             LWSMAX+1+LWTMAX+1+8+LWTFMX*4+2+1+LBSMAX+
     3             1+LBTMAX+1+LPSMAX+1+LPTMAX+1)
      EQUIVALENCE (LINFIN,LINRIN), (LWTFIN,LWTRIN)
C
      INTEGER WHICH(WHSIZE), IOS, STATUS
      INTEGER ROW1, ROW2, ROW3
      INTEGER DUMMY, HOLDER(LVCFMX*4), II, JJ
C
C  Branch to the proper level 2 processing.
C
      ROW1 = WHICH(2)
      ROW2 = WHICH(3)
      ROW3 = WHICH(4)
C
C  POLYLINE processing--
C
C       ROW1   ROW2  ROW3  Keyword
C       ----   ----  ----  -----------------------------------
C       1       1          LINE_DRAW_POLY_FLAG
C       1       2     1    LINE_DRAW_INSTRUCTION_START
C       1       2     2    LINE_DRAW_INSTRUCTION_TERMINATOR
C       2       1     1    LINE_MOVE_INSTRUCTION_START
C       2       1     2    LINE_MOVE_INSTRUCTION_TERMINATOR
C       3       1     1    LINE_COLOR_INSTRUCTION_START
C       3       1     2    LINE_COLOR_INSTRUCTION_TERMINATOR
C       4       1     1    LINE_WIDTH_INSTRUCTION_START
C       4       1     2    LINE_WIDTH_INSTRUCTION_TERMINATOR
C       4       2          LINE_WIDTH_FORMAT
C       4       3          LINE_WIDTH_ENCODING
C       4       4          LINE_WIDTH_RANGE
C       4       5          LINE_WIDTH_SCALE
C       4       6          LINE_WIDTH_FLOATING_INFO
C       5       1     1    LINE_BACKGROUND_COLOR_INSTRUCTION_START
C       5       1     2    LINE_BACKGROUND_COLOR_INSTRUCTION_TERMINATOR
C       6       1          LINE_POINT_START
C       6       2          LINE_POINT_TERMINATOR
C
      IF (ROW1.EQ.1 .AND. ROW2.EQ.1) THEN
        CALL GTLOG(PLAVBL, 1, DUMMY, IOS, STATUS)
      ELSE IF (ROW1.EQ.1 .AND. ROW2.EQ.2) THEN
        IF (ROW3.EQ.1) THEN
                CALL GTSTR(LDSSTR, LDSMAX, LDSSIZ, IOS, STATUS)
        ELSE IF (ROW3.EQ.2) THEN
                CALL GTSTR(LDTSTR, LDTMAX, LDTSIZ, IOS, STATUS)
        END IF
      ELSE IF (ROW1.EQ.2 .AND. ROW2.EQ.1) THEN
        IF (ROW3.EQ.1) THEN
                CALL GTSTR(LMSSTR, LMSMAX, LMSSIZ, IOS, STATUS)
        ELSE IF (ROW3.EQ.2) THEN
                CALL GTSTR(LMTSTR, LMTMAX, LMTSIZ, IOS, STATUS)
        END IF
      ELSE IF (ROW1.EQ.3 .AND. ROW2.EQ.1) THEN
        IF (ROW3.EQ.1) THEN
                CALL GTSTR(LCSSTR, LCSMAX, LCSSIZ, IOS, STATUS)
        ELSE IF (ROW3.EQ.2) THEN
                CALL GTSTR(LCTSTR, LCTMAX, LCTSIZ, IOS, STATUS)
        END IF
C
      ELSE IF (ROW1.EQ.4) THEN
        IF (ROW2.EQ.1) THEN
                IF (ROW3.EQ.1) THEN
                CALL GTSTR(LWSSTR, LWSMAX, LWSSIZ, IOS, STATUS)
                ELSE IF (ROW3.EQ.2) THEN
                CALL GTSTR(LWTSTR, LWTMAX, LWTSIZ, IOS, STATUS)
                END IF
        ELSE IF (ROW2.EQ.2) THEN
                CALL GTINT(HOLDER, LWTFMX*4, DUMMY, IOS, STATUS)
                IF (DUMMY .NE. 0) THEN
                        LWTFIN(2) = DUMMY/4
                        DO 20 II = 1,LWTFIN(2)
                        DO 20 JJ = 1,4
                                LWTFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 20                     CONTINUE
                END IF
        ELSE IF (ROW2.EQ.3) THEN
                CALL GTINT(LWTFIN(1), 1, DUMMY, IOS, STATUS)
        ELSE IF (ROW2.EQ.4) THEN
                CALL GTINT(LWTRNG, 2, DUMMY, IOS, STATUS)
        ELSE IF (ROW2.EQ.5) THEN
                CALL GTFLT(LWTSCF, 1, DUMMY, IOS, STATUS)
        ELSE IF (ROW2.EQ.6) THEN
                CALL GTFLT(LWTRIN(5), 4, DUMMY, IOS, STATUS)
        END IF
      ELSE IF (ROW1.EQ.5 .AND. ROW2.EQ.1) THEN
        IF (ROW3.EQ.1) THEN
                CALL GTSTR(LBSSTR, LBSMAX, LBSSIZ, IOS, STATUS)
        ELSE IF (ROW3.EQ.2) THEN
                CALL GTSTR(LBTSTR, LBTMAX, LBTSIZ, IOS, STATUS)
        END IF
      ELSE IF (ROW1.EQ.6 .AND. ROW2.EQ.1) THEN
        CALL GTSTR(LPSSTR, LPSMAX, LPSSIZ, IOS, STATUS)
      ELSE IF (ROW1.EQ.6 .AND. ROW2.EQ.2) THEN
        CALL GTSTR(LPTSTR, LPTMAX, LPTSIZ, IOS, STATUS)
C
      END IF
      RETURN
      END
