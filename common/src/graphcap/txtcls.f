C
C	$Id: txtcls.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE TXTCLS(WHICH, IOS, STATUS)
C
C  Process the TEXT keywords.
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
      COMMON /CAPTXT/ TCSSTR, TCSSIZ, TCTSTR, TCTSIZ, TXTFIN, TXTFMT,
     1                TXSSTR, TXSSIZ, TXTSTR, TXTSIZ
      INTEGER         TCSMAX, TCTMAX, TVCFMX, TXSMAX, TXTMAX
      PARAMETER (TCSMAX=10, TCTMAX=10, TVCFMX=5, TXSMAX=20, TXTMAX=20)
      INTEGER         TCSSTR(TCSMAX), TCSSIZ, TCTSTR(TCTMAX), TCTSIZ,
     1                TXTFIN(5)     , TXTFMT(TVCFMX,4)      ,
     2                TXSSTR(TXSMAX), TXSSIZ, TXTSTR(TXTMAX), TXTSIZ
      INTEGER         LENTXT
      PARAMETER (LENTXT=TCSMAX+1+TCTMAX+1+5+TVCFMX*4+TXSMAX+1+
     1                  TXTMAX+1)
C
      INTEGER WHICH(WHSIZE), IOS, STATUS
      INTEGER ROW2, ROW3, ROW4
      INTEGER DUMMY, HOLDER(TVCFMX*4), II, JJ
C
C  Branch to the proper level 2 processing.
C
      ROW2 = WHICH(2)
      ROW3 = WHICH(3)
      ROW4 = WHICH(4)
C
C  TEXT class processing.
C
C  ROW2   ROW3   ROW4   Keyword
C  ----   ----   ----   ---------------------------------------
C   1      1      1     TEXT_COLOR_INSTRUCTION_START
C   1      1      2     TEXT_COLOR_INSTRUCTION_TERMINATOR
C   2      1      1     TEXT_VECTOR_COUNT_FORMAT
C   2      1      2     TEXT_VECTOR_COUNT_ENCODING
C   3      1            TEXT_INSTRUCTION_START
C   3      2            TEXT_INSTRUCTION_TERMINATOR
C
C
      IF (ROW2.EQ.1 .AND. ROW3.EQ.1) THEN
        IF (ROW4 .EQ. 1) THEN
                CALL GTSTR(TCSSTR, TCSMAX, TCSSIZ, IOS, STATUS)
        ELSE IF (ROW4 .EQ. 2) THEN
                CALL GTSTR(TCTSTR, TCTMAX, TCTSIZ, IOS, STATUS)
        END IF
      ELSE IF (ROW2.EQ.2 .AND. ROW3.EQ.1) THEN
        IF (ROW4.EQ.1) THEN
                CALL GTINT(HOLDER, TVCFMX*4, DUMMY, IOS, STATUS)
                IF (DUMMY .NE. 0) THEN
                        TXTFIN(2) = DUMMY/4
                        DO 10 II = 1,TXTFIN(2)
                        DO 10 JJ = 1,4
                                TXTFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 10                     CONTINUE
                END IF
        ELSE IF (ROW4.EQ.2) THEN
                CALL GTINT(TXTFIN(1), 1, DUMMY, IOS, STATUS)
        END IF
      ELSE IF (ROW2.EQ.3 .AND. ROW3.EQ.1) THEN
                CALL GTSTR(TXSSTR, TXSMAX, TXSSIZ, IOS, STATUS)
      ELSE IF (ROW2.EQ.3 .AND. ROW3.EQ.2) THEN
                CALL GTSTR(TXTSTR, TXTMAX, TXTSIZ, IOS, STATUS)
      END IF
C
      RETURN
      END
