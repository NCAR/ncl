C
C	$Id: marcls.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE MARCLS(WHICH, IOS, STATUS)
C
C  Level two MARKER class keyword parsing.
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
      COMMON /CAPMAR/ MCSSTR, MCSSIZ, MCTSTR, MCTSIZ, MARFIN, MARFMT,
     1                MRSSTR, MRSSIZ, MRTSTR, MRTSIZ, MDOTSZ
      INTEGER         MCSMAX, MCTMAX, MVCFMX, MRSMAX, MRTMAX
      PARAMETER (MCSMAX=30, MCTMAX=15, MVCFMX=5, MRSMAX=20, MRTMAX=10)
      INTEGER         MCSSTR(MCSMAX), MCSSIZ, MCTSTR(MCTMAX), MCTSIZ,
     1                MARFIN(5)     , MARFMT(MVCFMX,4)      ,
     2                MRSSTR(MRSMAX), MRSSIZ, MRTSTR(MRTMAX), MRTSIZ,
     3                MDOTSZ
      INTEGER         LENMAR
      PARAMETER (LENMAR=MCSMAX+1+MCTMAX+1+5+MVCFMX*4+MRSMAX+1+
     1           MRTMAX+1+1)
C
      INTEGER WHICH(WHSIZE), IOS, STATUS
      INTEGER ROW2, ROW3, ROW4
      INTEGER DUMMY, HOLDER(MVCFMX*4), II, JJ
C
C  Branch to the proper level 2 processing.
C
      ROW2 = WHICH(2)
      ROW3 = WHICH(3)
      ROW4 = WHICH(4)
C
C  MARKER class processing--
C
C  ROW2   ROW3   ROW4   Keyword
C  ----   ----   ----   ------------------------------------
C   1      1      1     MARKER_COLOR_INSTRUCTION_START
C   1      1      2     MARKER_COLOR_INSTRUCTION_TERMINATOR
C   2      1      1     MARKER_VECTOR_COUNT_FORMAT
C   2      1      2     MARKER_VECTOR_COUNT_ENCODING
C   3      1            MARKER_INSTRUCTION_START
C   3      2            MARKER_INSTRUCTION_TERMINATOR
C   7                   MARKER_DOT_SIZE
C
C
      IF (ROW2.EQ.1 .AND. ROW3.EQ.1) THEN
        IF (ROW4 .EQ. 1) THEN
                CALL GTSTR(MCSSTR, MCSMAX, MCSSIZ, IOS, STATUS)
        ELSE IF (ROW4 .EQ. 2) THEN
                CALL GTSTR(MCTSTR, MCTMAX, MCTSIZ, IOS, STATUS)
        END IF
      ELSE IF (ROW2.EQ.2 .AND. ROW3.EQ.1) THEN
        IF (ROW4.EQ.1) THEN
                CALL GTINT(HOLDER, MVCFMX*4, DUMMY, IOS, STATUS)
                IF (DUMMY .NE. 0) THEN
                        MARFIN(2) = DUMMY/4
                        DO 10 II = 1,MARFIN(2)
                        DO 10 JJ = 1,4
                                MARFMT(II,JJ) = HOLDER((II-1)*4+JJ)
 10                     CONTINUE
                END IF
        ELSE IF (ROW4.EQ.2) THEN
                CALL GTINT(MARFIN(1), 1, DUMMY, IOS, STATUS)
        END IF
      ELSE IF (ROW2.EQ.3 .AND. ROW3.EQ.1) THEN
                CALL GTSTR(MRSSTR, MRSMAX, MRSSIZ, IOS, STATUS)
      ELSE IF (ROW2.EQ.3 .AND. ROW3.EQ.2) THEN
                CALL GTSTR(MRTSTR, MRTMAX, MRTSIZ, IOS, STATUS)
      ELSE IF (ROW2.EQ.7) THEN
                CALL GTINT(MDOTSZ, 1, DUMMY, IOS, STATUS)
      END IF
C
      RETURN
      END
