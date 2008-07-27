C
C	$Id: capdat.f,v 1.5 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE CAPDAT
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA CAPDATX
C
C  Define the data used by the CAPCHG program.
C
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
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
C
C  CONTROL THE PRINTING OF INPUT LINES
C
      COMMON /CAPSKP/ SKIPIT
      LOGICAL SKIPIT
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
C
C  Used for initialization of the device color map.
C
      INTEGER II
C
C  Set input line printing to off.
C
      DATA SKIPIT/.TRUE./
C
C  Define the error flags.
C
      DATA ALLOK , EOFFL , INTERR, MAXINT, PLBERR / 0,  1,  2,  3,  4/
      DATA PMBERR, FABERR, TXBERR, FLTERR, MAXFLT / 5,  6,  7,  8,  9/
      DATA NOTINT, SIZERR, UNDASC, DEVERR, DOCERR /10, 11, 12, 13, 14/
      DATA TBLERR, STSZER, ENTERR, TABERR, TABSER /15, 16, 17, 18, 19/
      DATA PRSFIL                                 /20                /
C
C  Define the keyword separator and terminator.
C
      DATA KEYSEP/'_'/, KEYTER/' '/
C
C  Define the two-character sequence to indicate comment lines
C  in the input file.
C
      DATA FRMCOM /'/','*'/
C
C  Initialize the variable containing the flag for the specific
C  graphcap.
C
      DATA NFLG/0/
C
C  Initialize the beginning of file flag.
C
      DATA NFST/1/
C
C  Initialize the parser tables--see the documentation in the driver
C  CAPCHG for details.
C
      DATA (PART1(II),II=   1, 161) / 'D'
     + ,'E','V','I','C','E',' ','L','I','N','E',' ',' ',' ','U','S','E'
     + ,'R',' ',' ',' ','B','U','N','D','L','E',' ','T','E','X','T',' '
     + ,' ',' ','M','A','R','K','E','R',' ','P','O','L','Y','G','O','N'
     + ,'D','A','S','H',' ',' ',' ','R','A','S','T','E','R',' ','G','R'
     + ,'A','P','H','I','C','T','E','X','T',' ',' ',' ','C','O','O','R'
     + ,'D',' ',' ','C','O','L','O','R',' ',' ','B','A','T','C','H',' '
     + ,' ','C','U','R','S','O','R',' ','M','A','P',' ',' ',' ',' ','E'
     + ,'R','A','S','E',' ',' ','V','E','C','T','O','R',' ','W','I','N'
     + ,'D','O','W',' ','D','R','A','W',' ',' ',' ',' ',' ',' ',' ',' '
     + ,' ',' ',' ',' ','M','O','V','E',' ',' ',' ',' ',' ',' ',' ',' '
     +   /
      DATA (PART1(II),II= 162, 322) / ' '
     + ,' ',' ',' ','C','O','L','O','R',' ',' ',' ',' ',' ',' ',' ',' '
     + ,' ',' ',' ','W','I','D','T','H',' ',' ',' ',' ',' ',' ',' ',' '
     + ,' ',' ',' ','B','A','C','K','G','R','O','U','N','D','_','C','O'
     + ,'L','O','R','P','O','I','N','T',' ',' ',' ',' ',' ',' ',' ',' '
     + ,' ',' ',' ','I','N','I','T','L','O','W','E','R','_','L','E','F'
     + ,'T','_','X',' ','L','O','W','E','R','_','L','E','F','T','_','Y'
     + ,' ','U','P','P','E','R','_','R','I','G','H','T','_','X','U','P'
     + ,'P','E','R','_','R','I','G','H','T','_','Y','F','O','R','M','A'
     + ,'T',' ',' ',' ',' ',' ',' ',' ','E','N','C','O','D','I','N','G'
     + ,' ',' ',' ',' ',' ','X','O','F','F','S','E','T',' ',' ',' ',' '
     +   /
      DATA (PART1(II),II= 323, 483) / ' '
     + ,' ','Y','O','F','F','S','E','T',' ',' ',' ',' ',' ',' ','X','S'
     + ,'C','A','L','E',' ',' ',' ',' ',' ',' ',' ','Y','S','C','A','L'
     + ,'E',' ',' ',' ',' ',' ',' ',' ','F','L','O','A','T','I','N','G'
     + ,'_','I','N','F','O','A','V','A','I','L','A','B','L','E','I','N'
     + ,'D','E','X',' ',' ',' ',' ','P','O','L','Y','_','F','L','A','G'
     + ,' ',' ','I','N','S','T','R','U','C','T','I','O','N','I','N','S'
     + ,'T','R','U','C','T','I','O','N','F','O','R','M','A','T',' ',' '
     + ,' ',' ',' ',' ',' ','E','N','C','O','D','I','N','G',' ',' ',' '
     + ,' ',' ','F','L','O','A','T','I','N','G','_','I','N','F','O','S'
     + ,'T','A','R','T',' ',' ',' ',' ',' ','T','E','R','M','I','N','A'
     +   /
      DATA (PART1(II),II= 484, 644) / 'T'
     + ,'O','R','P','R','O','M','P','T','H','O','M','E','A','V','A','I'
     + ,'L','A','B','L','E',' ',' ','I','N','D','E','X',' ',' ',' ',' '
     + ,' ',' ','I','N','T','E','N','S','I','T','Y',' ',' ','I','N','D'
     + ,'I','V','I','D','U','A','L',' ','I','N','S','T','R','U','C','T'
     + ,'I','O','N','I','N','I','T',' ',' ',' ',' ',' ',' ',' ','M','O'
     + ,'D','E','L',' ',' ',' ',' ',' ',' ','R','A','N','G','E','_','M'
     + ,'A','X',' ',' ',' ',' ','R','A','N','G','E','_','D','E','F','I'
     + ,'N','E','D','L','I','N','E',' ',' ',' ','M','A','R','K','E','R'
     + ,' ','P','O','L','Y','G','O','N','T','E','X','T',' ',' ',' ','I'
     + ,'N','D','E','X','T','Y','P','E',' ','W','I','D','T','H','C','O'
     +   /
      DATA (PART1(II),II= 645, 805) / 'L'
     + ,'O','R','I','N','D','E','X','T','Y','P','E',' ','S','I','Z','E'
     + ,' ','C','O','L','O','R','I','N','D','E','X',' ',' ',' ','I','N'
     + ,'T','E','R','I','O','R','S','T','Y','L','E',' ',' ',' ','C','O'
     + ,'L','O','R',' ',' ',' ','I','N','D','E','X',' ',' ',' ',' ','F'
     + ,'O','N','T',' ',' ',' ',' ',' ','P','R','E','C','I','S','I','O'
     + ,'N','C','E','X','P','N',' ',' ',' ',' ','C','S','P','A','C','E'
     + ,' ',' ',' ','C','O','L','O','R',' ',' ',' ',' ','C','O','L','O'
     + ,'R',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '
     + ,' ','V','E','C','T','O','R',' ',' ',' ',' ',' ',' ',' ',' ',' '
     + ,' ',' ',' ',' ',' ',' ','I','N','S','T','R','U','C','T','I','O'
     +   /
      DATA (PART1(II),II= 806, 966) / 'N'
     + ,' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','S','I','M','U','L','A'
     + ,'T','E',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','B'
     + ,'A','C','K','G','R','O','U','N','D','_','C','O','L','O','R',' '
     + ,' ',' ',' ',' ','P','O','I','N','T',' ',' ',' ',' ',' ',' ',' '
     + ,' ',' ',' ',' ',' ',' ',' ',' ',' ','D','O','T','_','S','I','Z'
     + ,'E',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','S','I'
     + ,'M','U','L','A','T','I','O','N','_','S','P','A','C','I','N','G'
     + ,' ',' ',' ','S','I','M','U','L','A','T','I','O','N','_','T','R'
     + ,'U','N','C','A','T','I','O','N','M','A','X','I','M','U','M','_'
     + ,'P','O','I','N','T','S',' ',' ',' ',' ',' ',' ',' ','H','A','T'
     +   /
      DATA (PART1(II),II= 967,1127) / 'C'
     + ,'H','_','S','P','A','C','I','N','G',' ',' ',' ',' ',' ',' ',' '
     + ,' ','C','O','U','N','T','B','I','T','_','L','E','N','G','T','H'
     + ,'I','N','S','T','R','U','C','T','I','O','N',' ',' ','F','O','R'
     + ,'M','A','T',' ',' ',' ',' ',' ',' ',' ','E','N','C','O','D','I'
     + ,'N','G',' ',' ',' ',' ',' ','R','A','N','G','E',' ',' ',' ',' '
     + ,' ',' ',' ',' ','S','C','A','L','E',' ',' ',' ',' ',' ',' ',' '
     + ,' ','F','L','O','A','T','I','N','G','_','I','N','F','O','C','O'
     + ,'O','R','D',' ',' ',' ',' ',' ','D','A','T','A',' ',' ',' ',' '
     + ,' ',' ','V','E','C','T','O','R',' ',' ',' ',' ','H','O','R','I'
     + ,'Z','O','N','T','A','L','S','I','M','U','L','A','T','E',' ',' '
     +   /
      DATA (PART2(II),II=   1, 104) /    10
     + ,   20,   48,   59,   81,   81,   81,   93,  100,   26,   26
     + ,   27,   38,    0,   49,   50,    0,   92,   27,   40,   42
     + ,   42,   94,   42,   46,    0,    0,    0,    0,    0,    0
     + ,    0,    0,    0,    0,    0,    0,    0,   43,    0,   46
     + ,   46,    0,    0,    0,    0,    0,    0,    0,    0,   57
     + ,   43,    0,   46,    0,    0,    0,    0,   63,   67,   71
     + ,   75,    0,    0,    0,    0,    0,    0,    0,    0,    0
     + ,    0,    0,    0,    0,    0,    0,    0,    0,    0,   42
     + ,   92,   46,    0,   42,   46,    0,    0,    0,    0,    0
     + ,   43,    0,   46,    0,    0,    0,    0,    0,   27,   43
     + ,   92,   42,    0
     +   /
      DATA (PART3(II),II=   1, 104) /     1
     + ,    1,    1,    1,    1,    1,    1,    1,    1,    2,    2
     + ,    2,    2,    2,    2,    2,    2,    2,    2,    3,    3
     + ,    3,    3,    3,    3,    4,    5,    5,    5,    5,    5
     + ,    5,    5,    5,    5,    5,    5,    6,    6,    7,    7
     + ,    8,    9,    9,    9,   10,   10,   11,   12,   13,   13
     + ,   13,   13,   13,   13,   13,   14,   14,   15,   15,   15
     + ,   15,   16,   16,   16,   16,   17,   17,   17,   17,   18
     + ,   18,   18,   18,   19,   19,   19,   19,   19,   19,   20
     + ,   20,   20,   20,   20,   20,   20,   20,   20,   20,   20
     + ,   21,   22,   23,   23,   23,   23,   23,   23,   24,   24
     + ,   24,   24,   24
     +   /
      DATA (PART4(II),II=   1,  24) /     1
     + ,   64,  134,  230,  234,  377,  395,  417,  428,  467,  487
     + ,  493,  497,  574,  600,  628,  648,  668,  700,  754,  985
     + ,  990, 1000, 1078
     +   /
      DATA (PART5(II),II=   1,  48) /     9
     + ,    7,   10,    7,    6,   16,    1,    4,   11,   13,    2
     + ,    9,    2,   11,    1,   11,    3,   13,    2,   10,    1
     + ,    6,    1,    4,    7,   11,    2,   13,    4,    7,    4
     + ,    5,    4,    5,    4,    8,    6,    9,   11,   21,    1
     + ,    5,    1,   10,    6,   13,    5,   10
     +   /
C
      END
