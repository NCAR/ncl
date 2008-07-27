C
C	$Id: cappar.f,v 1.4 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE CAPPAR (IOS, STATUS)
C
C  This subroutine calls  MTCHIT  to parse keywords and then
C  invokes the appropriate subroutines to process various
C  classes.
C
C  OUTPUT
C       IOS    -  The I/O status flag.  This flag is valid only
C                 if the output variable STATUS indicates an error.
C       STATUS -  The error status:  = 0 - All OK.
C                                    = 1 - END OF FILE
C                                    > 1 - Error as defined in the CAPERR
C                                          COMMON block.
C
      COMMON /CAPERR/ ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1                FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR,
     2                UNDASC, DEVERR, DOCERR, TBLERR , STSZER, ENTERR,
     3                TABERR, TABSER, PRSFIL
      INTEGER ALLOK, EOFFL, INTERR, MAXINT, PLBERR, PMBERR,
     1        FABERR, TXBERR, FLTERR, MAXFLT, NOTINT, SIZERR, UNDASC,
     2        DEVERR, DOCERR, STSZER, ENTERR, TABERR, TABSER, TBLERR,
     3        PRSFIL
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPUSR/ UPRSTR, UPRSIZ
      INTEGER    UPRMAX
      PARAMETER (UPRMAX=80)
      INTEGER    UPRSTR(UPRMAX), UPRSIZ
      INTEGER    LENUSR
      PARAMETER (LENUSR=UPRMAX+1)
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
      COMMON /CAPSPC/ DUMSPC,ENDDSP
      INTEGER     DUMSIZ,DUMSM1
      PARAMETER (DUMSIZ=327, DUMSM1=DUMSIZ-1)
      INTEGER     DUMSPC(DUMSM1),ENDDSP
C
      INTEGER IOS, STATUS
      INTEGER WHICH(WHSIZE), ROW
C
C  Get the first line of the file.
C
      CALL RDLINE(IOS, STATUS)
      IF (STATUS.NE.ALLOK) RETURN
C
C  Continue processing, look for the next keyword.
C
 1    CONTINUE
      CALL SKPBLK(IOS, STATUS)
      IF (STATUS.NE.ALLOK) RETURN
C
C  Locate the keyword segment and store the encoded path
C  in WHICH.
C
      CALL MTCHIT (WHICH, IOS, STATUS)
      IF (STATUS.NE.ALLOK) RETURN
C
C  If there was no match, get a new line.
C
      IF (WHICH(1) .EQ. 0) GO TO 1
C
C  Set ROW to the level one command flag.
C
      ROW = WHICH(1)
C
C  Branch to the proper class processor.
C
      GO TO (100,200,300,400,500,600,700,800,900),ROW
C
C  DEVICE class processing.
C
 100  CONTINUE
      CALL DEVCLS(WHICH, IOS, STATUS)
      GO TO 9000
C
C  LINE class processing.
C
 200  CONTINUE
      CALL LINCLS(WHICH, IOS, STATUS)
      GO TO 9000
C
C  USER Class processing, get the user prompt.
C
 300  CONTINUE
      CALL GTSTR(UPRSTR, UPRMAX, UPRSIZ, IOS, STATUS)
      GO TO 9000
C
C  BUNDLE class processing.
C
 400  CONTINUE
      CALL BNDCLS(WHICH, IOS, STATUS)
      GO TO 9000
C
C  TEXT class processing.
C
 500  CONTINUE
      CALL TXTCLS(WHICH, IOS, STATUS)
      GO TO 9000
C
C  MARKER class processing.
C
 600  CONTINUE
      CALL MARCLS(WHICH, IOS, STATUS)
      GO TO 9000
C
C  POLYGON class processing.
C
 700  CONTINUE
      CALL PLGCLS(WHICH, IOS, STATUS)
C
C  DASH class processing.
C
 800  CONTINUE
      CALL DASCLS(WHICH, IOS, STATUS)
      GO TO 9000
C
C  RASTER class processing.
C
 900  CONTINUE
      CALL RASCLS(WHICH, IOS, STATUS)
      GO TO 9000
C
C  Class processing over, check STATUS.
C
 9000 CONTINUE
      IF (STATUS .NE. ALLOK) RETURN
C
C  Go back and start the scan again.
C
      GO TO 1
C
      END
