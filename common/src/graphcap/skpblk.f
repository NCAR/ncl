C
C	$Id: skpblk.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE SKPBLK(IOS, STATUS)
C
C  Position the buffer pointer to point at the next non-blank
C  character in the file.
C
C
C  OUTPUT
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
C
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
C
      INTEGER IOS, STATUS
      CHARACTER*1 BLANK
      INTEGER II
C
      DATA BLANK/' '/
C
C  Scan the current buffer for a non-blank.
C
 10   CONTINUE
      DO 20 II = IPTR,LSIZE
        IF (LINE(II:II) .NE. BLANK) THEN
C
C  Non-blank found.
C
                IPTR = II
                RETURN
        END IF
 20   CONTINUE
C
C  End of current buffer and no non-blank, get another line.
C
      CALL RDLINE (IOS, STATUS)
C
C  Return if EOF or bad read.
C
      IF (STATUS.NE.ALLOK) RETURN
C
C  Go back and scan this line.
C
      GO TO 10
C
      END
