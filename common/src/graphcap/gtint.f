C
C	$Id: gtint.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE GTINT(STRING, STRMAX, STRSIZ, IOS, STATUS)
C
C  Get a string of integers from the input file.  The special symbol
C  "W" is interpreted to be the wordsize in bits of the host machine.
C  The special symbol "W1" is interpreted as W-1.
C
C  INPUT
C       STRMAX  --  The maximum number of integer values
C                   to transfer.
C
C  OUTPUT
C       STRING  --  The array of integer values.
C       STRSIZ  --  The number of integer values actually
C                   transfered.
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
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
      INTEGER STRMAX, STRING(STRMAX), STRSIZ, IOS, STATUS
      INTEGER CURVAL, II
C
C  Set STRING size to null.
C
      STRSIZ = 0
C
C  Skip blanks.
C
 10   CONTINUE
      CALL SKPBLK(IOS, STATUS)
      IF (STATUS .NE. ALLOK) RETURN
C
C  Try to get an integer.
C
      II = IPTR
      CALL INTCNV(CURVAL, II, IOS, STATUS)
      IF (STATUS.NE.ALLOK) THEN
        IF (STATUS .EQ. NOTINT) STATUS = ALLOK
        RETURN
      END IF
C
C  If the requested number of values has not been extracted,
C  try to get another value, otherwise set error condition and
C  return.
C
      IF (STRSIZ .LT. STRMAX) THEN
        STRSIZ = STRSIZ + 1
        STRING(STRSIZ) = CURVAL
        GO TO 10
      ELSE
        STATUS = SIZERR
        RETURN
      END IF
      END
