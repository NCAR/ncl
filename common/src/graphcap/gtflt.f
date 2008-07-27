C
C	$Id: gtflt.f,v 1.4 2008-07-27 12:23:43 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE GTFLT(STRING, STRMAX, STRSIZ, IOS, STATUS)
C
C  Get a string of floating point values from the input file.
C
C  INPUT
C       STRMAX  --  The maximum number of floating point values
C                   to transfer.
C
C  OUTPUT
C       STRING  --  The array of floating point values.
C       STRSIZ  --  The number of floating point values
C                   actually transfered.
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
C  FLTMX specifies the maximum number of characters allowed in any
C  floating point number occuring in the input file.  If FLTMX is
C  changed, then a corresponding change will have to be made in the
C  format statement at statement lebel 80.
C
      INTEGER FLTMX
      PARAMETER (FLTMX=10)
C
      INTEGER STRMAX, STRSIZ, IOS, STATUS
      INTEGER II, TPTR, NUMSZ
      REAL STRING(STRMAX)
      CHARACTER*1 DECPT, MINUS, ZERO, NINE
      CHARACTER*1 BUF1(FLTMX),BLANK
      CHARACTER*(FLTMX) BUF2
C
      DATA DECPT, MINUS, ZERO, NINE /'.', '-', '0', '9'/
      DATA BLANK/' '/
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
C  Check for a numeric string.
C
      IF (LINE(IPTR:IPTR).NE.DECPT.AND.LINE(IPTR:IPTR).NE.MINUS .AND.
     1   (LINE(IPTR:IPTR).LT.ZERO .OR. LINE(IPTR:IPTR).GT.NINE))
     1  RETURN
C
C  Extract characters from the floating-point input string until
C  a blank or end-of-line is encountered.
C
      NUMSZ = 0
      DO 50 II = IPTR, LSIZE
        IF (LINE(II:II) .NE. BLANK) THEN
                NUMSZ = NUMSZ + 1
C
C  NUMSZ exceeds the maximum number of digits allowed, set error
C  condition and return.
C
                IF (NUMSZ .GT. FLTMX) THEN
                        STATUS = MAXFLT
                        RETURN
                END IF
                BUF1(NUMSZ) = LINE(II:II)
                TPTR = II + 1
        ELSE
C
C  End of number.
C
                GO TO 55
        END IF
 50   CONTINUE
C
C  Set the buffer pointer to the current location.
C
 55   CONTINUE
      IPTR = TPTR
C
C  Zero out the conversion buffer.
C
      DO 60 II = 1,FLTMX
        BUF2(II:II) = ZERO
 60   CONTINUE
C
C  Move the input string to the conversion buffer.
C
       DO 70 II = 1,NUMSZ
        BUF2((FLTMX-II+1):(FLTMX-II+1)) = BUF1((NUMSZ-II)+1)
 70    CONTINUE
C
C  Convert to a floating value and store in STRING.
C
      STRSIZ = STRSIZ + 1
      READ(BUF2,80,ERR=1000) STRING(STRSIZ)
 80   FORMAT(F10.6)
C
C  If the requested number of values has been extracted, return.
C
      IF (STRSIZ .GE. STRMAX) RETURN
C
C  Continue to get next number.
C
      GO TO 10
C
C  Formatting error.
C
 1000 CONTINUE
      STATUS = FLTERR
      RETURN
      END
