C
C	$Id: intcnv.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE INTCNV(INTVAL, CURPTR, IOS, STATUS)
C
C  Attempt to decode an integer starting at position CURPTR
C  in the input buffer.
C
C  The special symbol C  "W" is interpreted to be the wordsize in
C  bits of the host machine.  The special symbol "W1" is interpreted
C  as W-1.
C
C  INPUT
C       CURPTR  --  The current location in the input buffer.
C
C  OUTPUT
C       INTVAL  --  The decoded integer value.
C       IOS     --  I/O status flag.  This flag is valid only
C                   if STATUS indicates an error.
C       STATUS  --  The error status as defined in COMMON CAPERR.
C
C  Note:  If the request is successful, then IPTR will be updated to
C         point to the next position past the integer;  if the request
C         fails, IPTR will remain unchanged.
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
      INTEGER INTVAL, CURPTR, IOS, STATUS
C
C  Define the maximum number of digits allowed in the input stream.
C  If this number is changed, then the integer width in the format
C  statement with label 80 must be changed to the same value as
C  INTMX.
C
      INTEGER INTMX
      PARAMETER (INTMX=6)
      INTEGER I, II, TPTR, NUMSZ
      CHARACTER*1 DECPT, MINUS, ZERO, NINE, WSZ
      CHARACTER*1 BUF1(INTMX),BLANK
      CHARACTER*(INTMX) BUF2
C
      DATA DECPT, MINUS, ZERO, NINE /'.', '-', '0', '9'/
      DATA WSZ /'W'/
      DATA BLANK/' '/
C
C  Check that the input string is numeric.
C
      I = CURPTR
      IF (LINE(I:I).NE.DECPT .AND. LINE(I:I).NE.MINUS .AND.
     1    LINE(I:I).NE.WSZ   .AND.
     2   (LINE(I:I).LT.ZERO  .OR.  LINE(I:I).GT.NINE)) THEN
        STATUS = NOTINT
        RETURN
      END IF
C
C  Process until a blank or end-of-line is encountered.
C
      NUMSZ = 0
      DO 50 II = CURPTR, LSIZE
        IF (LINE(II:II) .NE.BLANK) THEN
                NUMSZ = NUMSZ + 1
C
C  Error -- NUMSZ exceeds the maximum number of digits allowed.
C
                IF (NUMSZ .GT. INTMX) THEN
                        STATUS = MAXINT
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
C  Set buffer pointer to current location.
C
 55   CONTINUE
      IPTR = TPTR
C
C  Blank out the conversion buffer.
C
      DO 60 II = 1,INTMX
        BUF2(II:II) = BLANK
 60   CONTINUE
C
C  Move the string to the conversion buffer.
C
       DO 70 II = 1,NUMSZ
        BUF2((INTMX-II+1):(INTMX-II+1)) = BUF1((NUMSZ-II)+1)
 70    CONTINUE
C
C  Convert to an integer.
C
      IF (BUF2(INTMX:INTMX) .EQ. 'W') THEN
        INTVAL = I1MACH(5)
      ELSE IF (BUF2(INTMX-1:INTMX) .EQ. 'W1') THEN
        INTVAL = I1MACH(5) - 1
      ELSE
        READ(BUF2,80,ERR=1000) INTVAL
      ENDIF
      RETURN
C
C  Error exit.
C
 1000 CONTINUE
      STATUS = INTERR
      RETURN
C
 80   FORMAT(I6)
C
      END
