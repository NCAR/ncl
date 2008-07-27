C
C	$Id: gtstr.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE GTSTR(STRING, STRMAX, STRSIZ, IOS, STATUS)
C
C  Get a string of ASCII data values from the input file.
C  Each legal data value must be terminated by a blank or by an
C  end-of-line.
C
C  INPUT
C       STRMAX  --  Maximum number of characters to transfer.
C
C  OUTPUT
C       STRING  --  An integer array containing the ADE of the
C                   characters transferred.
C       STRSIZ  --  The number of characters actually transferred.
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
      COMMON /CAPASC/ ASCTB1, ASCTB2
      INTEGER TBLLE1, TBLWT1, INTVAL, TBLLE2, CORDVL
      PARAMETER (TBLLE1=41, TBLWT1=3, INTVAL=-1, TBLLE2=94, CORDVL=-7)
      CHARACTER*1 ASCTB1(TBLLE1, TBLWT1), ASCTB2(TBLLE2)
      COMMON /CAPASI/ ASCVL1, ASCVL2
      INTEGER ASCVL1(TBLLE1), ASCVL2(TBLLE2)
C
      INTEGER STRMAX, STRING(STRMAX), STRSIZ, IOS, STATUS
C
      INTEGER II,JJ,KK, AV, CURVAL
      CHARACTER*1 BLANK, CURCHR
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
C  Initialize search for an ASCII character (or processor dependent
C  strings, such as XYC, INT, etc.) which is represented
C  by a multi-character string.
C
      JJ = 1
 40   CONTINUE
      KK = 1
      II = IPTR
 50   CONTINUE
      CURCHR = ASCTB1(JJ,KK)
C
C  Search the ASCII definition table.
C
      IF(LINE(II:II).EQ. CURCHR) THEN
C
C  Match found with the current single character;  if we
C  have matched TBLWT1 single characters, then we have
C  an ASCII character match.
C
        IF (KK .EQ. TBLWT1) THEN
                AV = ASCVL1(JJ)
                GO TO 200
        END IF
C
C  Increment the width pointer and get the next character.
C
        KK = KK + 1
        CURCHR = ASCTB1(JJ,KK)
C
C  If the current character extracted from ASCTB1 is a blank, then
C  we must have a multi-character match.
C
        IF (CURCHR .EQ. BLANK) THEN
                AV = ASCVL1(JJ)
                GO TO 200
        END IF
C
C  If end of input line branch to new table entry.
C
        IF (II .LT. LSIZE) THEN
                II = II + 1
                GO TO 50
        END IF
      END IF
C
C  Go to new table entry.
C
      IF (JJ .EQ. TBLLE1) GO TO 100
      JJ = JJ + 1
      GO TO 40
C
C  Initialize the search for an ASCII character which
C  has an ADE between 33 (decimal) and 126 (decimal).
C
 100  CONTINUE
      JJ = 1
      II = IPTR
140   CONTINUE
      CURCHR = ASCTB2(JJ)
C
C  Search the ASCII definition table.
C
      IF(LINE(II:II).EQ. CURCHR) THEN
C
C  The current position is a match.
C
        AV = ASCVL2(JJ)
        GO TO 200
      END IF
C
C  Go to a new table entry.
C
      IF (JJ .EQ. TBLLE2) THEN
C
C  No match, so set the buffer pointer and return.
C
        IPTR = II
        RETURN
      END IF
      JJ = JJ + 1
      GO TO 140
C
C
 200  CONTINUE
C
C  ASCII table match, add value to STRING.
C
      IF (AV .GE. 0) THEN
C
C  Make certain that the next input position is a blank or
C  end-of-line, otherwise we don't really have a match.
C
        II = II + 1
        IF (II .LE.LSIZE) THEN
C
C  Not a match.
C
                IF (LINE(II:II) .NE. BLANK) RETURN
        END IF
C
C  A true (non processor dependent) ASCII character has been matched,
C  put it in STRING and search for another.
C
        CURVAL = AV
        IPTR = II
C
      ELSE IF (AV .EQ. INTVAL) THEN
C
C  INT flag has been matched, so decode the integer value following it.
C
        II = II + 1
        CALL INTCNV(CURVAL,II,IOS,STATUS)
        IF (STATUS.NE.ALLOK) THEN
                IF (STATUS.NE.NOTINT) RETURN
                STATUS = ALLOK
                RETURN
        END IF
      ELSE IF (AV .GE. CORDVL) THEN
C
C  Processor-dependent coordinate indicator has been matched,
C  put it into STRING.
C
        CURVAL = AV
        IPTR = II + 1
      ELSE
C
C   Undefined pattern.
C
                STATUS = UNDASC
      END IF
C
C  Set the current value in the output string if there is room.
C
      IF(STRSIZ.LT.STRMAX) THEN
        STRSIZ = STRSIZ + 1
        STRING(STRSIZ) = CURVAL
        GO TO 10
      END IF
C
C   Error --  No room left in STRING buffer.
C
      STATUS = SIZERR
      RETURN
      END
