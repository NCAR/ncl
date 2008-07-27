C
C	$Id: mtchit.f,v 1.4 2008-07-27 12:23:44 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C

      SUBROUTINE MTCHIT (WHICH, IOS, STATUS)
C
C  Scan the input buffer for a keyword.
C
C  OUTPUT
C       WHICH   --  Array of flags which serve as an encoding of
C                   the matched keyword; WHICH(1) is returned if
C                   there was no match.
C       IOS     --  The I/O status word.   IOS is valid only if STATUS
C                   is set to an error (non-zero).
C       STATUS  --  The error status, as defined in COMMON CAPERR.
C
C  Note:
C
C  If a match is found, the line buffer pointer IPTR in COMMON CAPIOB
C  is updated to point to the next position after the matched keyword.
C  If no match is found, IPTR is unchanged.
C
      COMMON /CAPIOB/ UNIT, IPTR, LSIZE, NFST, NFLG
      COMMON /CAPIO2/ LINE, GRNM
      INTEGER LNMAX
      PARAMETER (LNMAX=80)
      CHARACTER*80 LINE
      CHARACTER*80 GRNM
      INTEGER UNIT, IPTR, LSIZE, NFST, NFLG
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
      INTEGER IOS, STATUS, WHICH(WHSIZE)
C
      CHARACTER*1 BLANK
      INTEGER ROW, II, IPOS, IEND, TPTR, LEVEL, ROWNUM, TBIDX, DIM1
     1       ,DIM2, CHSTRT, RPTR
      LOGICAL FNDCHR
C
      DATA BLANK/' '/
C
C  Initialize WHICH to indicate no match.
C
      WHICH(1) = 0
C
C  Skip over any blanks in the input buffer.
C
      CALL SKPBLK(IOS, STATUS)
      IF (STATUS .NE. ALLOK) RETURN
C
C  Set a temporary buffer pointer.
C
      TPTR = IPTR
C
C  Initialize the keyword level counter.
C
      LEVEL = 0
C
C   Start the search at the first keyword group in PART1.
C
      ROWNUM = 1
C
C   Continue search.
C
 10   CONTINUE
      TBIDX = PART3(ROWNUM)
      CHSTRT = PART4(TBIDX)
      DIM1 = PART5((TBIDX*2)-1)
      DIM2 = PART5(TBIDX*2)
      LEVEL = LEVEL + 1
C
C  Loop through all the keywords in the current group.
C
      DO 100 ROW = 1,DIM1
        IEND = DIM2 * ROW + CHSTRT
        IPOS = DIM2 * (ROW-1) + CHSTRT
C
C  Check for a keyword match starting at the current buffer position.
C
        DO 50 II = TPTR, LSIZE
C
C  Continue as long as the characters match.
C
                IF (LINE(II:II) .NE. PART1(IPOS)) GO TO 100
C
C  Increment the position pointer.
C
                IPOS = IPOS + 1
C
C  If we are at the end of a table entry, then we have a match.
C
                IF (IPOS.GE.IEND .OR. PART1(IPOS).EQ.BLANK) THEN
C
C  Set WHICH for this level to flag the number of the keyword in
C  the current group of keywords and go to the next level (group
C  of keywords) in the parse table.
C
                        WHICH(LEVEL) = ROW
                        TPTR = II
                        GO TO 1000
                END IF
  50    CONTINUE
C
C  No match, check next the next keyword in the current group for a match.
C
 100  CONTINUE
C
C  No match, increment the buffer pointer and return.
C
      WHICH(1) = 0
      IPTR = IPTR + 1
      RETURN
C
C  Match found in the current group of keywords, go to the next
C  level of keywords.
C
 1000 CONTINUE
      ROWNUM = PART2(ROWNUM+ROW-1)
      IF (ROWNUM .EQ. 0) THEN
C
C  End of search, check that the keyword terminates with a blank.
C
        RPTR = IPTR
        IPTR = TPTR + 1
        IF ( FNDCHR(KEYTER) ) THEN
                RETURN
        ELSE
C
C  Fail, so reset buffer pointer to the start, clear WHICH, and return.
C
                IPTR = RPTR
                WHICH(1) = 0
                RETURN
        END IF
      END IF
C
C  Check for a keyword separator.
C
      RPTR = IPTR
      IPTR = TPTR + 1
      IF (FNDCHR(KEYSEP)) THEN
C
C  All OK, so continue.
C
        TPTR = IPTR
        GO TO 10
      ELSE
C
C  Failure.
C
        WHICH(1) = 0
        IPTR = RPTR
        RETURN
      END IF
C
      END
