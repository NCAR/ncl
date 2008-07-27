C
C	$Id: sfgtkw.f,v 1.4 2008-07-27 12:23:42 haley Exp $
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SFGTWK (WHICH, IOS, STATUS)
C
C  Scan the input line for a keyword.
C
C  OUTPUT
C    WHICH  -  Flag indicating which keyword was found.  WHICH is
C              returned as zero if there was no keyword found.
C    IOS    -  The I/O status, this value has meaning only if an 
C              error condition obtains (i.e. only if STATUS .GT. 0).
C    STATUS -  Status indicator, see subroutine SFPRCF for details.
C
C  COMMON variable IPTR --
C              Points to the next position in the line when a
C              keyword has been found, otherwise IPTR is unchanged.
C
      include 'fnterr.h'
      include 'fnttab.h'
      include 'fntcom.h'
C
      INTEGER IOS, STATUS, WHICH(WHSIZE)
C
      CHARACTER*1 BLANK
      INTEGER ROW, II, IPOS, IEND, TPTR, LEVEL, ROWNUM, TBIDX, DIM1
     1       ,DIM2, CHSTRT, RPTR, LEV
      LOGICAL SFFNDC
C
      DATA BLANK/' '/
C
C  Initialize as a failure.
C
      WHICH(1) = 0
C
C  Skip over any blanks.
C
      CALL SFSKBK(IOS, STATUS)
      IF (STATUS .NE. ALLOK) RETURN
C
C  Set a temporary line pointer.
C
      TPTR = IPTR
C
C  Initialize the level counter (level 0 is major class; level 1
C  is subclass of major class.)
C
      LEVEL = 0
C
C   Start search at index 1.
C
      ROWNUM = 1
C
C   Continue search .
C
 10   CONTINUE
      TBIDX = PART3(ROWNUM)
      CHSTRT = PART4(TBIDX)
      DIM1 = PART5((TBIDX*2)-1)
      DIM2 = PART5(TBIDX*2)
      LEVEL = LEVEL + 1
C
C  Loop through all possible entries in the current table.
C
      DO 100 ROW = 1,DIM1
        IEND = DIM2 * ROW + CHSTRT
        IPOS = DIM2 * (ROW-1) + CHSTRT
C
C  Loop from current position in the line searching for a match.
C
        DO 50 II = TPTR, LSIZE
C
C  Continue as long as the characters match a keyword.
C
          IF (LINE(II:II) .NE. PART1(IPOS)) GO TO 100
C
C  Increment the table position pointer.
C
          IPOS = IPOS + 1
C
C  If at end of table entry, then a match has been made.
C
          IF (IPOS.GE.IEND .OR. PART1(IPOS).EQ.BLANK) THEN
C
C  Set the first PTHBTS of WHICH to integer flag for major class; 
C  find subclass.
C
             LEV = (LEVEL-1)*PTHBTS
             CALL SBYTES(WHICH,ROW,LEV,PTHBTS,0,1)
             TPTR = II
             GO TO 1000
           END IF
  50    CONTINUE
C
C  Fall through, there is no match, check next row.
C
 100  CONTINUE
C
C  No match, increment pointer and return.
C
      WHICH(1) = 0
      IPTR = IPTR + 1
      RETURN
C
C  Either a major class was matched on pass 1 (LEV = 0) in which
C  case search for a subclass; or a subclass was found on pass 2
C  (LEV = 1), so terminate.
C
 1000 CONTINUE
C
C  ROWNUM will equal zero on a second pass (the trailing zeros in
C  the PART2 common block serve as pass 2 termination flags).
C
      ROWNUM = PART2(ROWNUM+ROW-1)
      IF (ROWNUM .EQ. 0) THEN
C
C  End of search, check that keyword terminates with a blank.
C
        RPTR = IPTR
        IPTR = TPTR + 1
        IF ( SFFNDC(KEYTER) ) THEN
          RETURN
        ELSE
C
C  Fail, reset pointer to start and clear WHICH.
C
          IPTR = RPTR
          WHICH(1) = 0
          RETURN
        END IF
      END IF
C
C  Check for a seperator.
C
      RPTR = IPTR
      IPTR = TPTR + 1
      IF (SFFNDC(KEYSEP)) THEN
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
