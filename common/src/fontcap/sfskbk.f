C
C	$Id: sfskbk.f,v 1.1 1997-01-24 21:59:56 haley Exp $
C
      SUBROUTINE SFSKBK(IOS, STATUS)
C
C  Set COMMON variable IPTR to point to the next non-blank
C  character in the file, the current line of which is stored
C  in character array LINE.
C
C  OUTPUT
C       IOS    - The I/O status, this value has meaning only if an 
C                error condition obtains (i.e. only if STATUS .GT. 0).
C       STATUS - Status indicator, see subroutine SFPRCF for details.
C
      include 'fnttab.h'
      include 'fnterr.h'
      include 'fntcom.h'
C
      INTEGER IOS, STATUS
      CHARACTER*1 BLANK
      INTEGER II
C
      DATA BLANK/' '/
C
C  Scan the current line for a non-blank.
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
C  End of current line and no no non-blank has been found,
C  get another line.
C
      CALL CFRDLN (ID1,ID2,STATUS)
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
