C
C	$Id: sffndc.f,v 1.1 1997-01-24 21:59:54 haley Exp $
C
      LOGICAL FUNCTION SFFNDC (CHAR)
C
C  Test the current position in LINE for the presence of CHAR.
C  If a match, then set SFFNDC to .TRUE. .  If no match, then set
C  SFFNDC to .FALSE. .
C
C  This routine assumes that no reloading of the line is necessary.
C
C  Note that a match is found if CHAR is BLANK and at END-OF-LINE.
C
C  OUTPUT
C	SFFNDC - Logical value indicating the presence of CHAR
C
C  COMMON variable IPTR -  If CHAR is found, then IPTR will point to
C                          the next position in the line;  if CHAR
C                          is not found,  then IPTR is unchanged.
C
      include 'fntcom.h'
      include 'fnttab.h'
C
      CHARACTER*1 CHAR
C
      CHARACTER*1 BLANK
      DATA BLANK/' '/
C
C  If END-OF-LINE and CHAR is BLANK, then set match.
C
      IF (IPTR.GT.LSIZE .AND. CHAR.EQ.BLANK) THEN
	SFFNDC = .TRUE.
        IPTR = IPTR + 1
        RETURN
      END IF
C
C  Check for a match.
C
      IF (LINE(IPTR:IPTR) .EQ. CHAR) THEN
	SFFNDC = .TRUE.
	IPTR = IPTR + 1
	RETURN
      END IF
C
C  Failure, so set SFFNDC to .FALSE. .
C
      SFFNDC = .FALSE.
      RETURN
      END
