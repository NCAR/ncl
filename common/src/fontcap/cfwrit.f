C
C	$Id: cfwrit.f,v 1.1 1997-01-24 21:59:50 haley Exp $
C
      SUBROUTINE CFWRIT(IUNIT,ITYP,IOS,STATUS)
C  
C  Write the binary fontcap file.
C
C  INPUT
C	UNIT - The fortran logical unit number to be written to.
C       ITYP - = 0  for stroked font
C              = 1  for filled font
C
C  OUTPUT
C	IOS    - The I/O status word, IOS has meaning only if STATUS indicates
C                an error condition (i.e. if STATUS.GT.0).
C	STATUS - Integer-valued status indicator, see subroutine SFPRCF
C                for details.
C
      include 'fnterr.h'
      include 'capfnt.h'
      include 'fntcom.h'
C
      INTEGER IUNIT, IOS, STATUS
C
C  Set error STATUS to ALLOK
C
      STATUS = ALLOK
C
C  Write out the font description.
C
      IF (ITYP .EQ. 0) THEN
        CALL BINWRI(IUNIT, WRLEN, CHSTRT, IOS, STATUS)
      ELSE IF (ITYP .EQ. 1) THEN
        CALL BINWRI(IUNIT, NWRDS, BUFFER, IOS, STATUS)
      ENDIF
C
      RETURN
      END
