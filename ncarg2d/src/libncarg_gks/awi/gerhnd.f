C
C	$Id: gerhnd.f,v 1.2 1993-01-09 01:58:23 fred Exp $
C
      SUBROUTINE GERHND(ERRNR,FCTID,ERRFIL)
C
C  ERROR HANDLING
C
      INTEGER ERRNR,FCTID,ERRFIL
C
      include 'gkscom.h'
C
C  Special common blocks containing current error number
C  and file identifier.
C
      COMMON /GKERR1/ ENUM
      COMMON /GKERR2/ FNAME
      INTEGER ENUM
      CHARACTER*6 FNAME
C
C  Record number of error message and maximum number of allowable
C  errors before abort.
C
      DATA MNERR,MAXERR/0,10/
C
      IF (CUFLAG.EQ.-1 .OR. ERRNR.NE.-109) MNERR = MNERR+1
      IF (MNERR .GT. MAXERR) THEN
        CALL GERLOG(-107,FCTID,ERRFIL)
        STOP
      ENDIF
      ENUM  = ERRNR
      FNAME = GNAM(FCTID+1)
      CALL GERLOG(ERRNR,FCTID,ERRFIL)
C
      RETURN
      END
