C
C	$Id: gerhnd.f,v 1.4 1994-05-28 00:42:18 fred Exp $
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
      DATA MNERR/0/
C
      MNERR = MNERR+1
      IF (MNERR .GT. MXERMG) THEN
        CALL GERLOG(-107,FCTID,ERRFIL)
        STOP
      ENDIF
      ENUM  = ERRNR
      FNAME = GNAM(FCTID+1)
      CALL GERLOG(ERRNR,FCTID,ERRFIL)
C
      RETURN
      END
