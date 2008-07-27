C
C	$Id: gerhnd.f,v 1.9 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
      IF (MXERMG .LE. 0) THEN
        WRITE(6,500) 
  500   FORMAT(' '/
     +         '    GKS load error: '/
     +         '       The flag specifying the maximum number '/
     +         '       of error messages is less than or equal to '/
     +         '       zero.  This usually means that blockdata '/
     +         '       has not been loaded and indicates an error '/
     +         '       with your loader.  Contact your vendor, or '/
     +         '       try using g77.'/
     +         ' ')
        STOP
      ENDIF
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
