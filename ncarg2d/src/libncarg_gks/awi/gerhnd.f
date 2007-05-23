C
C	$Id: gerhnd.f,v 1.7 2007-05-23 00:35:31 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
      print *,'Got to gerhnd',mxermg
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
