C
C	$Id: gzname.f,v 1.5 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZNAME(FCTID,FNAME)
C
C  This subroutine takes the GKS function id in the integer variable
C  FCTID (as per the codes assigned in the Fortran binding) and returns 
C  the function name in the CHARACTER*6 variable FNAME.  The name in 
C  FNAME is left justiifed and blank padded.
C
      include 'gkscom.h'
C
      CHARACTER*6 FNAME
      INTEGER FCTID
C
      FNAME = GNAM(FCTID+1)
      RETURN
      END
