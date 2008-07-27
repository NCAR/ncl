C
C $Id: gzgte2.f,v 1.5 2008-07-27 00:21:03 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZGTE2(NUM,ERMSG)
C
C  Given an NCAR GKS error number in NUM, this subroutine
C  returns the NCAR GKS error message string in ERMSG (the 
C  length of ERMSG will be a maximum of 210 characters). If
C  no error message is found for the given error number, the
C  string ' --UNKNOWN ERROR CODE' is returned in ERMSG.
C
      CHARACTER*(*) ERMSG
C
      include 'gkscom.h'
C
      INTEGER GZNUME
C
C  Determine the index into the error message array.
C
      IGKNER = GZNUME()
      DO 20 I=1,IGKNER
        IF (NUM .EQ. IERNMS(I)) THEN
          NDXER = I
          GO TO 50
        ENDIF
   20 CONTINUE
C
C  Error number is not known.
C
      NDXER = 79
   50 CONTINUE
      ERMSG = ERMSGS(NDXER)
C
      RETURN
      END
