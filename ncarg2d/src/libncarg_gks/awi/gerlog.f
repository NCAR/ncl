C
C       $Id: gerlog.f,v 1.9 2008-07-27 00:20:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GERLOG(ERRNR,FCTID,ERRFIL)
C
C  ERROR LOGGING
C
      include 'gkscom.h'
C
      INTEGER ERRNR,FCTID,ERRFIL
      CHARACTER*210 REMSG
      CHARACTER*6  IFMT
C
      SAVE
C
C  Write the first line of the GKS error message to the error file.
C
      WRITE(ERRFIL,700) ERRNR,GNAM(FCTID+1)
  700 FORMAT(' GKS ERROR NUMBER',I5,' ISSUED FROM SUBROUTINE ',A6,':')
C
C  Retrieve the error message string.
C
      CALL GZGTE2(ERRNR,REMSG)
C
C  Get the length of the error message.
C
      DO 30 J=210,1,-1
        IF (REMSG(J:J) .NE. ' ') THEN
          LENMSG = J
          GO TO 40
        ENDIF
   30 CONTINUE
      LENMSG = 1
   40 CONTINUE
C
C  Write out the error message.
C
      WRITE(IFMT,500) LENMSG
  500 FORMAT('(A',I3,')')
      WRITE(ERRFIL,IFMT) REMSG(1:LENMSG)
C
      RETURN
      END
