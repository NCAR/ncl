C
C       $Id: gerlog.f,v 1.6 2000-07-12 16:39:41 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE GERLOG(ERRNR,FCTID,ERRFIL)
C
C  ERROR LOGGING
C
      include 'gkscom.h'
C
      INTEGER ERRNR,FCTID,ERRFIL
      CHARACTER*90 REMSG
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
      DO 30 J=90,1,-1
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
