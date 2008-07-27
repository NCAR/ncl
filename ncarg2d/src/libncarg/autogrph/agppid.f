C
C $Id: agppid.f,v 1.6 2008-07-27 00:14:35 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGPPID (TPID)
C
      CHARACTER*(*) TPID
C
C The object of this routine is to print out a parameter identifier
C which has caused some kind of problem.
C
C Define a character variable to hold the print line.
C
      CHARACTER*124 TEMP
C
C Set up the print line.
C
      TEMP='0PARAMETER IDENTIFIER - '
C
C Transfer characters of the parameter identifier, one at a time, until
C 100 have been transferred or a period is encountered, whichever occurs
C first.  This is done so as to allow for old programs on the Cray which
C used Hollerith strings as parameter identifiers.
C
      I=24
C
      DO 101 J=1,100
        I=I+1
        TEMP(I:I)=TPID(J:J)
        IF (TEMP(I:I).EQ.'.') GO TO 102
  101 CONTINUE
C
C Print the line.
C
  102 WRITE (I1MACH(4),1001) TEMP
C
C Done.
C
      RETURN
C
C Format.
C
 1001 FORMAT (A124)
C
      END
