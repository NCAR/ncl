C
C $Id: agppid.f,v 1.5 2006-03-09 22:56:07 kennison Exp $
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
