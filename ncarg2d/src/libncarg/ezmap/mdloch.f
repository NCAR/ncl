C
C $Id: mdloch.f,v 1.2 2001-09-12 17:29:00 kennison Exp $
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
      SUBROUTINE MDLOCH (RLON,CHRS,NCHR)
C
C This routine, given a longitude RLON and a character buffer CHRS,
C returns CHRS and NCHR such that CHRS(1:NCHR), when written by PLCHHQ,
C will yield the nautical representation of RLON, in degrees, minutes,
C and seconds east or west of the prime meridian at Greenwich.
C
        DOUBLE PRECISION RLON
        CHARACTER*(*)    CHRS
        INTEGER          NCHR
C
C Declare local variables.
C
        DOUBLE PRECISION ALON
        INTEGER          IDEG,IMIN,ISEC,NSEC
C
C Compute the equivalent value of the longitude in the range -180 to
C +180 degrees.
C
        ALON=MOD(MOD(RLON,360.D0)+360.D0,360.D0)
        IF (ALON.GE.180.D0) ALON=ALON-360.D0
C
C Compute the absolute value of the longitude in seconds, limiting it to
C the range from 0 to 180 degrees (0 to 64,800 seconds), and rounding to
C the nearest second.
C
        NSEC=MIN(648000,INT(3600.*ABS(ALON)+.5))
C
C Compute the degree, minute, and second quantities, as integers.
C
        IDEG=NSEC/3600
        IMIN=MOD(NSEC,3600)/60
        ISEC=MOD(NSEC,60)
C
C If the value is zero, return a string that will make PLCHHQ write a
C 0 followed by a degree sign.
C
        IF (IDEG.EQ.0.AND.IMIN.EQ.0.AND.ISEC.EQ.0) THEN
          NCHR=7
          CHRS='0:F34:0'
          RETURN
        END IF
C
C Otherwise, put blanks in the buffer and zero the current character
C count.
C
        CHRS=' '
        NCHR=0
C
C Put the number of degrees in the buffer, followed by the code for a
C degree sign.
C
        CALL MDINCH (IDEG,CHRS,NCHR)
        NCHR=NCHR+9
        CHRS(NCHR-8:NCHR)=':F34:0:F:'
C
C If the number of minutes and seconds are both zero, we're done except
C for, perhaps, a "W" or an "E".
C
        IF (IMIN.EQ.0.AND.ISEC.EQ.0) THEN
          IF (IDEG.EQ.180) RETURN
          GO TO 101
        END IF
C
C Put the number of minutes in the buffer, followed by the code for a
C single quote.
C
        CALL MDINCH (IMIN,CHRS,NCHR)
        NCHR=NCHR+15
        CHRS(NCHR-14:NCHR)=':F29H-5:'':H-4F:'
C
C If the number of seconds is zero, we're done except for, perhaps, a
C "W" or an "E".
C
        IF (ISEC.EQ.0) GO TO 101
C
C Put the number of seconds in the buffer, followed by the code for a
C double quote.
C
        CALL MDINCH (ISEC,CHRS,NCHR)
        NCHR=NCHR+15
        CHRS(NCHR-14:NCHR)=':F29H-5:":FH-4:'
C
C Finally, put either a "W" or an "E" in the buffer.
C
  101   NCHR=NCHR+1
C
        IF (ALON.LT.0.) THEN
          CHRS(NCHR:NCHR)='W'
        ELSE
          CHRS(NCHR:NCHR)='E'
        END IF
C
C Done.
C
        RETURN
C
      END
