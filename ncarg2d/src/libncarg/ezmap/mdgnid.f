C
C $Id: mdgnid.f,v 1.1 2007-01-24 23:42:38 kennison Exp $
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
      SUBROUTINE MDGNID (DANG,DNCE,NOFD)
C
        DOUBLE PRECISION DANG,DNCE
        INTEGER NOFD
C
C This routine, given a particular angular "step" DANG, in degrees,
C returns the largest "nice" value which is smaller than or equal to
C that step (probably to be used as the interval between labels on
C a grid).
C
C Declare arrays in which to put "nice" values in degrees, minutes, and
C seconds, respectively.
C
        DOUBLE PRECISION TDEG(8),TFRA(4)
C
C Declare other local variables.
C
        INTEGER I,J
C
C Define the "nice" values.
C
        DATA TDEG / 90.D0,45.D0,30.D0,15.D0,10.D0, 5.D0, 2.D0,1.D0 /
        DATA TFRA / .50000D0,.25000D0,.20000D0,.10000D0 /
C
C Look for a nice interval which is an integral number of degrees.
C
        DO 101 I=1,8
          IF (DANG.GE.TDEG(I)) THEN
            DNCE=TDEG(I)
            NOFD=0
            RETURN
          END IF
  101   CONTINUE
C
C Look for nice intervals having one or more fractional digits.
C
        DO 103 I=1,4
          DO 102 J=1,4
            IF (DANG.GE.TFRA(J)/10.D0**(I-1)) THEN
              DNCE=TFRA(J)/10.D0**(I-1)
              NOFD=I
              IF (J.EQ.2) NOFD=NOFD+1
              RETURN
            END IF
  102     CONTINUE
  103   CONTINUE
C
C Failed to find a nice interval; return a zero.
C
        DNCE=0.D0
C
C Done.
C
        RETURN
C
      END
