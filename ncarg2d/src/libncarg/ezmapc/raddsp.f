C
C $Id: raddsp.f,v 1.2 2000-07-12 16:24:03 haley Exp $
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
      SUBROUTINE RADDSP (RAD,SGNA,DEGS,MINS,SECS)
C
C SUBROUTINE TO CONVERT ANGLE FROM RADIANS TO SIGNED DMS
C SGNA : SIGN OF ANGLE
C DEGS : DEGREES PORTION OF ANGLE
C MINS : MINUTES PORTION OF ANGLE
C SECS : SECONDS PORTION OF ANGLE
C
      REAL RAD,CON,RADSEC,ZERO,TOL
      REAL SECS
      INTEGER DEGS,MINS
      CHARACTER*1 SGNA,BLANK,NEG
      DATA RADSEC /206264.806247E0/
      DATA ZERO,TOL /0.0E0,1.0E-4/
      DATA BLANK,NEG /' ','-'/
C
C CONVERT THE ANGLE TO SECONDS.
C
      CON = ABS(RAD) * RADSEC
      ISEC = INT(CON + TOL)
C
C DETERMINE THE SIGN OF THE ANGLE.
C
      SGNA = BLANK
      IF (RAD .LT. ZERO .AND. CON .GE. 0.00005E0) SGNA = NEG
      IF (CON .LT. 0.00005E0) CON = ZERO
C
C COMPUTE DEGREES PART OF THE ANGLE.
C
      INTG = ISEC / 3600
      DEGS = INTG
      ISEC = INTG * 3600
      CON = CON - ISEC
      ISEC = INT(CON + TOL)
C
C COMPUTE MINUTES PART OF THE ANGLE.
C
      MINS = ISEC / 60
      ISEC = MINS * 60
      CON = CON - ISEC
C
C COMPUTE SECONDS PART OF THE ANGLE.
C
      SECS = CON
C
C     INCREASE MINS IF SECS CLOSE TO 60.000
C
      IF(SECS .LT. 59.9995E0) RETURN
      MINS = MINS + 1
      SECS = 0.0
C
C     INCREASE DEGS IF MINS EQUAL 60
C
      IF(MINS .LE. 59) RETURN
      MINS = 0
      DEGS = DEGS + 1
C
      RETURN
      END
