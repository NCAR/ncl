C
C $Id: pakdsp.f,v 1.4 2001-08-16 23:12:50 kennison Exp $
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
      SUBROUTINE PAKDSP (PAK,SGNA,DEGS,MINS,SECS)
C
C SUBROUTINE TO CONVERT PACKED DMS TO UNPACKED DMS ANGLE.
C
C SGNA : SIGN OF ANGLE
C DEGS : DEGREES PORTION OF ANGLE
C MINS : MINUTES PORTION OF ANGLE
C SECS : SECONDS PORTION OF ANGLE
C
      IMPLICIT REAL (A-Z)
      REAL SECS
      INTEGER DEGS,MINS
      CHARACTER*1 SGNA,IBLANK,NEG
      DATA ZERO,CON1,CON2 /0.0E0,1000000.0E0,1000.0E0/
      DATA TOL /1.0E-4/
      DATA IBLANK,NEG /' ','-'/
C
      SGNA = IBLANK
      IF (PAK .LT. ZERO) SGNA = NEG
      CON = ABS (PAK)
      DEGS = INT ((CON / CON1) + TOL)
      CON = MOD ( CON , CON1)
      MINS = INT ((CON / CON2) + TOL)
      SECS = MOD (CON , CON2)
      RETURN
C
      END
