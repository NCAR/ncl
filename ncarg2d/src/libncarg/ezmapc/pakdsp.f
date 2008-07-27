C
C $Id: pakdsp.f,v 1.5 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
