C
C $Id: dmspdp.f,v 1.5 2008-07-27 00:17:08 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION DMSPDP (SGNA,DEGS,MINS,SECS)
C
C FUNCTION TO CONVERT UNPACKED DMS TO PACKED DMS ANGLE
C SGNA : SIGN OF ANGLE
C DEGS : DEGREES PORTION OF ANGLE
C MINS : MINUTES PORTION OF ANGLE
C SECS : SECONDS PORTION OF ANGLE
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      REAL SECS
      INTEGER DEGS,MINS
      CHARACTER*1 SGNA,NEG
      DATA CON1,CON2 /1000000.0D0,1000.0D0/
      DATA NEG /'-'/
C
      CON = DEGS * CON1 + MINS * CON2 + SECS
      IF (SGNA .EQ. NEG) CON = - CON
      DMSPDP = CON
      RETURN
C
      END
