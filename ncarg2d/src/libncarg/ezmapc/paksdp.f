C
C $Id: paksdp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION PAKSDP (ANG)
C
C FUNCTION TO CONVERT DMS PACKED ANGLE INTO SECONDS OF ARC.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER I
      DIMENSION CODE(2)
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      DATA CODE /1000000.0D0,1000.0D0/
      DATA ZERO,ONE /0.0D0,1.0D0/
      DATA C1,C2 /3600.0D0,60.0D0/
      DATA TOL /1.0D-4/
C
C SEPARATE DEGREE FIELD.
C
      FACTOR = ONE
      IF (ANG .LT. ZERO) FACTOR = - ONE
      SEC = ABS(ANG)
      TMP = CODE(1)
      I = INT ((SEC / TMP) + TOL)
      IF (I .GT. 360) GO TO 020
      DEG = I
C
C SEPARATE MINUTES FIELD.
C
      SEC = SEC - DEG * TMP
      TMP = CODE(2)
      I = INT ((SEC / TMP) + TOL)
      IF (I .GT. 60) GO TO 020
      MIN = I
C
C SEPARATE SECONDS FIELD.
C
      SEC = SEC - MIN * TMP
      IF (SEC .GT. C2) GO TO 020
      SEC = FACTOR * (DEG * C1 + MIN * C2 + SEC)
      GO TO 040
C
C ERROR DETECTED IN DMS FORM.
C
  020 WRITE (IPELUN,2000) ANG
 2000 FORMAT (/' ERROR PAKSDP'/
     .         ' ILLEGAL DMS FIELD =',F15.3)
      STOP 16
C
  040 PAKSDP = SEC
C
      RETURN
      END
