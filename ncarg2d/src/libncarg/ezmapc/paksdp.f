C
C $Id: paksdp.f,v 1.2 2000-07-12 16:23:52 haley Exp $
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
