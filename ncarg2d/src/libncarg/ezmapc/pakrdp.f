C
C $Id: pakrdp.f,v 1.5 2008-07-27 00:17:09 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      DOUBLE PRECISION FUNCTION PAKRDP (ANG)
C
C Function to convert DMS packed angle into radians.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA SECRAD /0.4848136811095359D-5/
C
C Convert angle to seconds of arc.
C
      SEC = PAKSDP (ANG)
C
C Convert angle to radians.
C
      PAKRDP = SEC * SECRAD
C
      RETURN
      END
