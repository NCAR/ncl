C
C $Id: pakrsp.f,v 1.5 2008-07-27 00:17:10 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION PAKRSP (ANG)
C
C Function to convert DMS packed angle into radians.
C
      IMPLICIT REAL (A-Z)
      DATA SECRAD /0.4848136811095359E-5/
C
C Convert angle to seconds of arc.
C
      SEC = PAKSSP (ANG)
C
C Convert angle to radians.
C
      PAKRSP = SEC * SECRAD
C
      RETURN
      END
