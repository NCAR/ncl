C
C $Id: pakrz0.f,v 1.1 1999-04-02 23:05:53 kennison Exp $
C
      DOUBLE PRECISION FUNCTION PAKRZ0 (ANG)
C
C Function to convert DMS packed angle into radians.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA SECRAD /0.4848136811095359D-5/
C
C Convert angle to seconds of arc.
C
      SEC = PAKSZ0 (ANG)
C
C Convert angle to radians.
C
      PAKRZ0 = SEC * SECRAD
C
      RETURN
      END
