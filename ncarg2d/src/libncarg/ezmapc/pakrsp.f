C
C $Id: pakrsp.f,v 1.1 1999-04-19 22:10:02 kennison Exp $
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
