C
C $Id: pakrdp.f,v 1.1 1999-04-19 22:10:02 kennison Exp $
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
