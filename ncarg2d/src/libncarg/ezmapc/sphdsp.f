C
C $Id: sphdsp.f,v 1.6 2008-07-27 00:17:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SPHDSP (ISPH,PARM)
C
C Subroutine to compute spheroid parameters.  ISPH is a spheroid code,
C as follows:
C
C   0 = Clarke 1866             10 = Modified Everest
C   1 = Clarke 1880             11 = Modified Airy
C   2 = Bessel                  12 = WGS 84
C   3 = New International 1967  13 = Southeast Asia
C   4 = International 1909      14 = Australian National
C   5 = WGS 72                  15 = Krassovsky
C   6 = Everest                 16 = Hough
C   7 = WGS 66                  17 = Mercury 1960
C   8 = GRS 1980                18 = Modified Mercury 1968
C   9 = Airy                    19 = Sphere of radius 6,370,997 meters
C
C PARM is the array of projection parameters; in particular,
C
C   PARM(1) is the semi-major axis
C   PARM(2) is the eccentricity squared
C
C If ISPH is negative, user-specified values of these must be supplied;
C otherwise, the values are returned.
C
      IMPLICIT REAL (A-Z)
      INTEGER ISPH
      DIMENSION PARM(15),AXIS(20),BXIS(20)
C
      COMMON /ELLPSP/ AZ,EZ,ESZ,E0Z,E1Z,E2Z,E3Z,E4Z
      SAVE   /ELLPSP/
C
      COMMON /SPHRSP/ AZZ
      SAVE   /SPHRSP/
C
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
C
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
C
      DATA ZERO,ONE,TEN / 0.E0 , 1.E0 , 1.E1 /
C
      DATA AXIS / 6378206.400000E0,6378249.145000E0,6377397.155000E0,
     .            6378157.500000E0,6378388.000000E0,6378135.000000E0,
     .            6377276.345200E0,6378145.000000E0,6378137.000000E0,
     .            6377563.396000E0,6377304.063000E0,6377340.189000E0,
     .            6378137.000000E0,6378155.000000E0,6378160.000000E0,
     .            6378245.000000E0,6378270.000000E0,6378166.000000E0,
     .            6378150.000000E0,6370997.000000E0 /
C
      DATA BXIS / 6356583.800000E0,6356514.869550E0,6356078.962840E0,
     .            6356772.200000E0,6356911.946130E0,6356750.519915E0,
     .            6356075.413300E0,6356759.769356E0,6356752.314140E0,
     .            6356256.910000E0,6356103.039000E0,6356034.448000E0,
     .            6356752.314245E0,6356773.320500E0,6356774.719000E0,
     .            6356863.018800E0,6356794.343479E0,6356784.283666E0,
     .            6356768.337303E0,6370997.000000E0 /
C
C Compute A and B (the lengths of the semi-major and semi-minor axes),
C and ES (the square of the eccentricity).  Note that, if one sets ISPH
C less than zero, one can specify, in PARM(1) and PARM(2), "a" and "0"
C (implying a sphere of radius "a"), "a" and "b" (implying an ellipsoid
C with semi-major and semi-minor axes of lengths "a" and "b", or "a" and
C "e**2" (implying an ellipsoid with a semi-major axis of length "a" and
C eccentricity squared "e**2"); in any of these cases, the numbers can
C be given in either order.  (It is assumed, however, that "a" is not
C less than 1 and that "e**2" is not greater than "a/10".
C
      IF (ISPH.LT.0) THEN
        A=MAX(ABS(PARM(1)),ABS(PARM(2)))
        B=MIN(ABS(PARM(1)),ABS(PARM(2)))
        ES=ONE-(B/A)**2
        IF (A.EQ.ZERO) THEN
          A=AXIS(1)
          B=BXIS(1)
          ES=ONE-(B/A)**2
        ELSE IF (B.EQ.ZERO) then
          B=A
          ES=ZERO
        ELSE IF (B.LT.A/TEN) THEN
          ES=B
          B=A*SQRT(ONE-ES)
        END IF
      ELSE
        A=AXIS(MAX(1,MIN(20,ISPH+1)))
        B=BXIS(MAX(1,MIN(20,ISPH+1)))
        ES=ONE-(B/A)**2
      END IF
C
C Transfer the appropriate values to common blocks.
C
      AZ=A
      AZZ=A
      ESZ=ES
      EZ=SQRT(ES)
      E0Z=E0FNSP(ES)
      E1Z=E1FNSP(ES)
      E2Z=E2FNSP(ES)
      E3Z=E3FNSP(ES)
      E4Z=E4FNSP(EZ)
C
C Pass the length of the semi-major axis and the square of the
C eccentricity back to the caller.
C
      PARM(1)=A
      PARM(2)=ES
C
C Done.
C
      RETURN
C
      END
