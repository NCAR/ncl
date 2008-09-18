C
C $Id: mdgnin.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDGNIN (DANG,DNCE)
C
        DOUBLE PRECISION DANG,DNCE
C
C This routine, given a particular angular "step" DANG, in degrees,
C returns the largest "nice" value which is smaller than or equal to
C that step (probably to be used as the interval between labels on
C a grid).
C
C Declare arrays in which to put "nice" values in degrees, minutes, and
C seconds, respectively.
C
        DOUBLE PRECISION TDEG(8),TMIN(7),TSEC(7)
C
C Declare other local variables.
C
        INTEGER       I
C
C Define the "nice" values.
C
        DATA TDEG / 90.D0,45.D0,30.D0,15.D0,10.D0, 5.D0, 2.D0,1.D0 /
        DATA TMIN / 30.D0,20.D0,15.D0,10.D0, 5.D0, 2.D0, 1.D0 /
        DATA TSEC / 30.D0,20.D0,15.D0,10.D0, 5.D0, 2.D0, 1.D0 /
C
C Look for a nice interval which is an integral number of degrees.
C
        DO 101 I=1,8
          IF (DANG.GE.TDEG(I)) THEN
            DNCE=TDEG(I)
            RETURN
          END IF
  101   CONTINUE
C
C Look for a nice interval which is an integral number of minutes.
C
        DO 102 I=1,7
          IF (60.D0*DANG.GE.TMIN(I)) THEN
            DNCE=TMIN(I)/60.D0
            RETURN
          END IF
  102   CONTINUE
C
C Look for a nice interval which is an integral number of seconds.
C
        DO 103 I=1,7
          IF (3600.D0*DANG.GE.TSEC(I)) THEN
            DNCE=TSEC(I)/3600.D0
            RETURN
          END IF
  103   CONTINUE
C
C Failed to find a nice interval; return a zero.
C
        DNCE=0.D0
C
C Done.
C
        RETURN
C
      END
