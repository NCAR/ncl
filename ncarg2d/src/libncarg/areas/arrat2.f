C
C $Id: arrat2.f,v 1.2 1993-06-03 22:44:39 kennison Exp $
C
      FUNCTION ARRAT2 (YVL,XVL)
C
C The function ARRAT2, given two real values YVL and XVL, approximates
C the value of ATAN2 (YVL,XVL).  Because of the way in which ARRAT2 is
C used, the approximation need not be very accurate.
C
      IF (.NOT.(XVL.LT.0..AND.YVL.LE.0.)) GO TO 10001
        IF (.NOT.(-XVL.GT.-YVL)) GO TO 10002
          ARRAT2=-3.14159265358979+(YVL/XVL)/(1.+.28*(YVL/XVL)*(YVL/XVL)
     +)
        GO TO 10003
10002   CONTINUE
          ARRAT2=-1.57079632679490-(XVL/YVL)/(1.+.28*(XVL/YVL)*(XVL/YVL)
     +)
10003   CONTINUE
      GO TO 10004
10001 CONTINUE
      IF (.NOT.(XVL.GE.0..AND.YVL.LT.0.)) GO TO 10005
        IF (.NOT.(XVL.LT.-YVL)) GO TO 10006
          ARRAT2=-1.57079632679490-(XVL/YVL)/(1.+.28*(XVL/YVL)*(XVL/YVL)
     +)
        GO TO 10007
10006   CONTINUE
          ARRAT2=      (YVL/XVL)/(1.+.28*(YVL/XVL)*(YVL/XVL))
10007   CONTINUE
      GO TO 10004
10005 CONTINUE
      IF (.NOT.(XVL.GT.0..AND.YVL.GE.0.)) GO TO 10008
        IF (.NOT.(XVL.GT.YVL)) GO TO 10009
          ARRAT2=      (YVL/XVL)/(1.+.28*(YVL/XVL)*(YVL/XVL))
        GO TO 10010
10009   CONTINUE
          ARRAT2= 1.57079632679490-(XVL/YVL)/(1.+.28*(XVL/YVL)*(XVL/YVL)
     +)
10010   CONTINUE
      GO TO 10004
10008 CONTINUE
      IF (.NOT.(XVL.LE.0..AND.YVL.GT.0.)) GO TO 10011
        IF (.NOT.(-XVL.LT.YVL)) GO TO 10012
          ARRAT2= 1.57079632679490-(XVL/YVL)/(1.+.28*(XVL/YVL)*(XVL/YVL)
     +)
        GO TO 10013
10012   CONTINUE
          ARRAT2= 3.14159265358979+(YVL/XVL)/(1.+.28*(YVL/XVL)*(YVL/XVL)
     +)
10013   CONTINUE
      GO TO 10004
10011 CONTINUE
        ARRAT2=0.
10004 CONTINUE
      RETURN
      END
