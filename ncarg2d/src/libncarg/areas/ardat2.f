C
C $Id: ardat2.f,v 1.10 2000-07-12 16:21:46 haley Exp $
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
      FUNCTION ARDAT2 (YVL,XVL)
C
C The function ARDAT2, given two double precision values YVL and XVL,
C approximates the value of DATAN2 (YVL,XVL).  Because of the way in
C which ARDAT2 is used, the approximation need not be very accurate;
C what's important is that it should be fast.
C
      DOUBLE PRECISION YVL,XVL
C
      IF (.NOT.(XVL.LT.0.D0.AND.YVL.LE.0.D0)) GO TO 10001
        IF (.NOT.(-XVL.GT.-YVL)) GO TO 10002
          ARDAT2=REAL(-3.14159265358979323846264338328D0+(YVL/XVL)/
     +                                                          (1.D0+.2
     +8D0*(YVL/XVL)*(YVL/XVL)))
        GO TO 10003
10002   CONTINUE
          ARDAT2=REAL(-1.57079632679489661923132169164D0-(XVL/YVL)/
     +                                                          (1.D0+.2
     +8D0*(XVL/YVL)*(XVL/YVL)))
10003   CONTINUE
      GO TO 10004
10001 CONTINUE
      IF (.NOT.(XVL.GE.0.D0.AND.YVL.LT.0.D0)) GO TO 10005
        IF (.NOT.(XVL.LT.-YVL)) GO TO 10006
          ARDAT2=REAL(-1.57079632679489661923132169164D0-(XVL/YVL)/
     +                                                          (1.D0+.2
     +8D0*(XVL/YVL)*(XVL/YVL)))
        GO TO 10007
10006   CONTINUE
          ARDAT2=REAL(        (YVL/XVL)/
     +                               (1.D0+.28D0*(YVL/XVL)*(YVL/XVL)))
10007   CONTINUE
      GO TO 10004
10005 CONTINUE
      IF (.NOT.(XVL.GT.0.D0.AND.YVL.GE.0.D0)) GO TO 10008
        IF (.NOT.(XVL.GT.YVL)) GO TO 10009
          ARDAT2=REAL(        (YVL/XVL)/
     +                               (1.D0+.28D0*(YVL/XVL)*(YVL/XVL)))
        GO TO 10010
10009   CONTINUE
          ARDAT2=REAL( 1.57079632679489661923132169164D0-(XVL/YVL)/
     +                                                          (1.D0+.2
     +8D0*(XVL/YVL)*(XVL/YVL)))
10010   CONTINUE
      GO TO 10004
10008 CONTINUE
      IF (.NOT.(XVL.LE.0.D0.AND.YVL.GT.0.D0)) GO TO 10011
        IF (.NOT.(-XVL.LT.YVL)) GO TO 10012
          ARDAT2=REAL( 1.57079632679489661923132169164D0-(XVL/YVL)/
     +                                                          (1.D0+.2
     +8D0*(XVL/YVL)*(XVL/YVL)))
        GO TO 10013
10012   CONTINUE
          ARDAT2=REAL( 3.14159265358979323846264338328D0+(YVL/XVL)/
     +                                                          (1.D0+.2
     +8D0*(YVL/XVL)*(YVL/XVL)))
10013   CONTINUE
      GO TO 10004
10011 CONTINUE
        ARDAT2=0.
10004 CONTINUE
      RETURN
      END
