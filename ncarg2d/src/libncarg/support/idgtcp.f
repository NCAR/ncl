C
C $Id: idgtcp.f,v 1.4 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDGTCP (X1,Y1,X2,Y2,X3,Y3,XC,YC)
C
C This subroutine, given the coordinates of three points in the plane -
C (X1,Y1), (X2,Y2), and (X3,Y3) - returns the coordinates of the center
C of the circle passing through those three points: (XC,YC).
C
        A=2.*(X1-X2)
        B=2.*(Y1-Y2)
        C=X1*X1-X2*X2+Y1*Y1-Y2*Y2
        D=2.*(X1-X3)
        E=2.*(Y1-Y3)
        F=X1*X1-X3*X3+Y1*Y1-Y3*Y3
        XC=(C*E-B*F)/(A*E-B*D)
        YC=(C*D-A*F)/(B*D-A*E)
C
        RETURN
C
      END
