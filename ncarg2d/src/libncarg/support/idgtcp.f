C
C $Id: idgtcp.f,v 1.1 1995-11-03 23:45:22 kennison Exp $
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
