C
C $Id: idgtcp.f,v 1.2 2000-07-12 16:26:18 haley Exp $
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
