C
C	$Id: conlin.f,v 1.3 2000-08-22 15:02:40 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CONLIN(XD,YD,ZD,NT,IWK,WK)
C
C  THIS ROUTINE GENERATES THE COORDINATES USED IN A LINEAR INTERPOLATION
C  OF THE TRIANGLES CREATED FROM IRREGULARLY DISTRIBUTED DATA.
C
C  INPUT
C       XD-X INPUT COORDINATES]
C       YD-Y INPUT COORDINATES
C       ZD-Z VALUE AT INPUT X,Y
C       NT-NUMBER OF TRIANGLES GENERATED
C       IWK-LIST OF TRIANGLE POINTS, RELATIVE TO XD,YD
C               GROUPED 3 PER TRIANGLE I.E. TRIANGLE 1 IWK(1,2,3),
C               TRIANGLE 2 IWK(4,5,6)  ETC.
C
C  OUTPUT
C       WK ARRAY OF COEFICENTS FOR LINEATION FORMUALS
C               GROUPED 3 PER TRIANGLE
C               POINTS ARE (TRI-1)*3 + 1,2,3
C
      DIMENSION IWK(1),WK(1),XD(1),YD(1),ZD(1)
C
        SAVE
C
C   LOOP FOR ALL TRIANGLES
C
      DO 1000 ITRI = 1,NT
C
C       GET THE POINTS OF THE TRIANGLE
C
        IPOINT = (ITRI-1)*3
        IP1 = IWK(IPOINT+1)
        IP2 = IWK(IPOINT+2)
        IP3 = IWK(IPOINT+3)
C
C       GET THE VALUES AT THE TRIANBGLE POINTS
C
        X1 = XD(IP1)
        Y1 = YD(IP1)
        Z1 = ZD(IP1)
        X2 = XD(IP2)
        Y2 = YD(IP2)
        Z2 = ZD(IP2)
        X3 = XD(IP3)
        Y3 = YD(IP3)
        Z3 = ZD(IP3)
C
C  COMPUTE THE INTERPLOATING COEFICIENTS
C
        WK(IPOINT+1) = (Y2-Y1)*(Z3-Z1)-(Y3-Y1)*(Z2-Z1)
        WK(IPOINT+2) = (X3-X1)*(Z2-Z1)-(X2-X1)*(Z3-Z1)
        WK(IPOINT+3) = (X3-X1)*(Y2-Y1)-(X2-X1)*(Y3-Y1)
C
 1000 CONTINUE
C
      RETURN
      END
