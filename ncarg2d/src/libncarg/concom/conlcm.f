C
C	$Id: conlcm.f,v 1.3 2000-08-22 15:02:40 haley Exp $
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
      FUNCTION CONLCM(X,Y,XD,YD,ZD,NDP,WK,IWK,LOC)
C
C  COMPUTE A Z VALUE FOR A GIVEN X,Y VALUE
C  NOTE THAT X,Y MUST BE INSIDE THE CONVEX HULL OF THE INPUT DATA
C  INORDER FOR THIS FUNCTION TO WORK.
C
C  INPUT
C       X-X COORDINATE OF REQUESTED POINT
C       Y-Y COORDINATE OF REQUESTED POINT
C       WK-LIST OF COEFICENTS FOR LINEAR INTERPOLATION  FUNCTIONS
C             LOCATED BY A = WK((TRI-1)*3+1)
C                        B = WK((TRI-2)*3+1)
C                        C = WK((TRI-3)*3+1)
C
C  OUTPUT
C       LOC-TRIANGLE NUMBER OF REQUESTED POINT
C       Z VALUE AS FUNCTION RESULT
C
      DIMENSION WK(1),IWK(1),XD(1),YD(1),ZD(1)
C
      COMMON /CONR10/ NT         ,NL         ,NTNL       ,JWIPT      ,
     1                JWIWL      ,JWIWP      ,JWIPL      ,IPR        ,
     2                ITPV
C
        SAVE
C
C  LOCATE THE TRIANGLE
C
      CALL CONLOC(NDP,XD,YD,NT,IWK(JWIPT),NL,IWK(JWIPL),X,Y,LOC,
     1            IWK(JWIWL),WK)
C
C  IF OUTSIDE CONVEX HULL THEN DON'T COMPUTE A VALUE
C
      IF (LOC.GT.NT) RETURN
C
C  GET THE VECTOR 1 VALUES FOR THE TRIANGLE
C
      IVEC = (LOC-1)*3 + JWIPT
      IV = IWK(IVEC)
      X1 = X - XD(IV)
      Y1 = Y - YD(IV)
      Z1 = ZD(IV)
C
C  COMPUT THE Z VALUE
C
      IPOINT = (LOC-1)*3 + IPR
C
      Z = (WK(IPOINT)*X1+WK(IPOINT+1)*Y1)/WK(IPOINT+2) + Z1
C
      CONLCM = Z
C
      RETURN
      END
