C
C	$Id: conlcm.f,v 1.4 2008-07-27 00:16:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
