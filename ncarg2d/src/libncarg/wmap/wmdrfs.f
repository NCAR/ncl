C
C	$Id: wmdrfs.f,v 1.4 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMDRFS(X1,Y1,X2,Y2)
C
C  Draw a line of width SLINWD from (X1,Y1) to (X2,Y2).
C
      PARAMETER (NDIM=4)
C
      include 'wmcomn.h'
C
      DIMENSION XL(NDIM),YL(NDIM),REFX(NDIM),REFY(NDIM)
C
C  Save the current fill attributes and reset.
C
      CALL GQFACI(IER,ICOLD)
      CALL GQFAIS(IER,IFOLD)
      CALL GSFACI(ICOLOR)
      CALL GSFAIS(1)
C
C  Set up a reference rectangle.
C
      D  = SQRT((Y2-Y1)**2+(X2-X1)**2)
      HLFW = 0.5*SLINWD
      REFX(1) = 0.
      REFX(2) = D
      REFX(3) = D
      REFX(4) = 0.
      REFY(1) = -HLFW
      REFY(2) = -HLFW
      REFY(3) =  HLFW
      REFY(4) =  HLFW
C
C  Rotate the reference rectangle.
C
      COSANG = (X2-X1)/D
      SINANG = (Y2-Y1)/D
      DO 20 I=1,NDIM
        XL(I) = REFX(I)*COSANG-REFY(I)*SINANG
        YL(I) = REFX(I)*SINANG+REFY(I)*COSANG
   20 CONTINUE
C
C  Translate.
C
      DO 40 I=1,NDIM
        XL(I) = X1+XL(I)
        YL(I) = Y1+YL(I)
   40 CONTINUE
C
      CALL GFA(NDIM,XL,YL)
C
C  Restore fill attributes.
C
      CALL GSFACI(ICOLD)
      CALL GSFAIS(IFOLD)
C
      RETURN
      END
