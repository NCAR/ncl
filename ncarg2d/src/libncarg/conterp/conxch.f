C
C	$Id: conxch.f,v 1.5 2008-07-27 00:16:57 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION CONXCH (X,Y,I1,I2,I3,I4)
C
C THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO
C TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION
C BY C. L. LAWSON.
C THE INPUT PARAMETERS ARE
C     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA
C           POINTS,
C     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2,
C                   P3, AND P4 THAT FORM A QUADRILATERAL
C                   WITH P3 AND P4 CONNECTED DIADONALLY.
C THIS FUNCTION RETURNS A VALUE 1 (ONE) WHEN AN EXCHANGE IS
C NEEDED, AND 0 (ZERO) OTHERWISE.
C DECLARATION STATEMENTS
C
      DIMENSION       X(1)       ,Y(1)
      DIMENSION       X0(4)      ,Y0(4)
      EQUIVALENCE     (C2SQ,C1SQ),(A3SQ,B2SQ),(B3SQ,A1SQ),(A4SQ,B1SQ),
     1                (B4SQ,A2SQ),(C4SQ,C3SQ)
C
        SAVE
C
C STATEMENT FUNCTIONS
C
C CALCULATION
C
      X0(1) = X(I1)
      Y0(1) = Y(I1)
      X0(2) = X(I2)
      Y0(2) = Y(I2)
      X0(3) = X(I3)
      Y0(3) = Y(I3)
      X0(4) = X(I4)
      Y0(4) = Y(I4)
      IDX = 0
      U3 = (Y0(2)-Y0(3))*(X0(1)-X0(3))-(X0(2)-X0(3))*(Y0(1)-Y0(3))
      U4 = (Y0(1)-Y0(4))*(X0(2)-X0(4))-(X0(1)-X0(4))*(Y0(2)-Y0(4))
      IF (U3*U4 .LE. 0.0) GO TO  100
      U1 = (Y0(3)-Y0(1))*(X0(4)-X0(1))-(X0(3)-X0(1))*(Y0(4)-Y0(1))
      U2 = (Y0(4)-Y0(2))*(X0(3)-X0(2))-(X0(4)-X0(2))*(Y0(3)-Y0(2))
      A1SQ = (X0(1)-X0(3))**2+(Y0(1)-Y0(3))**2
      B1SQ = (X0(4)-X0(1))**2+(Y0(4)-Y0(1))**2
      C1SQ = (X0(3)-X0(4))**2+(Y0(3)-Y0(4))**2
      A2SQ = (X0(2)-X0(4))**2+(Y0(2)-Y0(4))**2
      B2SQ = (X0(3)-X0(2))**2+(Y0(3)-Y0(2))**2
      C3SQ = (X0(2)-X0(1))**2+(Y0(2)-Y0(1))**2
      S1SQ = U1*U1/(C1SQ*MAX(A1SQ,B1SQ))
      S2SQ = U2*U2/(C2SQ*MAX(A2SQ,B2SQ))
      S3SQ = U3*U3/(C3SQ*MAX(A3SQ,B3SQ))
      S4SQ = U4*U4/(C4SQ*MAX(A4SQ,B4SQ))
      IF (MIN(S1SQ,S2SQ) .LT. MIN(S3SQ,S4SQ)) IDX = 1
  100 CONXCH = IDX
      RETURN
C
      END
