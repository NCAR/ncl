C
C $Id: pj19z0.f,v 1.1 1999-04-02 23:06:02 kennison Exp $
C
      SUBROUTINE PJ19Z0 (COORD,CRDIO,INDIC)
C
C -- V A N   D E R   G R I N T E N   I
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER IERROR,IPEMSG,IPELUN,IPPARM,IPPLUN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2)
C **** PARAMETERS **** A,LON0,X0,Y0 ************************************
      COMMON /ERRMZ0/ IERROR
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
      COMMON /PJ19/ A,LON0,X0,Y0
      DATA PI /3.14159265358979323846D0/
      DATA HALFPI /1.5707963267948966D0/
      DATA EPSLN/1.0D-10/
      DATA ZERO,HALF,ONE,TWO,THREE/0.0D0,0.5D0,1.0D0,2.0D0,3.0D0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERROR = 0
         LON = ADJLZ0 (GEOG(1) - LON0)
         LAT = GEOG(2)
         IF (ABS(LAT) .GT. EPSLN) GO TO 140
         PROJ(1) = X0 + A * LON
         PROJ(2) = Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  140    THETA = ASINZ0 (MIN(ABS (LAT /HALFPI),ONE))
         IF (ABS(LON).GT.EPSLN.AND.ABS(ABS(LAT)-HALFPI).GT.EPSLN)
     .       GO TO 160
         PROJ(1) = X0
         PROJ(2) = Y0 + PI * A * SIGN( TAN (HALF * THETA), LAT)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
  160    AL = HALF * ABS (PI / LON - LON / PI)
         ASQ = AL * AL
         SINTHT = SIN (THETA)
         COSTHT = COS (THETA)
         G = COSTHT / (SINTHT + COSTHT - ONE)
         GSQ = G * G
         M = G * (TWO / SINTHT - ONE)
         MSQ = M * M
         CON = PI * A * (AL * (G - MSQ) + SQRT (ASQ * (G - MSQ)**2 -
     .         (MSQ + ASQ) * (GSQ - MSQ))) / (MSQ + ASQ)
         CON = SIGN (CON , LON)
         PROJ(1) = X0 + CON
         CON = ABS (CON / (PI * A))
         PROJ(2)=Y0+SIGN(PI*A*SQRT(MAX(0.D0,ONE-CON*CON
     .                                                -TWO*AL*CON)),LAT)
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C -- I N V E R S E   . . .
C
C ---------------------------------------------------------------------
C     ALGORITHM DEVELOPED BY D.P. RUBINCAM, THE AMERICAN CARTOGRAPHER,
C                1981, V. 8, NO. 2, P. 177-180.
C ---------------------------------------------------------------------
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERROR = 0
         X = PROJ(1) - X0
         Y = PROJ(2) - Y0
         CON = PI * A
         XX = X / CON
         YY = Y / CON
         XYS = XX * XX + YY * YY
         C1 = -ABS(YY) * (ONE + XYS)
         C2 = C1 - TWO * YY * YY + XX * XX
         C3 = -TWO * C1 + ONE + TWO * YY * YY + XYS*XYS
         D = YY * YY / C3 + (TWO * C2 * C2 * C2/ C3/ C3/ C3 - 9.0D0 * C1
     .       * C2/ C3/ C3) / 27.0D0
         A1 = (C1 - C2 * C2/ THREE/ C3)/ C3
         M1 = TWO * SQRT(-A1/ THREE)
         CON = ((THREE * D) / A1) / M1
         IF (ABS(CON).GT.ONE) CON = SIGN(ONE,CON)
         TH1 = ACOS(CON)/THREE
         GEOG(2) = (-M1 * COS(TH1 + PI/ THREE) - C2/ THREE/ C3)
     .   * SIGN(PI,Y)
         IF (ABS(XX).GE.EPSLN) GO TO 230
         GEOG(1) = LON0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  230    CONTINUE
         GEOG(1) = LON0 + PI * (XYS - ONE + SQRT(ONE + TWO * (XX * XX
     .      - YY * YY) + XYS * XYS))/ TWO/ XX
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
