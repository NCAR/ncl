C
C $Id: pj23sp.f,v 1.5 2008-07-27 00:17:12 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PJ23SP (COORD,CRDIO,INDIC)
C
C -- M O D I F I E D   S T E R E O G R A P H I C   -   A L A S K A
C
      IMPLICIT REAL (A-Z)
      INTEGER N,J,NN
      INTEGER INDIC
      DIMENSION GEOG(2),PROJ(2),COORD(2),CRDIO(2),
     . ACOEF(6),BCOEF(6)
C **** PARAMETERS **** A,E,ES,LON0,LAT0,X0,Y0,SINPH0,COSPH0 ************
      COMMON /ERRMZ0/ IERR
        INTEGER IERR
      SAVE   /ERRMZ0/
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      COMMON /PC23SP/ A,LON0,X0,Y0,ACOEF,BCOEF,EC,LAT0,CCHIO,SCHIO,N
      DATA HALFPI /1.5707963267948966E0/
      DATA EPSLN /1.0E-10/
      DATA ZERO,ONE,TWO /0.0E0,1.0E0,2.0E0/
C
C -- F O R W A R D   . . .
C
      IF (INDIC .EQ. 0) THEN
C
         GEOG(1) = COORD(1)
         GEOG(2) = COORD(2)
         IERR = 0
         IF (MDDASP(GEOG).GT.30.E0) THEN
           IF (IPEMSG.EQ.0) WRITE (IPELUN,2020)
 2020      FORMAT (/' ERROR PJ23SP'/
     .              ' POINT IS TOO FAR FROM CENTER OF PROJECTION')
           IERR = 233
           RETURN
         END IF
         LON = ADJLSP (GEOG(1) - LON0)
C
C     CALCULATE X-PRIME AND Y-PRIME FOR OBLIQUE STEREOGRAPHIC PROJ.
C          FROM LAT/LONG.
C
         SINLON = SIN (LON)
         COSLON = COS (LON)
         ESPHI = EC *SIN(GEOG(2))
         CHI=TWO*ATAN(TAN((HALFPI+GEOG(2))/TWO)*((ONE-ESPHI)/(ONE
     .      +ESPHI))**(EC/TWO)) - HALFPI
         SCHI=SIN(CHI)
         CCHI=COS(CHI)
         G=SCHIO*SCHI+CCHIO*CCHI*COSLON
         S=TWO/(ONE+G)
         XP=S*CCHI*SINLON
         YP=S*(CCHIO*SCHI-SCHIO*CCHI*COSLON)
C
C     USE KNUTH ALGORITHM FOR SUMMING COMPLEX TERMS, TO CONVERT
C     OBLIQUE STEREOGRAPHIC TO MODIFIED-STEREOGRAPHIC COORD.
C
         R=XP+XP
         S=XP*XP+YP*YP
         AR=ACOEF(N)
         AI=BCOEF(N)
         BR=ACOEF(N-1)
         BI=BCOEF(N-1)
         DO 140 J=2,N
         ARN=BR+R*AR
         AIN=BI+R*AI
         IF (J.EQ.N) GO TO 140
         BR=ACOEF(N-J)-S*AR
         BI=BCOEF(N-J)-S*AI
         AR=ARN
         AI=AIN
  140    CONTINUE
         BR=-S*AR
         BI=-S*AI
         AR=ARN
         AI=AIN
         X=XP*AR-YP*AI+BR
         Y=YP*AR+XP*AI+BI
         PROJ(1)=X*A+X0
         PROJ(2)=Y*A+Y0
         CRDIO(1) = PROJ(1)
         CRDIO(2) = PROJ(2)
         RETURN
      END IF
C
C -- I N V E R S E   . . .
C
      IF (INDIC .EQ. 1) THEN
C
         PROJ(1) = COORD(1)
         PROJ(2) = COORD(2)
         IERR = 0
         X = (PROJ(1) - X0)/A
         Y = (PROJ(2) - Y0)/A
         XP=X
         YP=Y
         NN=0
C
C     USE KNUTH ALGORITHM FOR SUMMING COMPLEX TERMS, TO CONVERT
C     MODIFIED-STEREOGRAPHIC CONFORMAL TO OBLIQUE STEREOGRAPHIC
C     COORDINATES (XP,YP).
C
  225    R=XP+XP
         S=XP*XP+YP*YP
         AR=ACOEF(N)
         AI=BCOEF(N)
         BR=ACOEF(N-1)
         BI=BCOEF(N-1)
         CR=N*AR
         CI=N*AI
         DR=(N-1)*BR
         DI=(N-1)*BI
         DO 230 J=2,N
         ARN=BR+R*AR
         AIN=BI+R*AI
         IF (J.EQ.N) GO TO 230
         BR=ACOEF(N-J)-S*AR
         BI=BCOEF(N-J)-S*AI
         AR=ARN
         AI=AIN
         CRN=DR+R*CR
         CIN=DI+R*CI
         DR=(N-J)*ACOEF(N-J)-S*CR
         DI=(N-J)*BCOEF(N-J)-S*CI
         CR=CRN
         CI=CIN
  230    CONTINUE
         BR=-S*AR
         BI=-S*AI
         AR=ARN
         AI=AIN
         FXYR=XP*AR-YP*AI+BR-X
         FXYI=YP*AR+XP*AI+BI-Y
         FPXYR=XP*CR-YP*CI+DR
         FPXYI=YP*CR+XP*CI+DI
         DEN=FPXYR*FPXYR+FPXYI*FPXYI
         DXP=-(FXYR*FPXYR+FXYI*FPXYI)/DEN
         DYP=-(FXYI*FPXYR-FXYR*FPXYI)/DEN
         XP=XP+DXP
         YP=YP+DYP
         DS=ABS(DXP)+ABS(DYP)
         NN=NN+1
         IF (NN.LE.20) GO TO 237
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,235)
  235    FORMAT (/' ERROR PJ23SP'/
     .            ' TOO MANY ITERATIONS IN ITERATING INVERSE')
         IERR = 235
         GO TO 238
  237    IF (DS.GT.EPSLN) GO TO 225
C
C     CONVERT OBLIQUE STEREOGRAPHIC COORDINATES TO LAT/LONG.
C
  238    RH = SQRT (XP * XP + YP * YP)
         Z = TWO * ATAN (RH / TWO)
         SINZ = SIN (Z)
         COSZ = COS (Z)
         GEOG(1) = LON0
         IF (ABS(RH) .GT. EPSLN) GO TO 240
         GEOG(2) = LAT0
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
  240    CHI = ASINSP (COSZ * SCHIO + YP *SINZ * CCHIO / RH)
         NN=0
         PHI=CHI
  250    ESPHI=EC*SIN(PHI)
         DPHI=TWO*ATAN(TAN((HALFPI+CHI)/TWO)*((ONE+ESPHI)/(ONE-ESPHI))
     .      **(EC/TWO)) - HALFPI - PHI
         PHI = PHI + DPHI
         NN = NN + 1
         IF (NN.LE.20) GO TO 257
         IF (IPEMSG .EQ. 0) WRITE (IPELUN,255)
  255    FORMAT (/' ERROR PJ23SP'/
     .            ' TOO MANY ITERATIONS IN CALCULATING PHI FROM CHI')
         IERR = 236
         GO TO 260
  257    IF (ABS(DPHI).GT.EPSLN) GO TO 250
  260    GEOG(2)=PHI
         GEOG(1) = ADJLSP (LON0 + ATAN2(XP*SINZ, RH*CCHIO*COSZ-YP*SCHIO
     .     *SINZ))
         CRDIO(1) = GEOG(1)
         CRDIO(2) = GEOG(2)
         RETURN
      END IF
C
      END
