C
C	$Id: msbsf1.f,v 1.2 2000-07-12 16:26:21 haley Exp $
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
      SUBROUTINE MSBSF1 (M,N,XMIN,XMAX,YMIN,YMAX,Z,IZ,ZP,TEMP,SIGMA)
C
      DIMENSION Z(IZ,N),ZP(M,N,3),TEMP(1)
C
C ---------------------------------------------------------------------
C Note:  This routine comes from a proprietary package called FITPACK.
C It is used in the NCAR graphics package by permission of the author,
C Alan Cline.
C ---------------------------------------------------------------------
C
C                                            CODED BY ALAN KAYLOR CLINE
C                                         FROM FITPACK -- JUNE 22, 1986
C                                   A CURVE AND SURFACE FITTING PACKAGE
C                                 A PRODUCT OF PLEASANT VALLEY SOFTWARE
C                             8603 ALTUS COVE, AUSTIN, TEXAS 78759, USA
C
C ---------------------------------------------------------------------
C
C THIS SUBROUTINE DETERMINES THE PARAMETERS NECESSARY TO
C COMPUTE AN INTERPOLATORY SURFACE PASSING THROUGH A RECT-
C ANGULAR GRID OF FUNCTIONAL VALUES. THE X AND Y VALUES ARE
C ASSUMED EQUALLY SPACED IN THE GRID. THE SURFACE DETERMINED
C CAN BE REPRESENTED AS THE TENSOR PRODUCT OF SPLINES UNDER
C TENSION. FOR ACTUAL INTERPOLATION AT A GRID OF POINTS
C EQUALLY SPACED IN BOTH X AND Y COORDINATES IT IS NECESSARY
C TO CALL SUBROUTINE MSBSF2.
C
C ON INPUT--
C
C   M IS THE NUMBER OF GRID LINES IN THE X-DIRECTION, I. E.
C   LINES PARALLEL TO THE Y-AXIS (M .GE. 2).
C
C   N IS THE NUMBER OF GRID LINES IN THE Y-DIRECTION, I. E.
C   LINES PARALLEL TO THE X-AXIS (N .GE. 2).
C
C   XMIN AND XMAX ARE THE LOWER AND UPPER LIMITS,
C   RESPECTIVELY, OF THE GRID IN THE X-DIRECTION. XMAX
C   SHOULD BE GREATER THAN XMIN.
C
C   YMIN AND YMAX ARE THE LOWER AND UPPER LIMITS,
C   RESPECTIVELY, OF THE GRID IN THE Y-DIRECTION. YMAX
C   SHOULD BE GREATER THAN YMIN.
C
C   Z IS AN ARRAY OF THE M * N FUNCTIONAL VALUES AT THE GRID
C   POINTS, I. E. Z(I,J) CONTAINS THE FUNCTIONAL VALUE AT
C   (X(I),Y(J)) FOR I = 1,...,M AND J = 1,...,N, WHERE X(I)
C   REPRESENTS THE I-TH EQUISPACED X VALUE, AND Y(J)
C   REPRESENTS THE J-TH EQUISPACED Y VALUE.
C
C   IZ IS THE ROW DIMENSION OF THE MATRIX Z USED IN THE
C   CALLING PROGRAM (IZ .GE. M).
C
C   ZP IS AN ARRAY OF AT LEAST 3*M*N LOCATIONS.
C
C   TEMP IS AN ARRAY OF AT LEAST N+N+M LOCATIONS WHICH IS
C   USED FOR SCRATCH STORAGE.
C
C   SIGMA CONTAINS THE TENSION FACTOR. THIS VALUE INDICATES
C   THE CURVINESS DESIRED. IF ABS(SIGMA) IS NEARLY ZERO
C   (E. G. .001) THE RESULTING SURFACE IS APPROXIMATELY THE
C   TENSOR PRODUCT OF CUBIC SPLINES. IF ABS(SIGMA) IS LARGE
C   (E. G. 50.) THE RESULTING SURFACE IS APPROXIMATELY
C   BI-LINEAR. IF SIGMA EQUALS ZERO TENSOR PRODUCTS OF
C   CUBIC SPLINES RESULT. A STANDARD VALUE FOR SIGMA IS
C   APPROXIMATELY 1. IN ABSOLUTE VALUE.
C
C ON OUTPUT--
C
C   ZP CONTAINS THE VALUES OF THE XX-, YY-, AND XXYY-PARTIAL
C   DERIVATIVES OF THE SURFACE AT THE GIVEN NODES.
C
C   M, N, XMIN, XMAX, YMIN, YMAX, Z, IZ, AND SIGMA ARE
C   UNALTERED.
C
C THIS SUBROUTINE REFERENCES PACKAGE MODULES MSCEEZ, MSTRMS,
C AND MSSHCH.
C
C-----------------------------------------------------------
C
      MM1 = M-1
      MP1 = M+1
      NM1 = N-1
      NP1 = N+1
      NPM = N+M
C
C DENORMALIZE TENSION FACTOR IN Y-DIRECTION
C
      SIGMAY = ABS(SIGMA)*FLOAT(N-1)/(YMAX-YMIN)
C
C OBTAIN Y-PARTIAL DERIVATIVES ALONG Y = YMIN
C
      DELY1 = (YMAX-YMIN)/FLOAT(N-1)
      DELY2 = DELY1+DELY1
      CALL MSCEEZ (DELY1,DELY2,SIGMAY,C1,C2,C3,N)
      DO 1 I = 1,M
    1   ZP(I,1,1) = C1*Z(I,1)+C2*Z(I,2)
      IF (N .EQ. 2) GO TO 3
      DO 2 I = 1,M
    2   ZP(I,1,1) = ZP(I,1,1)+C3*Z(I,3)
C
C OBTAIN Y-PARTIAL DERIVATIVES ALONG Y = YMAX
C
    3 C1 = -C1
      C2 = -C2
      IF (N .GT. 2) C3 = -C3
      DO 4 I = 1,M
        NPI = N+I
    4   TEMP(NPI) = C1*Z(I,N)+C2*Z(I,NM1)
      IF (N .EQ. 2) GO TO 6
      DO 5 I = 1,M
        NPI = N+I
    5   TEMP(NPI) = TEMP(NPI)+C3*Z(I,N-2)
C
C DENORMALIZE TENSION FACTOR IN X-DIRECTION
C
    6 SIGMAX = ABS(SIGMA)*FLOAT(M-1)/(XMAX-XMIN)
C
C OBTAIN X-PARTIAL DERIVATIVES ALONG X = XMIN
C
      DELX1 = (XMAX-XMIN)/FLOAT(M-1)
      DELX2 = DELX1+DELX1
      CALL MSCEEZ (DELX1,DELX2,SIGMAX,C1,C2,C3,M)
      DO 7 J = 1,N
    7   ZP(1,J,2) = C1*Z(1,J)+C2*Z(2,J)
      IF (M .EQ. 2) GO TO 9
      DO 8 J = 1,N
    8   ZP(1,J,2) = ZP(1,J,2)+C3*Z(3,J)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (XMIN,YMIN)
C
    9 ZP(1,1,3) = C1*ZP(1,1,1)+C2*ZP(2,1,1)
      IF (M .GT. 2) ZP(1,1,3) = ZP(1,1,3)+C3*ZP(3,1,1)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (XMIN,YMAX)
C
      ZXY1NS = C1*TEMP(N+1)+C2*TEMP(N+2)
      IF (M .GT. 2) ZXY1NS = ZXY1NS+C3*TEMP(N+3)
C
C OBTAIN X-PARTIAL DERIVATIVE ALONG X = XMAX
C
      C1 = -C1
      C2 = -C2
      IF (M .GT. 2) C3 = -C3
      DO 10 J = 1,N
        NPMPJ = NPM+J
   10   TEMP(NPMPJ) = C1*Z(M,J)+C2*Z(MM1,J)
      IF (M .EQ. 2) GO TO 12
      DO 11 J = 1,N
        NPMPJ = NPM+J
   11   TEMP(NPMPJ) = TEMP(NPMPJ)+C3*Z(M-2,J)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (XMAX,YMIN)
C
   12 ZP(M,1,3) = C1*ZP(M,1,1)+C2*ZP(MM1,1,1)
      IF (M .GT. 2) ZP(M,1,3) = ZP(M,1,3)+C3*ZP(M-2,1,1)
C
C OBTAIN X-Y-PARTIAL DERIVATIVE AT (XMAX,YMAX)
C
      ZXYMNS = C1*TEMP(NPM)+C2*TEMP(NPM-1)
      IF (M .GT. 2) ZXYMNS = ZXYMNS+C3*TEMP(NPM-2)
C
C SET UP RIGHT HAND SIDES AND TRIDIAGONAL SYSTEM FOR Y-GRID
C PERFORM FORWARD ELIMINATION
C
      DEL1 = DELY1
      DELI = 1./DEL1
      DO 13 I = 1,M
   13   ZP(I,2,1) = DELI*(Z(I,2)-Z(I,1))
      ZP(1,2,3) = DELI*(ZP(1,2,2)-ZP(1,1,2))
      ZP(M,2,3) = DELI*(TEMP(NPM+2)-TEMP(NPM+1))
      CALL MSTRMS (DIAG1,SDIAG1,SIGMAY,DEL1)
      DIAGI = 1./DIAG1
      DO 14 I = 1,M
   14   ZP(I,1,1) = DIAGI*(ZP(I,2,1)-ZP(I,1,1))
      ZP(1,1,3) = DIAGI*(ZP(1,2,3)-ZP(1,1,3))
      ZP(M,1,3) = DIAGI*(ZP(M,2,3)-ZP(M,1,3))
      TEMP(1) = DIAGI*SDIAG1
      IF (N .EQ. 2) GO TO 18
      DO 17 J = 2,NM1
        JM1 = J-1
        JP1 = J+1
        NPMPJ = NPM+J
        DO 15 I = 1,M
   15     ZP(I,JP1,1) = DELI*(Z(I,JP1)-Z(I,J))
        ZP(1,JP1,3) = DELI*(ZP(1,JP1,2)-ZP(1,J,2))
        ZP(M,JP1,3) = DELI*(TEMP(NPMPJ+1)-TEMP(NPMPJ))
        DIAGIN = 1./(DIAG1+DIAG1-SDIAG1*TEMP(JM1))
        DO 16 I = 1,M
   16     ZP(I,J,1) = DIAGIN*(ZP(I,JP1,1)-ZP(I,J,1)-
     *                        SDIAG1*ZP(I,JM1,1))
        ZP(1,J,3) = DIAGIN*(ZP(1,JP1,3)-ZP(1,J,3)-
     *                      SDIAG1*ZP(1,JM1,3))
        ZP(M,J,3) = DIAGIN*(ZP(M,JP1,3)-ZP(M,J,3)-
     *                      SDIAG1*ZP(M,JM1,3))
   17   TEMP(J) = DIAGIN*SDIAG1
   18 DIAGIN = 1./(DIAG1-SDIAG1*TEMP(NM1))
      DO 19 I = 1,M
        NPI = N+I
   19   ZP(I,N,1) = DIAGIN*(TEMP(NPI)-ZP(I,N,1)-
     *                      SDIAG1*ZP(I,NM1,1))
      ZP(1,N,3) = DIAGIN*(ZXY1NS-ZP(1,N,3)-
     *                    SDIAG1*ZP(1,NM1,3))
      TEMP(N) = DIAGIN*(ZXYMNS-ZP(M,N,3)-
     *                  SDIAG1*ZP(M,NM1,3))
C
C PERFORM BACK SUBSTITUTION
C
      DO 21 J = 2,N
        JBAK = NP1-J
        JBAKP1 = JBAK+1
        T = TEMP(JBAK)
        DO 20 I = 1,M
   20     ZP(I,JBAK,1) = ZP(I,JBAK,1)-T*ZP(I,JBAKP1,1)
        ZP(1,JBAK,3) = ZP(1,JBAK,3)-T*ZP(1,JBAKP1,3)
   21   TEMP(JBAK) = ZP(M,JBAK,3)-T*TEMP(JBAKP1)
C
C SET UP RIGHT HAND SIDES AND TRIDIAGONAL SYSTEM FOR X-GRID
C PERFORM FORWARD ELIMINATION
C
      DEL1 = DELX1
      DELI = 1./DEL1
      DO 22 J = 1,N
        ZP(2,J,2) = DELI*(Z(2,J)-Z(1,J))
   22   ZP(2,J,3) = DELI*(ZP(2,J,1)-ZP(1,J,1))
      CALL MSTRMS (DIAG1,SDIAG1,SIGMAX,DEL1)
      DIAGI = 1./DIAG1
      DO 23 J = 1,N
        ZP(1,J,2) = DIAGI*(ZP(2,J,2)-ZP(1,J,2))
   23   ZP(1,J,3) = DIAGI*(ZP(2,J,3)-ZP(1,J,3))
      TEMP(N+1) = DIAGI*SDIAG1
      IF (M  .EQ. 2) GO TO 27
      DO 26 I = 2,MM1
        IM1 = I-1
        IP1 = I+1
        NPI = N+I
        DO 24 J = 1,N
          ZP(IP1,J,2) = DELI*(Z(IP1,J)-Z(I,J))
   24     ZP(IP1,J,3) = DELI*(ZP(IP1,J,1)-ZP(I,J,1))
        DIAGIN = 1./(DIAG1+DIAG1-SDIAG1*TEMP(NPI-1))
        DO 25 J = 1,N
          ZP(I,J,2) = DIAGIN*(ZP(IP1,J,2)-ZP(I,J,2)-
     *                        SDIAG1*ZP(IM1,J,2))
   25     ZP(I,J,3) = DIAGIN*(ZP(IP1,J,3)-ZP(I,J,3)-
     *                        SDIAG1*ZP(IM1,J,3))
   26   TEMP(NPI) = DIAGIN*SDIAG1
   27 DIAGIN = 1./(DIAG1-SDIAG1*TEMP(NPM-1))
      DO 28 J = 1,N
        NPMPJ = NPM+J
        ZP(M,J,2) = DIAGIN*(TEMP(NPMPJ)-ZP(M,J,2)-
     *                      SDIAG1*ZP(MM1,J,2))
   28   ZP(M,J,3) = DIAGIN*(TEMP(J)-ZP(M,J,3)-
     *                      SDIAG1*ZP(MM1,J,3))
C
C PERFORM BACK SUBSTITUTION
C
      DO 29 I = 2,M
        IBAK = MP1-I
        IBAKP1 = IBAK+1
        NPIBAK = N+IBAK
        T = TEMP(NPIBAK)
        DO 29 J = 1,N
          ZP(IBAK,J,2) = ZP(IBAK,J,2)-T*ZP(IBAKP1,J,2)
   29     ZP(IBAK,J,3) = ZP(IBAK,J,3)-T*ZP(IBAKP1,J,3)
      RETURN
C
      END
