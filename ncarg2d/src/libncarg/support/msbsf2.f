C
C	$Id: msbsf2.f,v 1.2 2000-07-12 16:26:21 haley Exp $
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
      SUBROUTINE MSBSF2 (DXMIN,DXMAX,MD,DYMIN,DYMAX,ND,DZ,
     *                   IDZ,M,N,XMIN,XMAX,YMIN,YMAX,Z,IZ,
     *                   ZP,WORK,SIGMA)
C
      DIMENSION DZ(IDZ,ND),Z(IZ,N),ZP(M,N,3),WORK(4,MD)
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
C THIS SUBROUTINE MAPS VALUES ONTO A SURFACE AT EVERY POINT
C OF A GRID EQUALLY SPACED IN BOTH X AND Y COORDINATES. THE
C SURFACE INTERPOLATION IS PERFORMED USING A BI-SPLINE UNDER
C TENSION. THE SUBROUTINE MSBSF1 SHOULD BE CALLED EARLIER TO
C DETERMINE CERTAIN NECESSARY PARAMETERS. IN BOTH MSBSF1 AND
C MSBSF2, THE ORIGINAL GRID IS ASSUMED TO BE EQUALLY SPACED
C IN THE X AND Y COORDINATES.
C
C ON INPUT--
C
C   DXMIN AND DXMAX CONTAIN THE LOWER AND UPPER LIMITS,
C   RESPECTIVELY, OF THE X-COORDINATES OF THE SECOND GRID.
C
C   MD CONTAINS THE NUMBER OF GRID LINES IN THE X DIRECTION
C   OF THE SECOND GRID (MD .GE. 1).
C
C   DYMIN AND DYMAX CONTAIN THE LOWER AND UPPER LIMITS,
C   RESPECTIVELY, OF THE Y-COORDINATES OF THE SECOND GRID.
C
C   ND CONTAINS THE NUMBER OF GRID LINES IN THE Y DIRECTION
C   OF THE SECOND GRID (ND .GE. 1).
C
C   IDZ CONTAINS THE ROW DIMENSION OF THE ARRAY DZ AS
C   DECLARED IN THE CALLING PROGRAM.
C
C   M AND N CONTAIN THE NUMBER OF GRID LINES IN THE X- AND
C   Y-DIRECTIONS, RESPECTIVELY, OF THE RECTANGULAR GRID
C   WHICH SPECIFIED THE SURFACE.
C
C   XMIN AND XMAX ARE THE LOWER AND UPPER LIMITS,
C   RESPECTIVELY, OF THE GRID IN THE X DIRECTION.
C
C   YMIN AND YMAX ARE THE LOWER AND UPPER LIMITS,
C   RESPECTIVELY, OF THE GRID IN THE Y DIRECTION.
C
C   Z IS A MATRIX CONTAINING THE M * N FUNCTIONAL VALUES
C   CORRESPONDING TO THE GRID VALUES (I. E. Z(I,J) IS THE
C   SURFACE VALUE AT THE POINT (X(I),Y(J)) FOR I = 1,...,M
C   AND J = 1,...,N, WHERE X(I) REPRESENTS THE I-TH
C   EQUISPACED X VALUE AND Y(J) REPRESENTS THE J-TH
C   EQUISPACED Y VALUE).
C
C   IZ CONTAINS THE ROW DIMENSION OF THE ARRAY Z AS DECLARED
C   IN THE CALLING PROGRAM.
C
C   ZP IS AN ARRAY OF 3*M*N LOCATIONS STORED WITH THE
C   VARIOUS SURFACE DERIVATIVE INFORMATION DETERMINED BY
C   SURF1.
C
C   WORK IS AN ARRAY OF 4*MD LOCATIONS TO BE USED INTERNALLY
C   FOR WORKSPACE.
C
C   SIGMA CONTAINS THE TENSION FACTOR (ITS SIGN IS IGNORED).
C
C THE PARAMETERS M, N, XMIN, XMAX, YMIN, YMAX, Z, IZ, ZP,
C AND SIGMA SHOULD BE INPUT UNALTERED FROM THE OUTPUT OF
C MSBSF1.
C
C ON OUTPUT--
C
C   DZ CONTAINS THE MD BY ND ARRAY OF SURFACE VALUES
C   INTERPOLATED AT THE POINTS OF THE SECOND GRID.
C
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C
C THIS FUNCTION REFERENCES PACKAGE MODULE MSSHCH.
C
C-----------------------------------------------------------
C
C DENORMALIZE TENSION FACTOR IN X AND Y DIRECTION
C
      SIGMAX = ABS(SIGMA)*FLOAT(M-1)/(XMAX-XMIN)
      SIGMAY = ABS(SIGMA)*FLOAT(N-1)/(YMAX-YMIN)
C
C FIND INTERVALS OF SECOND X GRID WITH RESPECT TO ORIGINAL X
C GRID
C
      DELTDX = 0.
      IF (MD .GE. 2) DELTDX = (DXMAX-DXMIN)/FLOAT(MD-1)
      DELX = (XMAX-XMIN)/FLOAT(M-1)
      IF (SIGMAX .NE. 0.) CALL MSSHCH (SINHMS,DUMMY,
     *                                 SIGMAX*DELX,-1)
      DO 2 II = 1,MD
        XII = DXMIN+FLOAT(II-1)*DELTDX
        I = 2+IFIX((XII-XMIN)/DELX)
        IF (I .LT. 2) I = 2
        IF (I .GT. M) I = M
        DEL1 = XII-XMIN-FLOAT(I-2)*DELX
        DEL2 = DELX-DEL1
        WORK(1,II) = DEL2/DELX
        WORK(2,II) = DEL1/DELX
        IF (SIGMAX .NE. 0.) GO TO 1
        TEMP = -DEL1*DEL2/(6.*DELX)
        WORK(3,II) = TEMP*(DEL2+DELX)
        WORK(4,II) = TEMP*(DEL1+DELX)
        GO TO 2
    1   CALL MSSHCH (SINHM1,DUMMY,SIGMAX*DEL1,-1)
        CALL MSSHCH (SINHM2,DUMMY,SIGMAX*DEL2,-1)
        TEMP = SIGMAX*SIGMAX*DELX*(1.+SINHMS)
        WORK(3,II) = DEL2*(SINHM2-SINHMS)/TEMP
        WORK(4,II) = DEL1*(SINHM1-SINHMS)/TEMP
    2   CONTINUE
C
C FIND INTERVALS OF SECOND Y GRID WITH RESPECT TO ORIGINAL Y
C GRID AND PERFORM INTRPOLATION
C
      DELTDY = 0.
      IF (ND .GE. 2) DELTDY = (DYMAX-DYMIN)/FLOAT(ND-1)
      DELY = (YMAX-YMIN)/FLOAT(N-1)
      IF (SIGMAY .NE. 0.) CALL MSSHCH (SINHMS,DUMMY,
     *                                 SIGMAY*DELY,-1)
      DO 5 JJ=1,ND
        YJJ = DYMIN+FLOAT(JJ-1)*DELTDY
        J = 2+IFIX((YJJ-YMIN)/DELY)
        IF (J .LT. 2) J = 2
        IF (J .GT. N) J = N
        JM1 = J-1
        DEL1 = YJJ-YMIN-FLOAT(J-2)*DELY
        DEL2 = DELY-DEL1
        C1 = DEL2/DELY
        C2 = DEL1/DELY
        IF (SIGMAY .NE. 0.) GO TO 3
        TEMP = -DEL1*DEL2/(6.*DELY)
        C3 = TEMP*(DEL2+DELY)
        C4 = TEMP*(DEL1+DELY)
        GO TO 4
    3   CALL MSSHCH (SINHM1,DUMMY,SIGMAY*DEL1,-1)
        CALL MSSHCH (SINHM2,DUMMY,SIGMAY*DEL2,-1)
        TEMP = SIGMAY*SIGMAY*DELY*(1.+SINHMS)
        C3 = DEL2*(SINHM2-SINHMS)/TEMP
        C4 = DEL1*(SINHM1-SINHMS)/TEMP
    4   LASTI = 0
        DO 5 II=1,MD
          XII = DXMIN+FLOAT(II-1)*DELTDX
          I = 2+IFIX((XII-XMIN)/DELX)
          IF (I .LT. 2) I = 2
          IF (I .GT. M) I = M
          IM1 = I-1
          IF (IM1 .EQ. LASTI) GO TO 5
          LASTI = IM1
          ZIM1 = C1*Z(IM1,JM1)+C2*Z(IM1,J)
     *           +C3*ZP(IM1,JM1,1)+C4*ZP(IM1,J,1)
          ZI = C1*Z(I,JM1)+C2*Z(I,J)
     *         +C3*ZP(I,JM1,1)+C4*ZP(I,J,1)
          ZXXIM1 = C1*ZP(IM1,JM1,2)+C2*ZP(IM1,J,2)
     *             +C3*ZP(IM1,JM1,3)+C4*ZP(IM1,J,3)
          ZXXI = C1*ZP(I,JM1,2)+C2*ZP(I,J,2)
     *           +C3*ZP(I,JM1,3)+C4*ZP(I,J,3)
    5     DZ(II,JJ) = WORK(1,II)*ZIM1+WORK(2,II)*ZI
     *                +WORK(3,II)*ZXXIM1+WORK(4,II)*ZXXI
      RETURN
      END
