C
C	$Id: mssrf2.f,v 1.5 2008-07-27 00:17:31 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      REAL FUNCTION MSSRF2 (XX,YY,M,N,X,Y,Z,IZ,ZP,SIGMA)
C
      INTEGER M,N,IZ
      REAL XX,YY,X(M),Y(N),Z(IZ,N),ZP(M,N,3),SIGMA
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
C THIS FUNCTION INTERPOLATES A SURFACE AT A GIVEN COORDINATE
C PAIR USING A BI-SPLINE UNDER TENSION. THE SUBROUTINE MSSRF1
C SHOULD BE CALLED EARLIER TO DETERMINE CERTAIN NECESSARY
C PARAMETERS.
C
C ON INPUT--
C
C   XX AND YY CONTAIN THE X- AND Y-COORDINATES OF THE POINT
C   TO BE MAPPED ONTO THE INTERPOLATING SURFACE.
C
C   M AND N CONTAIN THE NUMBER OF GRID LINES IN THE X- AND
C   Y-DIRECTIONS, RESPECTIVELY, OF THE RECTANGULAR GRID
C   WHICH SPECIFIED THE SURFACE.
C
C   X AND Y ARE ARRAYS CONTAINING THE X- AND Y-GRID VALUES,
C   RESPECTIVELY, EACH IN INCREASING ORDER.
C
C   Z IS A MATRIX CONTAINING THE M * N FUNCTIONAL VALUES
C   CORRESPONDING TO THE GRID VALUES (I. E. Z(I,J) IS THE
C   SURFACE VALUE AT THE POINT (X(I),Y(J)) FOR I = 1,...,M
C   AND J = 1,...,N).
C
C   IZ CONTAINS THE ROW DIMENSION OF THE ARRAY Z AS DECLARED
C   IN THE CALLING PROGRAM.
C
C   ZP IS AN ARRAY OF 3*M*N LOCATIONS STORED WITH THE
C   VARIOUS SURFACE DERIVATIVE INFORMATION DETERMINED BY
C   MSSRF1.
C
C AND
C
C   SIGMA CONTAINS THE TENSION FACTOR (ITS SIGN IS IGNORED).
C
C THE PARAMETERS M, N, X, Y, Z, IZ, ZP, AND SIGMA SHOULD BE
C INPUT UNALTERED FROM THE OUTPUT OF MSSRF1.
C
C ON OUTPUT--
C
C   MSSRF2 CONTAINS THE INTERPOLATED SURFACE VALUE.
C
C NONE OF THE INPUT PARAMETERS ARE ALTERED.
C
C THIS FUNCTION REFERENCES PACKAGE MODULES MSNTVL AND
C MSSHCH.
C
C-----------------------------------------------------------
C
C INLINE ONE DIMENSIONAL CUBIC SPLINE INTERPOLATION
C
      HERMZ (F1,F2,FP1,FP2) = (F2*DEL1+F1*DEL2)/DELS-DEL1*
     *                        DEL2*(FP2*(DEL1+DELS)+
     *                              FP1*(DEL2+DELS))/
     *                        (6.*DELS)
C
C INLINE ONE DIMENSIONAL SPLINE UNDER TENSION INTERPOLATION
C
      HERMNZ (F1,F2,FP1,FP2,SIGMAP) = (F2*DEL1+F1*DEL2)/DELS
     *          +(FP2*DEL1*(SINHM1-SINHMS)
     *           +FP1*DEL2*(SINHM2-SINHMS)
     *          )/(SIGMAP*SIGMAP*DELS*(1.+SINHMS))
C
C DENORMALIZE TENSION FACTOR IN X AND Y DIRECTION
C
      SIGMAX = ABS(SIGMA)*REAL(M-1)/(X(M)-X(1))
      SIGMAY = ABS(SIGMA)*REAL(N-1)/(Y(N)-Y(1))
C
C DETERMINE Y INTERVAL
C
      JM1 = MSNTVL (YY,Y,N)
      J = JM1+1
C
C DETERMINE X INTERVAL
C
      IM1 = MSNTVL (XX,X,M)
      I = IM1+1
      DEL1 = YY-Y(JM1)
      DEL2 = Y(J)-YY
      DELS = Y(J)-Y(JM1)
      IF (SIGMAY .NE. 0.) GO TO 1
C
C PERFORM FOUR INTERPOLATIONS IN Y-DIRECTION
C
      ZIM1 = HERMZ(Z(I-1,J-1),Z(I-1,J),ZP(I-1,J-1,1),
     *                                  ZP(I-1,J,1))
      ZI = HERMZ(Z(I,J-1),Z(I,J),ZP(I,J-1,1),ZP(I,J,1))
      ZXXIM1 = HERMZ(ZP(I-1,J-1,2),ZP(I-1,J,2),
     *                ZP(I-1,J-1,3),ZP(I-1,J,3))
      ZXXI = HERMZ(ZP(I,J-1,2),ZP(I,J,2),
     *              ZP(I,J-1,3),ZP(I,J,3))
      GO TO 2
    1 CALL MSSHCH (SINHM1,DUMMY,SIGMAY*DEL1,-1)
      CALL MSSHCH (SINHM2,DUMMY,SIGMAY*DEL2,-1)
      CALL MSSHCH (SINHMS,DUMMY,SIGMAY*DELS,-1)
      ZIM1 = HERMNZ(Z(I-1,J-1),Z(I-1,J),ZP(I-1,J-1,1),
     *               ZP(I-1,J,1),SIGMAY)
      ZI = HERMNZ(Z(I,J-1),Z(I,J),ZP(I,J-1,1),ZP(I,J,1),
     *             SIGMAY)
      ZXXIM1 = HERMNZ(ZP(I-1,J-1,2),ZP(I-1,J,2),
     *                 ZP(I-1,J-1,3),ZP(I-1,J,3),SIGMAY)
      ZXXI = HERMNZ(ZP(I,J-1,2),ZP(I,J,2),
     *               ZP(I,J-1,3),ZP(I,J,3),SIGMAY)
C
C PERFORM FINAL INTERPOLATION IN X-DIRECTION
C
    2 DEL1 = XX-X(IM1)
      DEL2 = X(I)-XX
      DELS = X(I)-X(IM1)
      IF (SIGMAX .NE. 0.) GO TO 3
      MSSRF2 = HERMZ(ZIM1,ZI,ZXXIM1,ZXXI)
      RETURN
    3 CALL MSSHCH (SINHM1,DUMMY,SIGMAX*DEL1,-1)
      CALL MSSHCH (SINHM2,DUMMY,SIGMAX*DEL2,-1)
      CALL MSSHCH (SINHMS,DUMMY,SIGMAX*DELS,-1)
      MSSRF2 = HERMNZ(ZIM1,ZI,ZXXIM1,ZXXI,SIGMAX)
      RETURN
      END
