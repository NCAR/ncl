C
C $Id: istr32.f,v 1.6 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ISTR32 (UT,VT,WT,XT,YT,ZT,IENT)
C
C This routine implements the 3-space to 2-space transformation by
C Kuber, Szabo and Giulieri, as described in "The Perspective
C Representation of Functions of Two Variables", Journal of the ACM
C 15, 2, 193-204, 1968.
C
C If IENT = 1, the transformation is being defined:
C
C   UT, VT, and WT are the 3-space coordinates of the intersection of
C   the line of sight and the image plane.  This point can be thought
C   of as the point looked at.
C
C   XT, YT, and ZT are the 3-space coordinates of the eye position.
C
C If IENT is 2, a point is being transformed:
C
C   UT, VT, WT are the 3-space coordinates of a point to be transformed.
C
C   XT and YT are the coordinates resulting from the 3-space to 2-space
C   transformation.  When ISCALE is zero, XT and YT are in the same
C   units as U, V, and W.  When ISCALE is non-zero, XT and YT are in
C   the fractional coordinate system.
C
C   ZT is not used.
C
C Due to the nature of this routine, all variables used in it are saved
C from call to call.
C
      SAVE
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
C
      COMMON /PWRZ1I/ UUMIN,UUMAX,VVMIN,VVMAX,WWMIN,WWMAX,DELCRT,EYEU,
     +                EYEV,EYEW
C
C Define the range of fractional coordinates to be used.
C
      DATA FXL,FXR,FYB,FYT / .0005,.9995,.0005,.9995 /
C
C Define the value of "pi".
C
      DATA PI / 3.14159265358979 /
C
C Decide what kind of call was done.
C
      IF (IENT .NE. 1) GO TO  50
C
C The transformation is to be defined:
C
      AU = UT
      AV = VT
      AW = WT
      EU = XT
      EV = YT
      EW = ZT
C
      DU = AU-EU
      DV = AV-EV
      DW = AW-EW
      D = SQRT(DU*DU+DV*DV+DW*DW)
      COSAL = DU/D
      COSBE = DV/D
      COSGA = DW/D
C
      AL = ATAN(ABS(SQRT(1.-COSAL*COSAL)/COSAL))
      IF (COSAL .LE. 0.) AL = PI-AL
      BE = ATAN(ABS(SQRT(1.-COSBE*COSBE)/COSBE))
      IF (COSBE .LE. 0.) BE = PI-BE
      GA = ATAN(ABS(SQRT(1.-COSGA*COSGA)/COSGA))
      IF (COSGA .LE. 0.) GA = PI-GA
      SINGA = SIN(GA)
C
C The 3-space point looked at is transformed into (0,0) of the 2-space
C plane.  The 3-space W axis is transformed into the 2-space Y axis.
C If the line of sight is almost parallel to the 3-space W axis, the
C 3-space V axis is chosen (instead of the 3-space W axis) to be
C transformed into the 2-space Y axis.
C
      JDONE = 2
      IF (ISCALE)  10, 30, 10
   10 X0 = XMIN
      Y0 = YMIN
      X1 = FXL
      Y1 = FYB
      X2 = FXR-FXL
      Y2 = FYT-FYB
      X3 = X2/(XMAX-XMIN)
      Y3 = Y2/(YMAX-YMIN)
      X4 = FXR
      Y4 = FYT
      FACT = 1.
      IF (BIGD .LE. 0.) GO TO  20
      X0 = -BIGD
      Y0 = -BIGD
      X3 = X2/(2.*BIGD)
      Y3 = Y2/(2.*BIGD)
      FACT = R0/D
   20 DELCRT = X2*32767.
      JDONE = 1
   30 IF (SINGA .LT. 0.0001) GO TO  40
      R = 1./SINGA
      JUMP = 2
      RETURN
   40 SINBE = SIN(BE)
      R = 1./SINBE
      JUMP = 1
      RETURN
C
C A point is to be transformed:
C
   50 UU = UT
      VV = VT
      WW = WT
      Q = D/((UU-EU)*COSAL+(VV-EV)*COSBE+(WW-EW)*COSGA)
      GO TO (60, 70), JUMP
   60 UU = ((EW+Q*(WW-EW)-AW)*COSAL-(EU+Q*(UU-EU)-AU)*COSGA)*R
      VV = (EV+Q*(VV-EV)-AV)*R
      GO TO (80, 90), JDONE
   70 UU = ((EU+Q*(UU-EU)-AU)*COSBE-(EV+Q*(VV-EV)-AV)*COSAL)*R
      VV = (EW+Q*(WW-EW)-AW)*R
      GO TO (80, 90), JDONE
   80 XT = MIN(X4,MAX(X1,X1+X3*(FACT*UU-X0)))
      YT = MIN(Y4,MAX(Y1,Y1+Y3*(FACT*VV-Y0)))
      RETURN
   90 XT = UU
      YT = VV
      RETURN
C
      END
