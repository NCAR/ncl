C
C $Id: mdqtrn.f,v 1.1 2008-04-18 04:09:20 kennison Exp $
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
      SUBROUTINE MDQTRN (RLAT,RLON,U,V)
C
        DOUBLE PRECISION RLAT,RLON,U,V
C
C Given the latitude, RLAT, and the longitude, RLON, of a point on the
C earth, this routine returns the U and V coordinates of the projection
C of that point on the map.
C
C Declare a special common block, containing only those variables that
C MDQINI needs to set to make MDQTRA, MDQTRI, and MDQTRN carry out the
C transformation in effect at the time MDQINI was called.
C
        COMMON /MAQCMN/  ALFA,COSO,COSR,DCSA,DCSB,DSNA,DSNB,DTOR,DTRH,
     +                   OOPI,PHOC,  PI,PIOT,ROTA,RTDD,RTOD,SALT,SINO,
     +                   SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,UOFF,URNM,
     +                   VCNM,VMNM,VMXM,VOFF,VRNM,UTPA,IPRF,IPRJ,IROD,
     +                   ELPM
        DOUBLE PRECISION ALFA,COSO,COSR,DCSA,DCSB,DSNA,DSNB,DTOR,DTRH,
     +                   OOPI,PHOC,  PI,PIOT,ROTA,RTDD,RTOD,SALT,SINO,
     +                   SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,UOFF,URNM,
     +                   VCNM,VMNM,VMXM,VOFF,VRNM,UTPA(15)
C
        INTEGER IPRF,IPRJ,IROD
C
        LOGICAL ELPM
C
        SAVE   /MAQCMN/
C
C Declare local variables.
C
        DOUBLE PRECISION CHI,COSA,COSB,COSLA,COSPH,SINA,SINB,SINLA,
     +                   SINPH,TCOS,TEMP,TMP1,TMP2,UTM1,UTM2,UTM3,
     +                   VTM1,VTM2,VTM3
C
        DOUBLE PRECISION P,Q,R
C
        REAL             USNG,VSNG
C
C Declare function types.
C
        DOUBLE PRECISION MDGDDP,RBGDFE,RBGLEN
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDQTRN - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set up U and V for the fast paths.  U is a longitude, in degrees,
C between -180. and +180., inclusive, and V is a latitude, in degrees.
C
        TEMP=RLON-PHOC
        U=TEMP+(SIGN(180.D0,180.D0-TEMP)-SIGN(180.D0,TEMP+180.D0))
        V=MAX(-90.D0,MIN(90.D0,RLAT))
C
C If a fast-path projection is in use and the rotation angle is 180,
C adjust U and V.
C
        IF (IPRJ.GE.12.AND.IPRJ.LE.16.AND.
     +      ABS(ROTA).GT.179.999999D0) THEN
          U=-U
          V=-V
        END IF
C
C Take fast paths for simple cylindrical projections.
C
        IF (IPRJ-12) 100,197,114
C
C No fast path.  Sort out the USGS transformations and the Lambert
C conformal conic from the rest.
C
  100   IF (IPRJ-1) 101,102,103
C
C USGS projections.
C
  101   IF (IPRF.EQ.0) GO TO 901
C
        IF (IROD.EQ.0) THEN
          CALL MDUTFS (REAL(RLAT),REAL(RLON),USNG,VSNG)
          IF (USNG.NE.1.E12) THEN
            U=DBLE(USNG)
            V=DBLE(VSNG)
          ELSE
            U=1.D12
            V=1.D12
          END IF
        ELSE
          CALL MDUTFD (RLAT,RLON,U,V)
        END IF
C
        IF (IPRF.EQ. 3.OR.IPRF.EQ. 4.OR.IPRF.EQ. 5.OR.IPRF.EQ. 7.OR.
     +      IPRF.EQ. 8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +      IPRF.EQ.19.OR.IPRF.EQ.21) THEN
          TMP1=RLON-UTPA(5)
          P=TMP1+(SIGN(180.D0,180.D0-TMP1)-SIGN(180.D0,TMP1+180.D0))
          IF ((IPRF.EQ.3.OR.IPRF.EQ.8).AND.ABS(RLAT).GT.89.999999D0)
     +                                                         GO TO 200
        ELSE IF (IPRF.EQ.9) THEN
          P=V
        ELSE IF (IPRF.EQ.11.OR.IPRF.EQ.12) THEN
          IF (MDGDDP(RLAT,RLON,UTPA(6),UTPA(5)).GT.179.99D0) GO TO 200
        ELSE IF (IPRF.EQ.20) THEN
          P=U*SIN(DTOR*UTPA(4))+V*COS(DTOR*UTPA(4))
        ELSE IF (IPRF.EQ.22) THEN
          P=U*SIN(DTOR*UTPA(4))+V*COS(DTOR*UTPA(4))
        END IF
C
        GO TO 199
C
C Lambert conformal conic.
C
  102   P=U
        CHI=90.D0-SINO*V
        IF (CHI.GT.179.999999D0) GO TO 200
        R=TAN(DTRH*CHI)**COSO
        U=U*COSO*DTOR
        V=-R*SINO*COS(U)
        U=R*SIN(U)
        GO TO 198
C
C Not Lambert conformal conic.  Calculate constants common to most of
C the other projections.
C
  103   TMP1=U*DTOR
        TMP2=V*DTOR
        SINPH=SIN(TMP1)
        SINLA=SIN(TMP2)
        COSPH=COS(TMP1)
        COSLA=COS(TMP2)
        TCOS=COSLA*COSPH
        COSA=MAX(-1.D0,MIN(+1.D0,SINLA*SINO+TCOS*COSO))
        SINA=SQRT(1.D0-COSA*COSA)
        IF (SINA.LT..000001D0) THEN
          SINA=0.D0
          IF (IPRJ.EQ.3.AND.ABS(SALT).GT.1.D0) THEN
            SINB=0.D0
            COSB=1.D0
            GO TO 105
          END IF
          IF (IPRJ.GE.7.OR.COSA.LT.0.D0) GO TO 200
          U=0.D0
          V=0.D0
          GO TO 197
        END IF
        SINB=COSLA*SINPH/SINA
        COSB=(SINLA*COSO-TCOS*SINO)/SINA
C
C Jump to code appropriate for the chosen projection.
C
C Projection:   ST  OR  LE  GN  AE  CE  ME  MO  RO  EA
C
        GO TO (104,105,106,107,108,109,110,111,112,113) , IPRJ-1
C
C Stereographic.
C
  104   IF (ABS(SINA).LT..000001D0) THEN
          R=SINA/2.D0
        ELSE
          R=(1.D0-COSA)/SINA
        END IF
        GO TO 196
C
C Orthographic or satellite-view, depending on the value of SALT.
C
  105   IF (ABS(SALT).LE.1.D0) THEN
          IF (COSA.GT.0.D0) THEN
            R=SINA
          ELSE
            IF (SALT.GE.0.D0) GO TO 200
            R=2.D0-SINA
          END IF
          GO TO 196
        ELSE
          IF (COSA.GT.1.D0/ABS(SALT)) THEN
            R=SRSS*SINA/(ABS(SALT)-COSA)
          ELSE
            IF (SALT.GE.0.D0) GO TO 200
            R=2.D0-SRSS*SINA/(ABS(SALT)-COSA)
          END IF
          IF (ALFA.EQ.0.D0) GO TO 196
          UTM1=R*(SINB*COSR+COSB*SINR)
          VTM1=R*(COSB*COSR-SINB*SINR)
          UTM2=UTM1*DCSB+VTM1*DSNB
          VTM2=VTM1*DCSB-UTM1*DSNB
          IF ((SRSS*DCSA+UTM2*DSNA)/SRSS.LT..0001D0) GO TO 200
          UTM3=SRSS*(UTM2*DCSA-SRSS*DSNA)/
     +                    (UTM2*DSNA+SRSS*DCSA)
          VTM3=SRSS*VTM2/(UTM2*DSNA+SRSS*DCSA)
          U=UTM3*DCSB-VTM3*DSNB
          V=VTM3*DCSB+UTM3*DSNB
          GO TO 197
        END IF
C
C Lambert equal area.
C
  106   IF (ABS(COSA+1.D0).LT..000001D0) GO TO 200
        R=(1.D0+COSA)/SINA
        R=2.D0/SQRT(1.D0+R*R)
        GO TO 196
C
C Gnomonic.
C
  107   IF (COSA.LE..000001D0) GO TO 200
        R=SINA/COSA
        GO TO 196
C
C Azimuthal equidistant.
C
  108   IF (ABS(COSA+1.D0).LT..000001D0) GO TO 200
        R=ACOS(COSA)
        GO TO 196
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  109   U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*RTOD
        V=90.D0-ACOS(COSA)*RTOD
        GO TO 197
C
C Mercator, arbitrary pole and orientation.
C
  110   U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)
        V=LOG((1.D0+COSA)/SINA)
        GO TO 197
C
C Mollweide, arbitrary pole and orientation.
C
  111   P=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*TOPI
        U=P*SINA
        V=COSA
        GO TO 198
C
C Robinson, arbitrary pole and orientation.
C
  112   P=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)*OOPI
        U=P*RBGLEN(90.D0-ACOS(COSA)*RTOD)
        V=RBGDFE(90.D0-ACOS(COSA)*RTOD)
        GO TO 198
C
C Cylindrical equal-area, arbitrary pole and orientation.  ???
C
  113   U=ATAN2(SINB*COSR+COSB*SINR,SINB*SINR-COSB*COSR)
        V=COSA*4.D0/3.D0
        GO TO 197
C
C Fast-path cylindrical projections (with PLAT=0, ROTA=0 or 180).
C
C Projection:   ME  MO  RO  EA  RM
C
  114   GO TO (115,116,117,118,119) , IPRJ-12
C
C Mercator, fast-path.
C
  115   IF (ABS(V).GT.89.999999D0) GO TO 200
        U=U*DTOR
        V=LOG(TAN((V+90.D0)*DTRH))
        GO TO 197
C
C Mollweide, fast-path.
C
  116   P=U/90.D0
        V=SIN(V*DTOR)
        U=P*SQRT(1.D0-V*V)
        GO TO 198
C
C Robinson, fast-path.
C
  117   P=U/180.D0
        U=P*RBGLEN(V)
        V=RBGDFE(V)
        GO TO 198
C
C Cylindrical equal-area, fast-path.  ???
C
  118   U=U*DTOR
        V=SIN(V*DTOR)*4.D0/3.D0
        GO TO 197
C
C Rotated Mercator.
C
  119   IF (ABS(V).GT.89.999999D0) GO TO 200
        UTM1=U*DTOR
        VTM1=LOG(TAN((V+90.D0)*DTRH))
        P=U
        Q=V
        U=UTM1*COSR+VTM1*SINR
        V=VTM1*COSR-UTM1*SINR
        GO TO 199
C
C Common terminal code for certain projections.
C
  196   U=R*(SINB*COSR+COSB*SINR)
        V=R*(COSB*COSR-SINB*SINR)
C
  197   P=U
C
  198   Q=V
C
C Normal exit.
C
  199   U=U-UOFF
        V=V-VOFF
C
        RETURN
C
C Projection of point is invisible or undefined.
C
  200   U=1.D12
        P=U
C
        RETURN
C
C Error exit.
C
  901   CALL SETER ('MDQTRN - USGS PROJECTION WAS NOT INITIALIZED',2,1)
        RETURN
C
      END
