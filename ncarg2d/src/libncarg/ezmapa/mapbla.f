C
C $Id: mapbla.f,v 1.17 2000-07-12 16:23:38 haley Exp $
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
      SUBROUTINE MAPBLA (IAMP)
C
      DIMENSION IAMP(*)
C
C Declare required common blocks.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE   /MAPCM1/
C
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE   /MAPDP1/
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UCEN,VCEN,URNG,VRNG,BLAM,SLAM,
     +                BLOM,SLOM,ISSL,PEPS
      SAVE   /MAPCM2/
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE   /MAPCM3/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPCM5/ DDCT(5),DDCL(5),LDCT(6),LDCL(6),PDCT(12),PDCL(12)
      CHARACTER*2     DDCT,DDCL,LDCT,LDCL,PDCT,PDCL
      SAVE   /MAPCM5/
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE   /MAPCMC/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
C
      COMMON /USGSC1/ UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
      DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
      INTEGER IPRF
      SAVE   /USGSC1/
C
C Define some required double-precision variables.
C
      DOUBLE PRECISION DR,DS,DCOSB,DSINB,DCOSA,DSINA,DCOSPH,DSINPH,
     +                 DCOSLA,DSINLA
C
C Declare a couple of temporary arrays to hold coordinates of a circle.
C
      DIMENSION TLAT(361),TLON(361)
C
C Dimension the arrays needed to define some lines across the map.
C
      DIMENSION XCR(2),YCR(2)
C
C Declare an array in which to construct a file name.
C
      CHARACTER*128 FLNM
C
C Declare an array to use as an input buffer in reading characters.
C
      CHARACTER*1 CHRS(512)
C
C Define required constants.
C
      DATA DTOR / .017453292519943 /
      DATA DTRH / .008726646259971 /
      DATA RTOD / 57.2957795130823 /
C
C The arithmetic statement functions FLOOR and CLING give, respectively,
C the "floor" of X - the largest integer less than or equal to X - and
C the "ceiling" of X - the smallest integer greater than or equal to X.
C
      FLOOR(X)=REAL(DINT(DBLE(X)+1.D4)-1.D4)
      CLING(X)=-FLOOR(-X)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPBLA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MAPINT
      IF (ICFELL('MAPBLA',2).NE.0) RETURN
10000 CONTINUE
C
C Put the perimeter and the limb line into the area map (in group 1 and,
C perhaps, in group 2).
C
      IPSS=1
      IGRP=IGI1
C
10001 CONTINUE
C
C Perimeter.
C
      IDLT=0
      IDRT=-1
C
      IF (.NOT.(ELPF)) GO TO 10002
      TEMP=.9998
10003 CONTINUE
      U=URNG
      V=0.
      XCRD=UCEN+TEMP*U
      YCRD=VCEN
      L10005=    1
      GO TO 10005
10004 CONTINUE
      I = 1
      GO TO 10008
10006 CONTINUE
      I =I +1
10008 CONTINUE
      IF (I .GT.(360)) GO TO 10007
      U=URNG*COS(DTOR*FLOAT(I))
      V=URNG*SIN(DTOR*FLOAT(I))
      XCRD=UCEN+TEMP*U
      YCRD=VCEN+TEMP*V*VRNG/URNG
      L10010=    1
      GO TO 10010
10009 CONTINUE
      GO TO 10006
10007 CONTINUE
      IF (TEMP.EQ.1.0002) GO TO 10011
      TEMP=1.0002
      GO TO 10003
10011 CONTINUE
      GO TO 10012
10002 CONTINUE
      XCRD=UMIN-1.0002*(UMAX-UMIN)
      YCRD=VMIN-1.0002*(UMAX-UMIN)
      L10005=    2
      GO TO 10005
10013 CONTINUE
      XCRD=UMAX+1.0002*(UMAX-UMIN)
      YCRD=VMIN-1.0002*(UMAX-UMIN)
      L10010=    2
      GO TO 10010
10014 CONTINUE
      XCRD=UMAX+1.0002*(UMAX-UMIN)
      YCRD=VMAX+1.0002*(UMAX-UMIN)
      L10010=    3
      GO TO 10010
10015 CONTINUE
      XCRD=UMIN-1.0002*(UMAX-UMIN)
      YCRD=VMAX+1.0002*(UMAX-UMIN)
      L10010=    4
      GO TO 10010
10016 CONTINUE
      XCRD=UMIN-1.0002*(UMAX-UMIN)
      YCRD=VMIN-1.0002*(UMAX-UMIN)
      L10010=    5
      GO TO 10010
10017 CONTINUE
      XCRD=UMIN+.9998*(UMAX-UMIN)
      YCRD=VMIN+.9998*(VMAX-VMIN)
      L10005=    3
      GO TO 10005
10018 CONTINUE
      XCRD=UMAX-.9998*(UMAX-UMIN)
      YCRD=VMIN+.9998*(VMAX-VMIN)
      L10010=    6
      GO TO 10010
10019 CONTINUE
      XCRD=UMAX-.9998*(UMAX-UMIN)
      YCRD=VMAX-.9998*(VMAX-VMIN)
      L10010=    7
      GO TO 10010
10020 CONTINUE
      XCRD=UMIN+.9998*(UMAX-UMIN)
      YCRD=VMAX-.9998*(VMAX-VMIN)
      L10010=    8
      GO TO 10010
10021 CONTINUE
      XCRD=UMIN+.9998*(UMAX-UMIN)
      YCRD=VMIN+.9998*(VMAX-VMIN)
      L10010=    9
      GO TO 10010
10022 CONTINUE
10012 CONTINUE
      L10024=    1
      GO TO 10024
10023 CONTINUE
C
C Don't put the limb line in group 2.
C
      IF (IPSS.EQ.2) GO TO 108
C
C Limb line.
C
C Projection:     US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
      GO TO (100,101,108,102,103,108,104,107,107,105,107,
     +                                   107,107,105,107) , IPRJ+1
C
C USGS transformations.
C
  100 CONTINUE
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.4.OR.IPRF.EQ.5.OR.IPRF.EQ.7.OR.IPRF
     +.EQ.8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.IPRF.EQ.19.OR.I
     +PRF.EQ.21)) GO TO 10025
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.8.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.IP
     +RF.EQ.21)) GO TO 10026
      DLON=GRDR
      RLAT=-89.998
      IDLT=0
      IDRT=-1
      K=CLING(360./DLON)
      DO 10027 I=1,2
      RLON=REAL(UTPA(5))-180.
      CALL MAPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',3).NE.0) RETURN
      DO 10028 J=1,K-1
      RLON=RLON+DLON
      CALL MAPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',4).NE.0) RETURN
10028 CONTINUE
      RLON=REAL(UTPA(5))+180.
      CALL MAPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',5).NE.0) RETURN
      CALL MAPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',6).NE.0) RETURN
      RLAT=89.998
      IDLT=-1
      IDRT=0
10027 CONTINUE
      L10024=    2
      GO TO 10024
10029 CONTINUE
10026 CONTINUE
      IF (.NOT.(IPRF.EQ.7)) GO TO 10030
      DLON= 89.9999
      GO TO 10031
10030 CONTINUE
      DLON=179.9999
10031 CONTINUE
      DLAT=GRDR
      RLON=REAL(UTPA(5))+DLON
      IDLT=0
      IDRT=-1
      K=CLING(180./DLAT)
      DO 10032 I=1,2
      RLAT=-90.
      CALL MAPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',7).NE.0) RETURN
      DO 10033 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MAPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',8).NE.0) RETURN
10033 CONTINUE
      RLAT=90.
      CALL MAPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',9).NE.0) RETURN
      CALL MAPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',10).NE.0) RETURN
      RLON=REAL(UTPA(5))-DLON
      IDLT=-1
      IDRT=0
10032 CONTINUE
      L10024=    3
      GO TO 10024
10034 CONTINUE
      GO TO 108
10025 CONTINUE
      IF (.NOT.(IPRF.EQ.9)) GO TO 10035
      DLON=GRDR
      RLAT=-.001
      IDLT=-1
      IDRT=0
      K=CLING(180./DLON)
      DO 10036 I=1,2
      RLON=REAL(UTPA(5))+90.
      CALL MAPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',11).NE.0) RETURN
      DO 10037 J=1,K-1
      RLON=RLON+DLON
      CALL MAPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',12).NE.0) RETURN
10037 CONTINUE
      RLON=REAL(UTPA(5))+270.
      CALL MAPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',13).NE.0) RETURN
      CALL MAPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',14).NE.0) RETURN
      RLAT=.001
      IDLT=0
      IDRT=-1
10036 CONTINUE
      L10024=    4
      GO TO 10024
10038 CONTINUE
      GO TO 108
10035 CONTINUE
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12.OR.IPRF.EQ.14.OR.IPRF.EQ.15.OR.
     +IPRF.EQ.23)) GO TO 10039
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12)) GO TO 10040
      CLAT=REAL(UTPA(6))
      CLON=REAL(UTPA(5))
      CRAD=179.95
      GO TO 10041
10040 CONTINUE
      IF (.NOT.(IPRF.EQ.14)) GO TO 10042
      CLAT=REAL(UTPA(6))
      CLON=REAL(UTPA(5))
      CRAD=89.999
      GO TO 10041
10042 CONTINUE
      IF (.NOT.(IPRF.EQ.15)) GO TO 10043
      CLAT=REAL(UTPA(6))
      CLON=REAL(UTPA(5))
      CRAD=RTOD*ACOS(REAL(UTPA(1)/(UTPA(1)+UTPA(3))))-.001
      GO TO 10041
10043 CONTINUE
      IF (.NOT.(IPRF.EQ.23)) GO TO 10044
      CLAT=  64.
      CLON=-152.
      CRAD=  29.999
10041 CONTINUE
10044 CONTINUE
      CALL NGGCOG (CLAT,CLON,CRAD,TLAT,TLON,361)
      CALL MAPITA (TLAT(1),TLON(1),0,IAMP,IGRP,0,-1)
      IF (ICFELL('MAPBLA',15).NE.0) RETURN
      DO 10045 I=2,360
      CALL MAPITA (TLAT(I),TLON(I),1,IAMP,IGRP,0,-1)
      IF (ICFELL('MAPBLA',16).NE.0) RETURN
10045 CONTINUE
      CALL MAPITA (TLAT(361),TLON(361),2,IAMP,IGRP,0,-1)
      IF (ICFELL('MAPBLA',17).NE.0) RETURN
      CALL MAPIQA (                      IAMP,IGRP,0,-1)
      IF (ICFELL('MAPBLA',18).NE.0) RETURN
      GO TO 108
10039 CONTINUE
      IF (.NOT.(IPRF.EQ.20)) GO TO 10046
      ALPH=.017453292519943*(180.-REAL(UTPA(4)))
      CALL MAPTRN ( 90.,0.,XANP,YANP)
      IF (ICFELL('MAPBLA',19).NE.0) RETURN
      CALL MAPTRN (-90.,0.,XAS1,YAS1)
      IF (ICFELL('MAPBLA',20).NE.0) RETURN
      UNS1=(XAS1-XANP)*COS(ALPH)+(YAS1-YANP)*SIN(ALPH)
      VNS1=(YAS1-YANP)*COS(ALPH)-(XAS1-XANP)*SIN(ALPH)
      XAS2=XANP+VNS1*SIN(ALPH)+UNS1*COS(ALPH)
      YAS2=YANP+UNS1*SIN(ALPH)-VNS1*COS(ALPH)
      DIST=SQRT((XAS2-XAS1)*(XAS2-XAS1)+(YAS2-YAS1)*(YAS2-YAS1))
      IF (.NOT.(VNS1.LT.0.)) GO TO 10047
      DEPS=-.001*DIST
      IDLT=-1
      IDRT= 0
      GO TO 10048
10047 CONTINUE
      DEPS=+.001*DIST
      IDLT= 0
      IDRT=-1
10048 CONTINUE
      DIST=2.*DIST
      XCR(1)=XAS1-DIST*COS(ALPH)+DEPS*SIN(ALPH)
      YCR(1)=YAS1-DIST*SIN(ALPH)-DEPS*COS(ALPH)
      XCR(2)=XAS1+DIST*COS(ALPH)+DEPS*SIN(ALPH)
      YCR(2)=YAS1+DIST*SIN(ALPH)-DEPS*COS(ALPH)
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IDRT,IDLT)
      IF (ICFELL('MAPBLA',21).NE.0) RETURN
      XCR(1)=XAS2-DIST*COS(ALPH)-DEPS*SIN(ALPH)
      YCR(1)=YAS2-DIST*SIN(ALPH)+DEPS*COS(ALPH)
      XCR(2)=XAS2+DIST*COS(ALPH)-DEPS*SIN(ALPH)
      YCR(2)=YAS2+DIST*SIN(ALPH)+DEPS*COS(ALPH)
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',22).NE.0) RETURN
      GO TO 108
10046 CONTINUE
      GO TO 108
C
C Lambert conformal conic.
C
  101 DLAT=GRDR
      RLON=PHOC+179.9999
      IDLT=0
      IDRT=-1
      K=CLING(180./DLAT)
      DO 10049 I=1,2
      RLAT=-90.
      CALL MAPITA (RLAT,RLON,0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',23).NE.0) RETURN
      DO 10050 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MAPITA (RLAT,RLON,1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',24).NE.0) RETURN
10050 CONTINUE
      RLAT=90.
      CALL MAPITA (RLAT,RLON,2,IAMP,IGRP,IDLT,IDRT)
      CALL MAPIQA (            IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',25).NE.0) RETURN
      RLON=PHOC-179.9999
      IDLT=-1
      IDRT=0
10049 CONTINUE
      L10024=    5
      GO TO 10024
10051 CONTINUE
      GO TO 108
C
C Orthographic (or satellite-view).
C
  102 CONTINUE
      IF (.NOT.(ABS(SALT).LE.1..OR.ALFA.EQ.0.)) GO TO 10052
      URAD=1.
      RVTU=1.
      GO TO 106
10052 CONTINUE
      SSLT=SALT
      SALT=-ABS(SALT)
      IDLT=-1
      IDRT=0
      IF (.NOT.(IROD.EQ.0)) GO TO 10053
      R=.9998
10054 CONTINUE
      IPEN=0
      DO 10055 I=1,361
      RCOSB=COS(DTOR*REAL(I-1))
      RSINB=SIN(DTOR*REAL(I-1))
      IF (.NOT.(R.LT.1.)) GO TO 10056
      RCOSA=(R*R*ABS(SALT)+SSMO*SQRT(1.-R*R))/(R*R+SSMO)
      GO TO 10057
10056 CONTINUE
      S=2.-R
      RCOSA=(S*S*ABS(SALT)-SSMO*SQRT(1.-S*S))/(S*S+SSMO)
10057 CONTINUE
      RSINA=SQRT(1.-RCOSA*RCOSA)
      RSINPH=RSINA*RSINB
      RCOSPH=RCOSA*RCSO-RSINA*RSNO*RCOSB
      RCOSLA=SQRT(RSINPH*RSINPH+RCOSPH*RCOSPH)
      IF (.NOT.(RCOSLA.NE.0.)) GO TO 10058
      RSINPH=RSINPH/RCOSLA
      RCOSPH=RCOSPH/RCOSLA
10058 CONTINUE
      IF (.NOT.(ABS(RSNO).GT.1.E-4)) GO TO 10059
      RSINLA=(RCOSA-RCOSLA*RCOSPH*RCSO)/RSNO
      GO TO 10060
10059 CONTINUE
      RSINLA=RSINA*RCOSB
10060 CONTINUE
      RLAT=RTOD*ATAN2(RSINLA,RCOSLA)
      RLON=PHOC+RTOD*ATAN2(RSINA*RSINB,
     +                     RCOSA*RCSO-RSINA*RSNO*RCOSB)
      IF (ABS(RLON).GT.180.) RLON=RLON-SIGN(360.,RLON)
      CALL MAPITA (RLAT,RLON,IPEN,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',26).NE.0) RETURN
      IPEN=1
      IF (I.EQ.360) IPEN=2
10055 CONTINUE
      L10024=    6
      GO TO 10024
10061 CONTINUE
      IF (R.EQ.1.0002) GO TO 10062
      R=1.0002
      GO TO 10054
10062 CONTINUE
      GO TO 10063
10053 CONTINUE
      DSALT=DBLE(ABS(SALT))
      DSSMO=DSALT*DSALT-1.D0
      DR=.9998D0
10064 CONTINUE
      IPEN=0
      DO 10065 I=1,361
      DCOSB=COS(DBLE(DTOR*REAL(I-1)))
      DSINB=SIN(DBLE(DTOR*REAL(I-1)))
      IF (.NOT.(DR.LT.1.D0)) GO TO 10066
      DCOSA=(DR*DR*DSALT+DSSMO*SQRT(1.D0-DR*DR))/
     +                             (DR*DR+DSSMO)
      GO TO 10067
10066 CONTINUE
      DS=2.D0-DR
      DCOSA=(DS*DS*DSALT-DSSMO*SQRT(1.D0-DS*DS))/
     +                             (DS*DS+DSSMO)
10067 CONTINUE
      DSINA=SQRT(1.D0-DCOSA*DCOSA)
      DSINPH=DSINA*DSINB
      DCOSPH=DCOSA*DCSO-DSINA*DSNO*DCOSB
      DCOSLA=SQRT(DSINPH*DSINPH+DCOSPH*DCOSPH)
      IF (.NOT.(DCOSLA.NE.0.D0)) GO TO 10068
      DSINPH=DSINPH/DCOSLA
      DCOSPH=DCOSPH/DCOSLA
10068 CONTINUE
      IF (.NOT.(ABS(DSNO).GT.1.D-4)) GO TO 10069
      DSINLA=(DCOSA-DCOSLA*DCOSPH*DCSO)/DSNO
      GO TO 10070
10069 CONTINUE
      DSINLA=DSINA*DCOSB
10070 CONTINUE
      RLAT=RTOD*REAL(ATAN2(DSINLA,DCOSLA))
      RLON=PHOC+RTOD*REAL(ATAN2(DSINA*DSINB,
     +                    DCOSA*DCSO-DSINA*DSNO*DCOSB))
      IF (ABS(RLON).GT.180.) RLON=RLON-SIGN(360.,RLON)
      CALL MAPITA (RLAT,RLON,IPEN,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',27).NE.0) RETURN
      IPEN=1
      IF (I.EQ.360) IPEN=2
10065 CONTINUE
      L10024=    7
      GO TO 10024
10071 CONTINUE
      IF (DR.EQ.1.0002D0) GO TO 10072
      DR=1.0002D0
      GO TO 10064
10072 CONTINUE
10063 CONTINUE
      SALT=SSLT
      GO TO 108
C
C Lambert equal-area.  Note:  The constant "1.999999500000" is the real
C effective radius of the limb of the Lambert equal area projection, as
C determined by the test at statement number 106 in the routine MAPTRN.
C
  103 URAD=1.999999500000
      RVTU=1.
      GO TO 106
C
C Azimuthal equidistant.  Note:  The constant "3.140178439909" is the
C real effective radius of the limb of the azimuthal equidistant
C projection, as determined by the test at statement number 108 in the
C routine MAPTRN.
C
  104 URAD=3.140178439909
      RVTU=1.
      GO TO 106
C
C Mollweide.
C
  105 URAD=2.
      RVTU=0.5
C
  106 IF (ELPF.AND.ABS(UCEN).LT.1.E-4.AND.
     +             ABS(VCEN).LT.1.E-4.AND.
     +             ABS(URNG-URAD).LT.1.E-4.AND.
     +             ABS(VRNG/URNG-RVTU).LT.1.E-4) GO TO 108
C
      TEMP=.9998
C
10073 CONTINUE
      IDLT=0
      IDRT=-1
      IVIS=-1
      I = 1
      GO TO 10076
10074 CONTINUE
      I =I +1
10076 CONTINUE
      IF (I .GT.(361)) GO TO 10075
      UCIR=TEMP*URAD*COS(DTOR*REAL(I-1))
      VCIR=TEMP*URAD*SIN(DTOR*REAL(I-1))
      U=UCIR
      V=RVTU*VCIR
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10077
      IF (.NOT.(IVIS.EQ.1)) GO TO 10078
      CALL MAPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   10
      GO TO 10010
10079 CONTINUE
10078 CONTINUE
      IVIS=0
      GO TO 10080
10077 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.)))
     +GO TO 10081
      IF (.NOT.(IVIS.EQ.1)) GO TO 10082
      CALL MAPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   11
      GO TO 10010
10083 CONTINUE
10082 CONTINUE
      IVIS=0
      GO TO 10080
10081 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10084
      XCRD=U
      YCRD=V
      L10005=    4
      GO TO 10005
10085 CONTINUE
      IVIS=1
      GO TO 10086
10084 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10087
      IF (.NOT.ELPF) CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    5
      GO TO 10005
10088 CONTINUE
      IVIS=1
10087 CONTINUE
      XCRD=U
      YCRD=V
      L10010=   12
      GO TO 10010
10089 CONTINUE
10086 CONTINUE
10080 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10074
10075 CONTINUE
      L10024=    8
      GO TO 10024
10090 CONTINUE
      IF (TEMP.EQ.1.0002) GO TO 10091
      TEMP=1.0002
      GO TO 10073
10091 CONTINUE
      GO TO 108
C
C Cylindrical equidistant, Mercator, or Robinson.
C
  107 TEMP=.9998
C
10092 CONTINUE
      IDLT=0
      IDRT=-1
      IVIS=-1
      RLAT=-90.
      RLON=-180.
      I = 1
      GO TO 10095
10093 CONTINUE
      I =I +1
10095 CONTINUE
      IF (I .GT.(361)) GO TO 10094
      IF (.NOT.(IPRJ.EQ.7.OR.IPRJ.EQ.11)) GO TO 10096
      U=TEMP*RLON
      V=TEMP*RLAT
      GO TO 10097
10096 CONTINUE
      IF (.NOT.(IPRJ.EQ.8.OR.IPRJ.EQ.12)) GO TO 10098
      U=TEMP*DTOR*RLON
      V=TEMP*LOG(TAN((MAX(-89.9999,
     +                MIN(+89.9999,RLAT))+90.)*DTRH))
      GO TO 10097
10098 CONTINUE
      U=TEMP*(RLON/180.)*RBGLEN(RLAT)
      V=TEMP*RBGDFE(RLAT)
10097 CONTINUE
      IF (.NOT.(I.LE.90)) GO TO 10099
      RLON=RLON+4.
      GO TO 10100
10099 CONTINUE
      IF (.NOT.(I.LE.180)) GO TO 10101
      RLAT=RLAT+2.
      GO TO 10100
10101 CONTINUE
      IF (.NOT.(I.LE.270)) GO TO 10102
      RLON=RLON-4.
      GO TO 10100
10102 CONTINUE
      IF (.NOT.(I.LE.360)) GO TO 10103
      RLAT=RLAT-2.
10100 CONTINUE
10103 CONTINUE
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10104
      IF (.NOT.(IVIS.EQ.1)) GO TO 10105
      CALL MAPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   13
      GO TO 10010
10106 CONTINUE
10105 CONTINUE
      IVIS=0
      GO TO 10107
10104 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.)))
     +GO TO 10108
      IF (.NOT.(IVIS.EQ.1)) GO TO 10109
      CALL MAPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   14
      GO TO 10010
10110 CONTINUE
10109 CONTINUE
      IVIS=0
      GO TO 10107
10108 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10111
      XCRD=U
      YCRD=V
      L10005=    6
      GO TO 10005
10112 CONTINUE
      IVIS=1
      GO TO 10113
10111 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10114
      IF (.NOT.ELPF) CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    7
      GO TO 10005
10115 CONTINUE
      IVIS=1
10114 CONTINUE
      XCRD=U
      YCRD=V
      L10010=   15
      GO TO 10010
10116 CONTINUE
10113 CONTINUE
10107 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10093
10094 CONTINUE
      L10024=    9
      GO TO 10024
10117 CONTINUE
      IF (TEMP.EQ.1.0002) GO TO 10118
      TEMP=1.0002
      GO TO 10092
10118 CONTINUE
C
  108 CONTINUE
      IF (IGRP.EQ.IGI2.OR.NOVS.LE.0) GO TO 10119
C
      IPSS=2
      IGRP=IGI2
C
      GO TO 10001
10119 CONTINUE
C
C Add lines to group 2 to create vertical strips.
C
      IF (.NOT.(NOVS.GT.1)) GO TO 10120
C
      IDLT=0
      IDRT=0
C
      YCR(1)=VMIN
      YCR(2)=VMAX
C
      DO 10121 I=1,NOVS-1
      XCR(1)=UMIN+(FLOAT(I)/FLOAT(NOVS))*(UMAX-UMIN)
      XCR(2)=XCR(1)
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',28).NE.0) RETURN
10121 CONTINUE
C
10120 CONTINUE
C
C If the selected outline type is "NONE", quit; no outlines need be
C added to the area map.
C
      IF (NOUT.LE.0) RETURN
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window.
C
      IWGF=0
      IF (BLAM-SLAM.GT.179.9999.AND.BLOM-SLOM.GT.359.9999) IWGF=1
C
C Position to the user-selected portion of the outline dataset.
C
      IGRP=IGI1
      CALL MAPIO (1)
      IF (ICFELL('MAPBLA',29).NE.0) RETURN
      NSEG=0
C
C Save the pointer that will tell us whether anything actually got
C put into the area map, so that, if not, we can take remedial action.
C
      IAM5=IAMP(5)
C
C Read the next record (group of points).
C
  301 CALL MAPIO (2)
      IF (ICFELL('MAPBLA',30).NE.0) RETURN
      IDLT=IDOS(NOUT)+IDLS
      IDRT=IDOS(NOUT)+IDRS
      NSEG=NSEG+1
C
C If the end of the desired data has been reached, quit reading.
C
      IF (NPTS.EQ.0) GO TO 302
C
C If less than the whole globe is shown by the projection, do a quick
C check for intersection of the box surrounding the point group with
C the area shown.
C
      IF (.NOT.(IWGF.EQ.0)) GO TO 10122
      IF (SLAG.GT.BLAM.OR.BLAG.LT.SLAM) GO TO 301
      IF ((SLOG     .GT.BLOM.OR.BLOG     .LT.SLOM).AND.
     +    (SLOG-360..GT.BLOM.OR.BLOG-360..LT.SLOM).AND.
     +    (SLOG+360..GT.BLOM.OR.BLOG+360..LT.SLOM)) GO TO 301
10122 CONTINUE
C
C See if the user wants to omit this point group.
C
      CALL HLUMAPEOD (NOUT,NSEG,IDLT,IDRT,NPTS,PNTS)
      IF (ICFELL('MAPBLA',31).NE.0) RETURN
      IF (NPTS.LE.1) GO TO 301
C
C Put the group into the area map.
C
      CALL MAPITA (PNTS(1),PNTS(2),0,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',32).NE.0) RETURN
C
      DO 10123 K=2,NPTS-1
      CALL MAPITA (PNTS(2*K-1),PNTS(2*K),1,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',33).NE.0) RETURN
10123 CONTINUE
C
      CALL MAPITA (PNTS(2*NPTS-1),PNTS(2*NPTS),2,IAMP,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',34).NE.0) RETURN
C
C Force a buffer dump.
C
      L10024=   10
      GO TO 10024
10124 CONTINUE
C
C Go get another group.
C
      GO TO 301
C
C See if anything was actually put into the area map and, if not, take
C action to supply AREAS with a correct area identifier.
C
  302 CONTINUE
      IF (.NOT.(IAMP(5).EQ.IAM5)) GO TO 10125
      CALL MPDBDI (FLNM,ISTA)
      IF (ISTA.EQ.-1) GO TO 309
      DO 303 I=1,111
      IF (.NOT.(FLNM(I:I).EQ.CHAR(0))) GO TO 10126
      FLNM(I:I+17)='/EzmapAreaInfo.'//DDCT(NOUT+1)//CHAR(0)
      GO TO 304
10126 CONTINUE
  303 CONTINUE
      GO TO 309
  304 CALL NGOFRO (FLNM,IFDE,ISTA)
      IF (ISTA.NE.0) GO TO 309
      NTMS=0
      MCHR=0
      NCHR=0
      DO 307 IDIV=0,9
      NROW=2**IDIV
      NCOL=2*NROW
      DO 306 J=1,NROW
      RLAT=-90.+(REAL(J)-.5)*(180./REAL(NROW))
      DO 305 I=1,NCOL
      RLON=-180.+(REAL(I)-.5)*(360./REAL(NCOL))
      IF (.NOT.(NTMS.EQ.0)) GO TO 10127
      CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,NTMS)
      IF (MCHR.EQ.0) GO TO 308
      CALL MPRDNM (IFDE,CHRS,512,MCHR,NCHR,IAID)
      IF (MCHR.EQ.0) GO TO 308
10127 CONTINUE
      CALL MAPTRA (RLAT,RLON,UVAL,VVAL)
      IF (.NOT.(UVAL.NE.1.E12)) GO TO 10128
      XCR(1)=UMIN
      XCR(2)=UMAX
      YCR(1)=VMIN
      YCR(2)=VMAX
      CALL AREDAM (IAMP,XCR,YCR,2,IGRP,IAID,IAID)
      GO TO 308
10128 CONTINUE
      NTMS=NTMS-1
  305 CONTINUE
  306 CONTINUE
  307 CONTINUE
  308 CALL NGCLFI (IFDE)
C
10125 CONTINUE
C
C Done.
C
  309 RETURN
C
C The following internal procedure is invoked to start a line.
C
10005 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10129
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',35).NE.0) RETURN
10129 CONTINUE
      XCRA(1)=XCRD
      YCRA(1)=YCRD
      NCRA=1
      GO TO (10004,10013,10018,10085,10088,10112,10115) , L10005
C
C The following internal procedure is invoked to continue a line.
C
10010 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10130
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',36).NE.0) RETURN
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10130 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=XCRD
      YCRA(NCRA)=YCRD
      GO TO (10009,10014,10015,10016,10017,10019,10020,10021,10022,10079
     +,10083,10089,10106,10110,10116) , L10010
C
C The following internal procedure is invoked to terminate a line.
C
10024 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10131
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPBLA',37).NE.0) RETURN
      NCRA=0
10131 CONTINUE
      GO TO (10023,10029,10034,10038,10051,10061,10071,10090,10117,10124
     +) , L10024
C
      END
