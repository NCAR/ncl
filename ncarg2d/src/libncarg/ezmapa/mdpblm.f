C
C $Id: mdpblm.f,v 1.2 2002-02-25 18:06:20 kennison Exp $
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
      SUBROUTINE MDPBLM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
C
C Declare required common blocks.
C
      COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
      DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
      SAVE   /MAPCM0/
C
      COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
      DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
      INTEGER          IPRJ,IROD
      SAVE   /MAPCM1/
C
      COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
      DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG
      INTEGER          ISSL
      SAVE   /MAPCM2/
C
      COMMON /MAPCM3/  ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,
     +                 SLOG,PNTS(200),IDOS(4)
      INTEGER          ITPN,NOUT,NPTS,IGID,IDLS,IDRS,IDOS
      REAL             BLAG,SLAG,BLOG,SLOG,PNTS
      SAVE   /MAPCM3/
C
      COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                 PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                 ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
      DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                 PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW
      INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
      LOGICAL          ELPF,INTF,LBLF,PRMF
      SAVE   /MAPCM4/
C
      COMMON /MAPCMC/  IGI1,IGI2,NCRA,NOVS,XCRA(100),YCRA(100)
      INTEGER          IGI1,IGI2,NCRA,NOVS
      REAL             XCRA,YCRA
      SAVE   /MAPCMC/
C
      COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
      DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
      SAVE   /MAPSAT/
C
      COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
      DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
      INTEGER IPRF
      SAVE   /USGSC1/
C
C Declare local variables.
C
      INTEGER          I,IGIS,IPEN,IVIS,IWGF,J,K,NSEG
C
      DOUBLE PRECISION ALPH,CLAT,CLON,CRAD,DCOSA,DCOSB,DCOSLA,DCOSPH,
     +                 DEPS,DIST,DLAT,DLON,DR,DS,DSINA,DSINB,DSINLA,
     +                 DSINPH,RLAT,RLON,RVTU,SSLT,TEMP,U,UCIR,UEDG,
     +                 UNS1,UOLD,URAD,V,VCIR,VEDG,VNS1,VOLD,X,XANP,
     +                 XAS1,XAS2,XCRD,YANP,YAS1,YAS2,YCRD
C
C Declare a couple of temporary arrays to hold coordinates of a circle.
C
      DOUBLE PRECISION TLAT(361),TLON(361)
C
C Dimension the arrays needed to define some lines across the map.
C
      REAL             XCR(2),YCR(2)
C
C Declare arithmetic statement functions.
C
      DOUBLE PRECISION CEIL,FLOR
C
C Declare external functions.
C
      DOUBLE PRECISION RBGDFE,RBGLEN
C
C The arithmetic statement functions FLOR and CEIL give, respectively,
C the "floor" of X - the largest integer less than or equal to X - and
C the "ceiling" of X - the smallest integer greater than or equal to X.
C
      FLOR(X)=DINT(X+1.D4)-1.D4
      CEIL(X)=-FLOR(-X)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MDPBLM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MDPINT
      IF (ICFELL('MDPBLM',2).NE.0) RETURN
10000 CONTINUE
C
C If the perimeter is to be drawn ...
C
      IF (.NOT.(PRMF)) GO TO 10001
C
C ... reset the color index and dash pattern for the perimeter ...
C
      CALL MDPCHM (1,IOR(ISHIFT(32767,1),1),
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',3).NE.0) RETURN
C
C .. and draw the perimeter.
C
      IF (.NOT.(ELPF)) GO TO 10002
      TEMP=.9998D0
10003 CONTINUE
      U=URNG
      V=0.D0
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
      U=URNG*COS(DTOR*DBLE(I))
      V=URNG*SIN(DTOR*DBLE(I))
      XCRD=UCEN+TEMP*U
      YCRD=VCEN+TEMP*V*VRNG/URNG
      L10010=    1
      GO TO 10010
10009 CONTINUE
      GO TO 10006
10007 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10011
      TEMP=1.0002D0
      GO TO 10003
10011 CONTINUE
      GO TO 10012
10002 CONTINUE
      XCRD=UMIN
      YCRD=VMIN
      L10005=    2
      GO TO 10005
10013 CONTINUE
      XCRD=UMAX
      YCRD=VMIN
      L10010=    2
      GO TO 10010
10014 CONTINUE
      XCRD=UMAX
      YCRD=VMAX
      L10010=    3
      GO TO 10010
10015 CONTINUE
      XCRD=UMIN
      YCRD=VMAX
      L10010=    4
      GO TO 10010
10016 CONTINUE
      XCRD=UMIN
      YCRD=VMIN
      L10010=    5
      GO TO 10010
10017 CONTINUE
10012 CONTINUE
      L10019=    1
      GO TO 10019
10018 CONTINUE
C
C Restore the color index and dash pattern.
C
      CALL MDPCHM (-1,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',4).NE.0) RETURN
C
10001 CONTINUE
C
C Reset the color index and dash pattern for limb lines.
C
      CALL MDPCHM (4,IOR(ISHIFT(32767,1),1),
     +                                IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',5).NE.0) RETURN
C
C Draw the limb line.
C
C Projection:   US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
      GO TO (100,101,108,102,103,108,104,107,107,105,107,
     +                                   107,107,105,107) , IPRJ+1
C
C USGS transformations.
C
  100 CONTINUE
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.4.OR.IPRF.EQ.5.OR.IPRF.EQ.7.OR.IPRF
     +.EQ.8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.IPRF.EQ.19.OR.I
     +PRF.EQ.21)) GO TO 10020
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.8.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.IP
     +RF.EQ.21)) GO TO 10021
      DLON=GRDR
      RLAT=-89.998D0
      K=CEIL(360.D0/DLON)
      DO 10022 I=1,2
      RLON=UTPA(5)-180.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',6).NE.0) RETURN
      DO 10023 J=1,K-1
      RLON=RLON+DLON
      CALL MDPITM (RLAT,RLON,1,
     +                        IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',7).NE.0) RETURN
10023 CONTINUE
      RLON=UTPA(5)+180.D0
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',8).NE.0) RETURN
      CALL MDPIQM (            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',9).NE.0) RETURN
      RLAT=89.998D0
10022 CONTINUE
      L10019=    2
      GO TO 10019
10024 CONTINUE
10021 CONTINUE
      IF (.NOT.(IPRF.EQ.7)) GO TO 10025
      DLON= 89.999999D0
      GO TO 10026
10025 CONTINUE
      DLON=179.999999D0
10026 CONTINUE
      DLAT=GRDR
      RLON=UTPA(5)+DLON
      K=CEIL(180.D0/DLAT)
      DO 10027 I=1,2
      RLAT=-90.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',10).NE.0) RETURN
      DO 10028 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',11).NE.0) RETURN
10028 CONTINUE
      RLAT=90.D0
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',12).NE.0) RETURN
      CALL MDPIQM (            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',13).NE.0) RETURN
      RLON=UTPA(5)-DLON
10027 CONTINUE
      L10019=    3
      GO TO 10019
10029 CONTINUE
      GO TO 108
10020 CONTINUE
      IF (.NOT.(IPRF.EQ.9)) GO TO 10030
      DLON=GRDR
      RLAT=-.001D0
      K=CEIL(180.D0/DLON)
      DO 10031 I=1,2
      RLON=UTPA(5)+90.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',14).NE.0) RETURN
      DO 10032 J=1,K-1
      RLON=RLON+DLON
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',15).NE.0) RETURN
10032 CONTINUE
      RLON=UTPA(5)+270.D0
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',16).NE.0) RETURN
      CALL MDPIQM (            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',17).NE.0) RETURN
      RLAT=.001D0
10031 CONTINUE
      L10019=    4
      GO TO 10019
10033 CONTINUE
      GO TO 108
10030 CONTINUE
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12.OR.IPRF.EQ.14.OR.IPRF.EQ.15.OR.
     +IPRF.EQ.23)) GO TO 10034
      IF (.NOT.(IPRF.EQ.11.OR.IPRF.EQ.12)) GO TO 10035
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=179.95D0
      GO TO 10036
10035 CONTINUE
      IF (.NOT.(IPRF.EQ.14)) GO TO 10037
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=89.999D0
      GO TO 10036
10037 CONTINUE
      IF (.NOT.(IPRF.EQ.15)) GO TO 10038
      CLAT=UTPA(6)
      CLON=UTPA(5)
      CRAD=RTOD*ACOS(UTPA(1)/(UTPA(1)+UTPA(3)))-.001D0
      GO TO 10036
10038 CONTINUE
      IF (.NOT.(IPRF.EQ.23)) GO TO 10039
      CLAT=  64.D0
      CLON=-152.D0
      CRAD=  29.999D0
10036 CONTINUE
10039 CONTINUE
      CALL MDGCOG (CLAT,CLON,CRAD,TLAT,TLON,361)
      CALL MDPITM (TLAT(1),TLON(1),0,
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',18).NE.0) RETURN
      DO 10040 I=2,360
      CALL MAPITM (TLAT(I),TLON(I),1,
     +                            IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',19).NE.0) RETURN
10040 CONTINUE
      CALL MAPITM (TLAT(361),TLON(361),2,
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',20).NE.0) RETURN
      CALL MAPIQM (                 IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',21).NE.0) RETURN
      GO TO 108
10034 CONTINUE
      IF (.NOT.(IPRF.EQ.20)) GO TO 10041
      ALPH=DTOR*(180.D0-UTPA(4))
      CALL MDPTRN (+90.D0,0.D0,XANP,YANP)
      IF (ICFELL('MDPBLM',22).NE.0) RETURN
      CALL MDPTRN (-90.D0,0.D0,XAS1,YAS1)
      IF (ICFELL('MDPBLM',23).NE.0) RETURN
      UNS1=(XAS1-XANP)*COS(ALPH)+(YAS1-YANP)*SIN(ALPH)
      VNS1=(YAS1-YANP)*COS(ALPH)-(XAS1-XANP)*SIN(ALPH)
      XAS2=XANP+VNS1*SIN(ALPH)+UNS1*COS(ALPH)
      YAS2=YANP+UNS1*SIN(ALPH)-VNS1*COS(ALPH)
      DIST=SQRT((XAS2-XAS1)*(XAS2-XAS1)+(YAS2-YAS1)*(YAS2-YAS1))
      IF (.NOT.(VNS1.LT.0.D0)) GO TO 10042
      DEPS=-.001D0*DIST
      GO TO 10043
10042 CONTINUE
      DEPS=+.001D0*DIST
10043 CONTINUE
      DIST=2.D0*DIST
      XCR(1)=REAL(XAS1-DIST*COS(ALPH)+DEPS*SIN(ALPH))
      YCR(1)=REAL(YAS1-DIST*SIN(ALPH)-DEPS*COS(ALPH))
      XCR(2)=REAL(XAS1+DIST*COS(ALPH)+DEPS*SIN(ALPH))
      YCR(2)=REAL(YAS1+DIST*SIN(ALPH)-DEPS*COS(ALPH))
      CALL ARDRLN (IAM,XCR,YCR,2,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',24).NE.0) RETURN
      XCR(1)=REAL(XAS2-DIST*COS(ALPH)-DEPS*SIN(ALPH))
      YCR(1)=REAL(YAS2-DIST*SIN(ALPH)+DEPS*COS(ALPH))
      XCR(2)=REAL(XAS2+DIST*COS(ALPH)-DEPS*SIN(ALPH))
      YCR(2)=REAL(YAS2+DIST*SIN(ALPH)+DEPS*COS(ALPH))
      CALL ARDRLN (IAM,XCR,YCR,2,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',25).NE.0) RETURN
      GO TO 108
10041 CONTINUE
      GO TO 108
C
C Lambert conformal conic.
C
  101 DLAT=GRDR
      RLON=PHOC+179.999999D0
      K=CEIL(180.D0/DLAT)
      DO 10044 I=1,2
      RLAT=-90.D0
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',26).NE.0) RETURN
      DO 10045 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',27).NE.0) RETURN
10045 CONTINUE
      RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',28).NE.0) RETURN
      RLON=PHOC-179.999999D0
10044 CONTINUE
      L10019=    5
      GO TO 10019
10046 CONTINUE
      GO TO 108
C
C Orthographic (or satellite-view).
C
  102 CONTINUE
      IF (.NOT.(ABS(SALT).LE.1.D0.OR.ALFA.EQ.0.D0)) GO TO 10047
      URAD=1.D0
      RVTU=1.D0
      GO TO 106
10047 CONTINUE
      SSLT=SALT
      SALT=-ABS(SALT)
      DR=.9998D0
10048 CONTINUE
      IPEN=0
      DO 10049 I=1,361
      DCOSB=COS(DTOR*DBLE(I-1))
      DSINB=SIN(DTOR*DBLE(I-1))
      IF (.NOT.(DR.LT.1.D0)) GO TO 10050
      DCOSA=(DR*DR*ABS(SALT)+SSMO*SQRT(1.D0-DR*DR))/
     +                                  (DR*DR+SSMO)
      GO TO 10051
10050 CONTINUE
      DS=2.D0-DR
      DCOSA=(DS*DS*ABS(SALT)-SSMO*SQRT(1.D0-DS*DS))/
     +                                  (DS*DS+SSMO)
10051 CONTINUE
      DSINA=SQRT(1.D0-DCOSA*DCOSA)
      DSINPH=DSINA*DSINB
      DCOSPH=DCOSA*COSO-DSINA*SINO*DCOSB
      DCOSLA=SQRT(DSINPH*DSINPH+DCOSPH*DCOSPH)
      IF (.NOT.(DCOSLA.NE.0.D0)) GO TO 10052
      DSINPH=DSINPH/DCOSLA
      DCOSPH=DCOSPH/DCOSLA
10052 CONTINUE
      IF (.NOT.(ABS(SINO).GT..000001D0)) GO TO 10053
      DSINLA=(DCOSA-DCOSLA*DCOSPH*COSO)/SINO
      GO TO 10054
10053 CONTINUE
      DSINLA=DSINA*DCOSB
10054 CONTINUE
      RLAT=RTOD*ATAN2(DSINLA,DCOSLA)
      RLON=PHOC+RTOD*ATAN2(DSINA*DSINB,
     +                     DCOSA*COSO-DSINA*SINO*DCOSB)
      IF (ABS(RLON).GT.180.D0) RLON=RLON-SIGN(360.D0,RLON)
      CALL MDPITM (RLAT,RLON,IPEN,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',29).NE.0) RETURN
      IPEN=1
      IF (I.EQ.360) IPEN=2
10049 CONTINUE
      L10019=    6
      GO TO 10019
10055 CONTINUE
      IF (DR.EQ.1.0002D0) GO TO 10056
      DR=1.0002D0
      GO TO 10048
10056 CONTINUE
      SALT=SSLT
      GO TO 108
C
C Lambert equal-area.  Note:  The constant "1.999999500000" is the real
C effective radius of the limb of the Lambert equal area projection, as
C determined by the test at statement number 106 in the routine MDPTRN.
C
  103 URAD=1.999999500000D0
      RVTU=1.D0
      GO TO 106
C
C Azimuthal equidistant.  Note:  The constant "3.140178439909" is the
C real effective radius of the limb of the azimuthal equidistant
C projection, as determined by the test at statement number 108 in the
C routine MDPTRN.
C
  104 URAD=3.140178439909D0
      RVTU=1.D0
      GO TO 106
C
C Mollweide.
C
  105 URAD=2.D0
      RVTU=0.5D0
C
  106 IF (ELPF.AND.ABS(UCEN).LT..0001D0.AND.
     +             ABS(VCEN).LT..0001D0.AND.
     +             ABS(URNG-URAD).LT..0001D0.AND.
     +             ABS(VRNG/URNG-RVTU).LT..0001D0) GO TO 108
C
      TEMP=.9998D0
C
10057 CONTINUE
      IVIS=-1
      I = 1
      GO TO 10060
10058 CONTINUE
      I =I +1
10060 CONTINUE
      IF (I .GT.(361)) GO TO 10059
      UCIR=TEMP*URAD*COS(DTOR*DBLE(I-1))
      VCIR=TEMP*URAD*SIN(DTOR*DBLE(I-1))
      U=UCIR-UOFF
      V=RVTU*VCIR-VOFF
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10061
      IF (.NOT.(IVIS.EQ.1)) GO TO 10062
      CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    6
      GO TO 10010
10063 CONTINUE
10062 CONTINUE
      IVIS=0
      GO TO 10064
10061 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)
     +)) GO TO 10065
      IF (.NOT.(IVIS.EQ.1)) GO TO 10066
      CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    7
      GO TO 10010
10067 CONTINUE
10066 CONTINUE
      IVIS=0
      GO TO 10064
10065 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10068
      XCRD=U
      YCRD=V
      L10005=    3
      GO TO 10005
10069 CONTINUE
      IVIS=1
      GO TO 10070
10068 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10071
      IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    4
      GO TO 10005
10072 CONTINUE
      IVIS=1
10071 CONTINUE
      XCRD=U
      YCRD=V
      L10010=    8
      GO TO 10010
10073 CONTINUE
10070 CONTINUE
10064 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10058
10059 CONTINUE
      L10019=    7
      GO TO 10019
10074 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10075
      TEMP=1.0002D0
      GO TO 10057
10075 CONTINUE
      GO TO 108
C
C Cylindrical equidistant, Mercator, or Robinson.
C
  107 TEMP=.9998D0
C
10076 CONTINUE
      IVIS=-1
      RLAT=-90.D0
      RLON=-180.D0
      I = 1
      GO TO 10079
10077 CONTINUE
      I =I +1
10079 CONTINUE
      IF (I .GT.(361)) GO TO 10078
      IF (.NOT.(IPRJ.EQ.7.OR.IPRJ.EQ.11)) GO TO 10080
      U=TEMP*RLON-UOFF
      V=TEMP*RLAT-VOFF
      GO TO 10081
10080 CONTINUE
      IF (.NOT.(IPRJ.EQ.8.OR.IPRJ.EQ.12)) GO TO 10082
      U=TEMP*DTOR*RLON-UOFF
      V=TEMP*LOG(TAN((MAX(-89.999999D0,
     +                MIN(+89.999999D0,RLAT))+90.D0)*DTRH))-VOFF
      GO TO 10081
10082 CONTINUE
      U=TEMP*(RLON/180.D0)*RBGLEN(RLAT)-UOFF
      V=TEMP*RBGDFE(RLAT)-VOFF
10081 CONTINUE
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10083
      IF (.NOT.(IVIS.EQ.1)) GO TO 10084
      CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    9
      GO TO 10010
10085 CONTINUE
10084 CONTINUE
      IVIS=0
      GO TO 10086
10083 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)
     +)) GO TO 10087
      IF (.NOT.(IVIS.EQ.1)) GO TO 10088
      CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=   10
      GO TO 10010
10089 CONTINUE
10088 CONTINUE
      IVIS=0
      GO TO 10086
10087 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10090
      XCRD=U
      YCRD=V
      L10005=    5
      GO TO 10005
10091 CONTINUE
      IVIS=1
      GO TO 10092
10090 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10093
      IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    6
      GO TO 10005
10094 CONTINUE
      IVIS=1
10093 CONTINUE
      XCRD=U
      YCRD=V
      L10010=   11
      GO TO 10010
10095 CONTINUE
10092 CONTINUE
10086 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10077
10078 CONTINUE
      L10019=    8
      GO TO 10019
10096 CONTINUE
      IF (TEMP.EQ.1.0002D0) GO TO 10097
      TEMP=1.0002D0
      GO TO 10076
10097 CONTINUE
C
C Restore the color index and dash pattern.
C
  108 CALL MDPCHM (-4,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',30).NE.0) RETURN
C
C If the selected outline type is "NONE", quit; no outlines need to
C be drawn.
C
      IF (NOUT.LE.0) RETURN
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window.
C
      IWGF=0
      IF (BLAM-SLAM.GT.179.D0.AND.BLOM-SLOM.GT.359.D0) IWGF=1
C
C IGIS keeps track of changes in the group identifier, so that the
C color index can be changed when necessary.
C
      IGIS=0
C
C Position to the user-selected portion of the outline dataset.
C
      CALL MDPIO (1)
      IF (ICFELL('MDPBLM',31).NE.0) RETURN
      NSEG=0
C
C Read the next record (group of points).
C
  301 CALL MDPIO (2)
      IF (ICFELL('MDPBLM',32).NE.0) RETURN
      NSEG=NSEG+1
C
C Check for the end of the desired data.
C
      IF (NPTS.EQ.0) GO TO 303
C
C If less than the whole globe is shown by the projection, do a quick
C check for intersection of the box surrounding the point group with
C the area shown.
C
      IF (.NOT.(IWGF.EQ.0)) GO TO 10098
      IF (DBLE(SLAG).GT.BLAM.OR.DBLE(BLAG).LT.SLAM) GO TO 301
      IF ((DBLE(SLOG     ).GT.BLOM.OR.
     +     DBLE(BLOG     ).LT.SLOM).AND.
     +    (DBLE(SLOG-360.).GT.BLOM.OR.
     +     DBLE(BLOG-360.).LT.SLOM).AND.
     +    (DBLE(SLOG+360.).GT.BLOM.OR.
     +     DBLE(BLOG+360.).LT.SLOM)) GO TO 301
10098 CONTINUE
C
C See if the user wants to omit this point group.
C
      CALL HLUMAPEOD (NOUT,NSEG,IDOS(NOUT)+IDLS,
     +                          IDOS(NOUT)+IDRS,NPTS,PNTS)
      IF (ICFELL('MDPBLM',33).NE.0) RETURN
      IF (NPTS.LE.1) GO TO 301
C
C If we've switched to a new group, set the color index, dotting, and
C dash pattern for the group.
C
      IF (.NOT.(IGID.NE.IGIS)) GO TO 10099
      IF (.NOT.(IGIS.NE.0)) GO TO 10100
      CALL MDPCHM (-4-IGIS,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',34).NE.0) RETURN
10100 CONTINUE
      CALL MDPCHM (4+IGID,IOR(ISHIFT(32767,1),1),
     +                              IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',35).NE.0) RETURN
      IGIS=IGID
10099 CONTINUE
C
C Plot the group.
C
      CALL MAPITM (PNTS(1),PNTS(2),0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',36).NE.0) RETURN
C
      DO 10101 K=2,NPTS-1
      CALL MAPITM (PNTS(2*K-1),PNTS(2*K),1,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',37).NE.0) RETURN
10101 CONTINUE
C
      CALL MAPITM (PNTS(2*NPTS-1),PNTS(2*NPTS),2,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',38).NE.0) RETURN
C
C Force a buffer dump.
C
      L10019=    9
      GO TO 10019
10102 CONTINUE
C
C Go get another group.
C
      GO TO 301
C
C Reset the color index, dotting, and dash pattern, if necessary.
C
  303 CONTINUE
      IF (.NOT.(IGIS.NE.0)) GO TO 10103
      CALL MDPCHM (-4-IGIS,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',39).NE.0) RETURN
10103 CONTINUE
C
C Done.
C
      RETURN
C
C The following internal procedure is invoked to start a line.
C
10005 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10104
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',40).NE.0) RETURN
10104 CONTINUE
      XCRA(1)=REAL(XCRD)
      YCRA(1)=REAL(YCRD)
      NCRA=1
      GO TO (10004,10013,10069,10072,10091,10094) , L10005
C
C The following internal procedure is invoked to continue a line.
C
10010 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10105
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',41).NE.0) RETURN
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10105 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=REAL(XCRD)
      YCRA(NCRA)=REAL(YCRD)
      GO TO (10009,10014,10015,10016,10017,10063,10067,10073,10085,10089
     +,10095) , L10010
C
C The following internal procedure is invoked to terminate a line.
C
10019 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10106
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPBLM',42).NE.0) RETURN
      NCRA=0
10106 CONTINUE
      GO TO (10018,10024,10029,10033,10046,10055,10074,10096,10102) , L1
     +0019
C
      END
