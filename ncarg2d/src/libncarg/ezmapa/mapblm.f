C
C $Id: mapblm.f,v 1.1 1994-04-07 21:02:29 kennison Exp $
C
      SUBROUTINE MAPBLM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      DIMENSION IAM(*)
C
C Declare required common blocks.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE /MAPCM1/
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE /MAPDP1/
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE /MAPCM2/
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE /MAPCM3/
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
C
C Define some required double-precision variables.
C
      DOUBLE PRECISION DR,DS,DCOSB,DSINB,DCOSA,DSINA,DCOSPH,DSINPH,
     +                 DCOSLA,DSINLA
C
C Dimension the arrays needed to define some lines across the map.
C
      DIMENSION XCR(2),YCR(2)
C
C Define required constants.
C
      DATA DTOR / .017453292519943 /
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
      IF (.NOT.(ICFELL('MAPBLM - UNCLEARED PRIOR ERROR',1).NE.0))
     +GO TO 10000
      IIER=-1
      RETURN
10000 CONTINUE
C
C If EZMAP needs initialization or if an error has occurred since the
C last initialization, do nothing.
C
      IF (INTF) RETURN
      IF (IIER.NE.0) RETURN
C
C Put the perimeter and the limb line into the area map (in group 1 and,
C perhaps, in group 2).
C
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
C Limb line.
C
      GO TO (101,107,102,103,107,104,107,107,105,107,107,105) , IPRJ
C
  101 DLAT=GRDR
      RLON=PHIO+179.9999
      IDLT=0
      IDRT=-1
      K=CLING(180./DLAT)
      DO 10020 I=1,2
      RLAT=-90.
      CALL MAPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',2).NE.0) RETURN
      DO 10021 J=1,K-1
      RLAT=RLAT+DLAT
      CALL MAPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',3).NE.0) RETURN
10021 CONTINUE
      RLAT=RLAT+DLAT
      CALL MAPITM (RLAT,RLON,2,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',4).NE.0) RETURN
      RLON=PHIO-179.9999
      IDLT=-1
      IDRT=0
10020 CONTINUE
      L10019=    2
      GO TO 10019
10022 CONTINUE
      GO TO 107
C
  102 CONTINUE
      IF (.NOT.(ABS(SALT).LE.1..OR.ALFA.EQ.0.)) GO TO 10023
      URAD=1.
      RVTU=1.
      GO TO 106
10023 CONTINUE
      SSLT=SALT
      SALT=-ABS(SALT)
      IDLT=-1
      IDRT=0
      IF (.NOT.(IROD.EQ.0)) GO TO 10024
      R=.9998
10025 CONTINUE
      IPEN=0
      DO 10026 I=1,361
      RCOSB=COS(DTOR*REAL(I-1))
      RSINB=SIN(DTOR*REAL(I-1))
      IF (.NOT.(R.LT.1.)) GO TO 10027
      RCOSA=(R*R*ABS(SALT)+SSMO*SQRT(1.-R*R))/(R*R+SSMO)
      GO TO 10028
10027 CONTINUE
      S=2.-R
      RCOSA=(S*S*ABS(SALT)-SSMO*SQRT(1.-S*S))/(S*S+SSMO)
10028 CONTINUE
      RSINA=SQRT(1.-RCOSA*RCOSA)
      RSINPH=RSINA*RSINB
      RCOSPH=RCOSA*RCSO-RSINA*RSNO*RCOSB
      RCOSLA=SQRT(RSINPH*RSINPH+RCOSPH*RCOSPH)
      IF (.NOT.(RCOSLA.NE.0.)) GO TO 10029
      RSINPH=RSINPH/RCOSLA
      RCOSPH=RCOSPH/RCOSLA
10029 CONTINUE
      IF (.NOT.(ABS(RSNO).GT.1.E-4)) GO TO 10030
      RSINLA=(RCOSA-RCOSLA*RCOSPH*RCSO)/RSNO
      GO TO 10031
10030 CONTINUE
      RSINLA=RSINA*RCOSB
10031 CONTINUE
      RLAT=RTOD*ATAN2(RSINLA,RCOSLA)
      RLON=PHOC+RTOD*ATAN2(RSINA*RSINB,
     +                     RCOSA*RCSO-RSINA*RSNO*RCOSB)
      IF (ABS(RLON).GT.180.) RLON=RLON-SIGN(360.,RLON)
      CALL MAPITM (RLAT,RLON,IPEN,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',5).NE.0) RETURN
      IPEN=1
      IF (I.EQ.360) IPEN=2
10026 CONTINUE
      L10019=    3
      GO TO 10019
10032 CONTINUE
      IF (R.EQ.1.0002) GO TO 10033
      R=1.0002
      GO TO 10025
10033 CONTINUE
      GO TO 10034
10024 CONTINUE
      DSALT=DBLE(ABS(SALT))
      DSSMO=DSALT*DSALT-1.D0
      DR=.9998D0
10035 CONTINUE
      IPEN=0
      DO 10036 I=1,361
      DCOSB=COS(DBLE(DTOR*REAL(I-1)))
      DSINB=SIN(DBLE(DTOR*REAL(I-1)))
      IF (.NOT.(DR.LT.1.D0)) GO TO 10037
      DCOSA=(DR*DR*DSALT+DSSMO*SQRT(1.D0-DR*DR))/
     +                             (DR*DR+DSSMO)
      GO TO 10038
10037 CONTINUE
      DS=2.D0-DR
      DCOSA=(DS*DS*DSALT-DSSMO*SQRT(1.D0-DS*DS))/
     +                             (DS*DS+DSSMO)
10038 CONTINUE
      DSINA=SQRT(1.D0-DCOSA*DCOSA)
      DSINPH=DSINA*DSINB
      DCOSPH=DCOSA*DCSO-DSINA*DSNO*DCOSB
      DCOSLA=SQRT(DSINPH*DSINPH+DCOSPH*DCOSPH)
      IF (.NOT.(DCOSLA.NE.0.D0)) GO TO 10039
      DSINPH=DSINPH/DCOSLA
      DCOSPH=DCOSPH/DCOSLA
10039 CONTINUE
      IF (.NOT.(ABS(DSNO).GT.1.D-4)) GO TO 10040
      DSINLA=(DCOSA-DCOSLA*DCOSPH*DCSO)/DSNO
      GO TO 10041
10040 CONTINUE
      DSINLA=DSINA*DCOSB
10041 CONTINUE
      RLAT=RTOD*REAL(ATAN2(DSINLA,DCOSLA))
      RLON=PHOC+RTOD*REAL(ATAN2(DSINA*DSINB,
     +                    DCOSA*DCSO-DSINA*DSNO*DCOSB))
      IF (ABS(RLON).GT.180.) RLON=RLON-SIGN(360.,RLON)
      CALL MAPITM (RLAT,RLON,IPEN,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',6).NE.0) RETURN
      IPEN=1
      IF (I.EQ.360) IPEN=2
10036 CONTINUE
      L10019=    4
      GO TO 10019
10042 CONTINUE
      IF (DR.EQ.1.0002D0) GO TO 10043
      DR=1.0002D0
      GO TO 10035
10043 CONTINUE
10034 CONTINUE
      SALT=SSLT
      GO TO 107
C
C Note:  The constant "1.999999500000" is the real effective radius of
C the limb of the Lambert equal area projection, as determined by the
C test at statement number 106 in the routine MAPTRN.
C
  103 URAD=1.999999500000
      RVTU=1.
      GO TO 106
C
C Note:  The constant "3.140178439909" is the real effective radius of
C the limb of the azimuthal equidistant projection, as determined by the
C test at statement number 108 in the routine MAPTRN.
C
  104 URAD=3.140178439909
      RVTU=1.
      GO TO 106
C
  105 URAD=2.
      RVTU=0.5
C
  106 IF (ELPF.AND.ABS(UCEN).LT.1.E-4.AND.
     +             ABS(VCEN).LT.1.E-4.AND.
     +             ABS(URNG-URAD).LT.1.E-4.AND.
     +             ABS(VRNG/URNG-RVTU).LT.1.E-4) GO TO 107
C
      TEMP=.9998
C
10044 CONTINUE
      IDLT=0
      IDRT=-1
      IVIS=-1
      I = 1
      GO TO 10047
10045 CONTINUE
      I =I +1
10047 CONTINUE
      IF (I .GT.(361)) GO TO 10046
      UCIR=TEMP*URAD*COS(DTOR*REAL(I-1))
      VCIR=TEMP*URAD*SIN(DTOR*REAL(I-1))
      U=UCIR
      V=RVTU*VCIR
      IF (.NOT.(.NOT.ELPF.AND.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.
     +GT.VMAX))) GO TO 10048
      IF (.NOT.(IVIS.EQ.1)) GO TO 10049
      CALL MAPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    6
      GO TO 10010
10050 CONTINUE
10049 CONTINUE
      IVIS=0
      GO TO 10051
10048 CONTINUE
      IF (.NOT.(ELPF.AND.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.)))
     +GO TO 10052
      IF (.NOT.(IVIS.EQ.1)) GO TO 10053
      CALL MAPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
      XCRD=UEDG
      YCRD=VEDG
      L10010=    7
      GO TO 10010
10054 CONTINUE
10053 CONTINUE
      IVIS=0
      GO TO 10051
10052 CONTINUE
      IF (.NOT.(IVIS.LT.0)) GO TO 10055
      XCRD=U
      YCRD=V
      L10005=    3
      GO TO 10005
10056 CONTINUE
      IVIS=1
      GO TO 10057
10055 CONTINUE
      IF (.NOT.(IVIS.EQ.0)) GO TO 10058
      IF (.NOT.ELPF) CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
      IF (     ELPF) CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
      XCRD=UOLD
      YCRD=VOLD
      L10005=    4
      GO TO 10005
10059 CONTINUE
      IVIS=1
10058 CONTINUE
      XCRD=U
      YCRD=V
      L10010=    8
      GO TO 10010
10060 CONTINUE
10057 CONTINUE
10051 CONTINUE
      UOLD=U
      VOLD=V
      GO TO 10045
10046 CONTINUE
      L10019=    5
      GO TO 10019
10061 CONTINUE
      IF (TEMP.EQ.1.0002) GO TO 10062
      TEMP=1.0002
      GO TO 10044
10062 CONTINUE
C
  107 CONTINUE
      IF (IGRP.EQ.IGI2.OR.NOVS.LE.0) GO TO 10063
C
      IGRP=IGI2
C
      GO TO 10001
10063 CONTINUE
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
      IF (ICFELL('MAPBLM',8).NE.0) RETURN
      IDLT=IDOS(NOUT)+IDLS
      IDRT=IDOS(NOUT)+IDRS
      NSEG=0
C
C Read the next record (group of points).
C
  301 CALL MAPIO (2)
      IF (ICFELL('MAPBLM',9).NE.0) RETURN
      IDLT=IDOS(NOUT)+IDLS
      IDRT=IDOS(NOUT)+IDRS
      NSEG=NSEG+1
C
C Check for the end of the desired data.
C
      IF (NPTS.EQ.0) RETURN
C
C If less than the whole globe is shown by the projection, do a quick
C check for intersection of the box surrounding the point group with
C the area shown.
C
      IF (.NOT.(IWGF.EQ.0)) GO TO 10064
      IF (SLAG.GT.BLAM.OR.BLAG.LT.SLAM) GO TO 301
      IF ((SLOG     .GT.BLOM.OR.BLOG     .LT.SLOM).AND.
     +    (SLOG-360..GT.BLOM.OR.BLOG-360..LT.SLOM).AND.
     +    (SLOG+360..GT.BLOM.OR.BLOG+360..LT.SLOM)) GO TO 301
10064 CONTINUE
C
C See if the user wants to omit this point group.
C
      CALL MAPEOD (NOUT,NSEG,IDLT,IDRT,NPTS,PNTS)
      IF (.NOT.(ICFELL('MAPBLM',10).NE.0)) GO TO 10065
      IIER=-1
      RETURN
10065 CONTINUE
      IF (NPTS.LE.1) GO TO 301
C
C Plot the group.
C
      CALL MAPITM (PNTS(1),PNTS(2),0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',11).NE.0) RETURN
C
      DO 10066 K=2,NPTS-1
      CALL MAPITM (PNTS(2*K-1),PNTS(2*K),1,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',12).NE.0) RETURN
10066 CONTINUE
C
      CALL MAPITM (PNTS(2*NPTS-1),PNTS(2*NPTS),2,
     +             IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',13).NE.0) RETURN
C
C Force a buffer dump.
C
      L10019=    6
      GO TO 10019
10067 CONTINUE
C
C Go get another group.
C
      GO TO 301
C
C The following internal procedure is invoked to start a line.
C
10005 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10068
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (.NOT.(ICFELL('MAPBLM',14).NE.0)) GO TO 10069
      IIER=-1
      RETURN
10069 CONTINUE
10068 CONTINUE
      XCRA(1)=XCRD
      YCRA(1)=YCRD
      NCRA=1
      GO TO (10004,10013,10056,10059) , L10005
C
C The following internal procedure is invoked to continue a line.
C
10010 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10070
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (.NOT.(ICFELL('MAPBLM',15).NE.0)) GO TO 10071
      IIER=-1
      RETURN
10071 CONTINUE
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10070 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=XCRD
      YCRA(NCRA)=YCRD
      GO TO (10009,10014,10015,10016,10017,10050,10054,10060) , L10010
C
C The following internal procedure is invoked to terminate a line.
C
10019 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10072
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,
     +             XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (.NOT.(ICFELL('MAPBLM',16).NE.0)) GO TO 10073
      IIER=-1
      RETURN
10073 CONTINUE
      NCRA=0
10072 CONTINUE
      GO TO (10018,10022,10032,10042,10061,10067) , L10019
C
      END
