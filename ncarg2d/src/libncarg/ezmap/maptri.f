C
C $Id: maptri.f,v 1.10 1999-06-04 21:44:56 kennison Exp $
C
      SUBROUTINE MAPTRI (UVAL,VVAL,RLAT,RLON)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE   /MAPCM1/
C
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE   /MAPDP1/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPCM6/ ELPM,UMNM,UMXM,VMNM,VMXM,UCNM,VCNM,URNM,VRNM
      LOGICAL ELPM
      SAVE   /MAPCM6/
C
      COMMON /MAPCM8/ P,Q,R
      SAVE   /MAPCM8/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
C
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE   /MAPDPS/
C
C Declare a couple of double-precision temporary variables.
C
      DOUBLE PRECISION DLAT,DLON
C
C Define various required constants.  DTOR is pi over 180, DTRH is half
C of DTOR, PIOT is pi over 2, RTDD is RTOD doubled, RTOD is 180 over pi,
C and TOPI is 2 over pi.
C
      DATA DTOR / .017453292519943 /
      DATA DTRH / .008726646259971 /
      DATA PI   / 3.14159265358979 /
      DATA PIOT / 1.57079632679489 /
      DATA RTDD / 114.591559026165 /
      DATA RTOD / 57.2957795130823 /
      DATA TOPI / .636619772367581 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPTRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPTRI',2).NE.0) RETURN
      END IF
C
C Check for a point outside the perimeter.  Return 1.E12's for such
C points.
C
      IF (ELPM) THEN
        IF (((UVAL-UCNM)/URNM)**2+
     +      ((VVAL-VCNM)/VRNM)**2.GT.1.000002) GO TO 301
      ELSE
        IF (UVAL.LT.UMNM.OR.UVAL.GT.UMXM.OR.
     +      VVAL.LT.VMNM.OR.VVAL.GT.VMXM) GO TO 301
      END IF
C
C The point is inside the perimeter.  Jump to the proper piece of code,
C depending on the projection type.
C
C Projection: US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
      GO TO (100,101,102,103,104,105,106,107,108,109,110,
     +                                   111,112,113,114) , IPRJ+1
C
C USGS transformations.
C
  100 IF (IROD.EQ.0) THEN
        CALL MPUTIS (UVAL,VVAL,RLAT,RLON)
      ELSE
        CALL MPUTID (DBLE(UVAL),DBLE(VVAL),DLAT,DLON)
        IF (DLAT.NE.1.D12) THEN
          RLAT=REAL(DLAT)
          RLON=REAL(DLON)
        ELSE
          RLAT=1.E12
          RLON=1.E12
        END IF
      END IF
      IF (RLAT.NE.1.E12) GO TO 201
      RETURN
C
C Lambert conformal conic.
C
  101 R=SQRT(UVAL*UVAL+VVAL*VVAL)
      IF (R.LT.1.E-6) THEN
        RLAT=RSNO*90.
        TMP1=0.
        TMP2=1.
      ELSE
        RLAT=RSNO*(90.-RTDD*ATAN(R**(1./RCSO)))
        TMP1=UVAL/R
        TMP2=-RSNO*VVAL/R
      END IF
      RLON=PHOC+RTOD*ATAN2(TMP1,TMP2)/RCSO
      IF (ABS(RLON-PHOC).GT.180.) GO TO 301
      GO TO 201
C
C Stereographic.
C
  102 R=SQRT(UVAL*UVAL+VVAL*VVAL)
      IF (R.LT.1.E-6) GO TO 198
      RSINB=(UVAL*RCSR-VVAL*RSNR)/R
      RCOSB=(UVAL*RSNR+VVAL*RCSR)/R
      RSINA=2.*R/(1.+R*R)
      RCOSA=(1.-R*R)/(1.+R*R)
      GO TO 199
C
C Orthographic or satellite-view (depending on the value of SALT).
C
  103 IF (ABS(SALT).LE.1.) THEN
        R=SQRT(UVAL*UVAL+VVAL*VVAL)
        IF (R.LT.1.E-6) GO TO 198
        RSINB=(UVAL*RCSR-VVAL*RSNR)/R
        RCOSB=(UVAL*RSNR+VVAL*RCSR)/R
        IF (R.LE.1.) THEN
          RSINA=R
          RCOSA=SQRT(1.-RSINA*RSINA)
        ELSE
          IF (SALT.GE.0..OR.R.GT.2.) GO TO 301
          RSINA=2.-R
          RCOSA=-SQRT(1.-RSINA*RSINA)
        END IF
        GO TO 199
      ELSE
        IF (ALFA.EQ.0.) THEN
          UTM1=UVAL
          VTM1=VVAL
        ELSE
          UTM3=UVAL*RCSB+VVAL*RSNB
          VTM3=VVAL*RCSB-UVAL*RSNB
          UTM2=SRSS*(SRSS*RSNA+UTM3*RCSA)/(SRSS*RCSA-UTM3*RSNA)
          IF ((SRSS*RCSA+UTM2*RSNA)/SRSS.LT.1.E-4) GO TO 301
          VTM2=SRSS*VTM3/(SRSS*RCSA-UTM3*RSNA)
          UTM1=UTM2*RCSB-VTM2*RSNB
          VTM1=UTM2*RSNB+VTM2*RCSB
        END IF
        R=SQRT(UTM1*UTM1+VTM1*VTM1)
        IF (R.LT.1.E-6) GO TO 198
        RSINB=(UTM1*RCSR-VTM1*RSNR)/R
        RCOSB=(UTM1*RSNR+VTM1*RCSR)/R
        IF (R.LE.1.) THEN
          RCOSA=(R*R*ABS(SALT)+SSMO*SQRT(1.-R*R))/(R*R+SSMO)
        ELSE
          IF (SALT.GE.0..OR.R.GT.2.) GO TO 301
          R=2.-R
          RCOSA=(R*R*ABS(SALT)-SSMO*SQRT(1.-R*R))/(R*R+SSMO)
        END IF
        RCOSA=MAX(-1.,MIN(+1.,RCOSA))
        RSINA=SQRT(1.-RCOSA*RCOSA)
        GO TO 199
      END IF
C
C Lambert equal-area.
C
  104 R=SQRT(UVAL*UVAL+VVAL*VVAL)
      IF (R.LT.1.E-6) GO TO 198
      IF (R.GT.2.) GO TO 301
      RSINB=(UVAL*RCSR-VVAL*RSNR)/R
      RCOSB=(UVAL*RSNR+VVAL*RCSR)/R
      RCOSA=MAX(-1.,MIN(+1.,1.-R*R/2.))
      RSINA=SQRT(1.-RCOSA*RCOSA)
      GO TO 199
C
C Gnomonic.
C
  105 R=SQRT(UVAL*UVAL+VVAL*VVAL)
      IF (R.LT.1.E-6) GO TO 198
      RSINB=(UVAL*RCSR-VVAL*RSNR)/R
      RCOSB=(UVAL*RSNR+VVAL*RCSR)/R
      RCOSA=MAX(-1.,MIN(+1.,1./SQRT(1.+R*R)))
      RSINA=SQRT(1.-RCOSA*RCOSA)
      GO TO 199
C
C Azimuthal equidistant.
C
  106 R=SQRT(UVAL*UVAL+VVAL*VVAL)
      IF (R.LT.1.E-6) GO TO 198
      IF (R.GT.PI) GO TO 301
      RSINB=(UVAL*RCSR-VVAL*RSNR)/R
      RCOSB=(UVAL*RSNR+VVAL*RCSR)/R
      RCOSA=COS(R)
      RSINA=SQRT(1.-RCOSA*RCOSA)
      GO TO 199
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  107 IF (ABS(UVAL).GT.180..OR.ABS(VVAL).GT.90.) GO TO 301
      ANGA=DTOR*(90.-VVAL)
      RSINA=SIN(ANGA)
      RCOSA=COS(ANGA)
      ANGU=DTOR*UVAL
      RSINU=SIN(ANGU)
      RCOSU=COS(ANGU)
      RSINB=RSINU*RCSR+RCOSU*RSNR
      RCOSB=RSINU*RSNR-RCOSU*RCSR
      GO TO 199
C
C Mercator, arbitrary pole and orientation.
C
  108 IF (ABS(UVAL).GT.PI) GO TO 301
      RSINA=SIN(PI-2.*ATAN(EXP(VVAL)))
      RCOSA=COS(PI-2.*ATAN(EXP(VVAL)))
      RSINU=SIN(UVAL)
      RCOSU=COS(UVAL)
      RSINB=RSINU*RCSR+RCOSU*RSNR
      RCOSB=RSINU*RSNR-RCOSU*RCSR
      GO TO 199
C
C Mollweide, arbitrary pole and orientation.
C
  109 IF (ABS(VVAL).GT.1.) GO TO 301
      RCOSA=VVAL
      RSINA=SQRT(1.-RCOSA*RCOSA)
      IF (RSINA.NE.0.) THEN
        IF (ABS(UVAL/RSINA).GT.2.) GO TO 301
        ANGU=PIOT*UVAL/RSINA
        RSINU=SIN(ANGU)
        RCOSU=COS(ANGU)
      ELSE
        IF (UVAL.NE.0.) GO TO 301
        RSINU=0.
        RCOSU=1.
      END IF
      RSINB=RSINU*RCSR+RCOSU*RSNR
      RCOSB=RSINU*RSNR-RCOSU*RCSR
      GO TO 199
C
C Robinson, arbitrary pole and orientation.
C
  110 IF (ABS(VVAL).GT..5072) GO TO 301
      VVTM=RBIDFE(VVAL)
      IF (ABS(UVAL).GT.RBGLEN(VVTM)) GO TO 301
      ANGA=PIOT-DTOR*VVTM
      RSINA=SIN(ANGA)
      RCOSA=COS(ANGA)
      ANGU=PI*UVAL/RBGLEN(VVTM)
      RSINU=SIN(ANGU)
      RCOSU=COS(ANGU)
      RSINB=RSINU*RCSR+RCOSU*RSNR
      RCOSB=RSINU*RSNR-RCOSU*RCSR
      GO TO 199
C
C Cylindrical equidistant, fast path.
C
  111 IF (ABS(UVAL).GT.180..OR.ABS(VVAL).GT.90.) GO TO 301
      RLAT=VVAL
      RLON=PHOC+UVAL
      GO TO 200
C
C Mercator, fast path.
C
  112 IF (ABS(UVAL).GT.PI) GO TO 301
      RLAT=RTDD*ATAN(EXP(VVAL))-90.
      RLON=PHOC+RTOD*UVAL
      GO TO 200
C
C Mollweide, fast path.
C
  113 IF (ABS(VVAL).GT.1.) GO TO 301
      RLAT=ASIN(VVAL)*RTOD
      IF (1.-VVAL*VVAL.NE.0.) THEN
        RLON=PHOC+90.*UVAL/SQRT(1.-VVAL*VVAL)
      ELSE
        RLON=PHOC
      END IF
      IF (ABS(RLON-PHOC).GT.180.) GO TO 301
      GO TO 200
C
C Robinson, fast path.
C
  114 IF (ABS(VVAL).GT..5072) GO TO 301
      VVTM=RBIDFE(VVAL)
      IF (ABS(UVAL).GT.RBGLEN(VVTM)) GO TO 301
      RLAT=VVTM
      RLON=PHOC+180.*UVAL/RBGLEN(VVTM)
      GO TO 200
C
C The following code is common to all of the azimuthal projections when
C the "radius" R is within epsilon of zero.
C
  198 RSINB=0.
      RCOSB=1.
      RSINA=0.
      RCOSA=1.
C
C The following code is common to all of the azimuthal projections.
C
  199 RSINPH=RSINA*RSINB
      RCOSPH=RCOSA*RCSO-RSINA*RSNO*RCOSB
      RCOSLA=SQRT(RSINPH*RSINPH+RCOSPH*RCOSPH)
C
      IF (RCOSLA.NE.0.) THEN
        RSINPH=RSINPH/RCOSLA
        RCOSPH=RCOSPH/RCOSLA
      END IF
      IF (ABS(RSNO).GT.1.E-4) THEN
        RSINLA=(RCOSA-RCOSLA*RCOSPH*RCSO)/RSNO
      ELSE
        RSINLA=RSINA*RCOSB
      END IF
C
      IF (RSINLA.NE.0..OR.RCOSLA.NE.0.) THEN
        RLAT=RTOD*ATAN2(RSINLA,RCOSLA)
      ELSE
        RLAT=0.
      END IF
      IF (RSINA*RSINB.NE.0..OR.RCOSA*RCSO-RSINA*RSNO*RCOSB.NE.0.) THEN
        RLON=PHOC+RTOD*ATAN2(RSINA*RSINB,RCOSA*RCSO-RSINA*RSNO*RCOSB)
      ELSE
        RLON=0.
      END IF
C
      GO TO 201
C
C The following code is common to all the fast-path projections.  If the
C rotation angle is 180, negate the output values of RLAT and RLON.
C
  200 IF (ABS(ROTA).GT.179.9999) THEN
        RLAT=-RLAT
        RLON=-RLON
      END IF
C
      GO TO 201
C
C Done.
C
  201 IF (ABS(RLON).GT.180.) RLON=RLON-SIGN(360.,RLON)
C
      RETURN
C
C Inverse is not defined; return the values that signal that.
C
  301 RLAT=1.E12
      RLON=1.E12
C
      RETURN
C
      END
