C
C $Id: maptrn.f,v 1.10 1999-06-04 21:44:56 kennison Exp $
C
      SUBROUTINE MAPTRN (RLAT,RLON,U,V)
C
C Given the latitude, RLAT, and the longitude, RLON, of a point on the
C earth, this routine returns the U and V coordinates of the projection
C of that point on the map.
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
      COMMON /USGSC1/ UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER IPRF
      SAVE   /USGSC1/
C
C Declare some things double precision.
C
      DOUBLE PRECISION TMP1,TMP2,SINPH,SINLA,COSPH,COSLA,TCOS,COSA,SINA,
     +                 COSB,SINB,UTM1,VTM1,UTM2,VTM2,UTM3,VTM3,MPGDDP,
     +                 UDBL,VDBL
C
C Define required constants.  DTOR is pi over 180, DTRH is half of DTOR
C or pi over 360, TOPI is 2 over pi, and OOPI is 1 over pi.
C
      DATA DTOR / .017453292519943 /
      DATA DTRH / .008726646259971 /
      DATA RTOD / 57.2957795130823 /
      DATA TOPI / .636619772367581 /
      DATA OOPI / .318309886183790 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPTRN - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, there's a problem: MAPTRN can't call
C MAPINT, because MAPINT calls MAPTRN; instead, it calls the internal
C routine MAPIN1 to initialize just the constants that it needs.
C
      IF (INTF) CALL MAPIN1
C
C Set up U and V for the fast paths.  U is a longitude, in degrees,
C between -180. and +180., inclusive, and V is a latitude, in degrees.
C
      TEMP=RLON-PHOC
      U=TEMP-SIGN(180.,TEMP+180.)+SIGN(180.,180.-TEMP)
      V=MAX(-90.,MIN(90.,RLAT))
C
C If a fast-path projection is in use and the rotation angle is 180,
C adjust U and V.
C
      IF (IPRJ.GE.11.AND.ABS(ROTA).GT.179.9999) THEN
        U=-U
        V=-V
      END IF
C
C Take fast paths for simple cylindrical projections.
C
      IF (IPRJ-11) 100,197,113
C
C No fast path.  Sort out the USGS transformations and the Lambert
C conformal conic from the rest.
C
  100 IF (IPRJ-1) 101,102,103
C
C USGS projections.
C
  101 IF (IPRF.EQ.0) GO TO 901
C
      IF (IROD.EQ.0) THEN
        CALL MPUTFS (RLAT,RLON,U,V)
      ELSE
        CALL MPUTFD (DBLE(RLAT),DBLE(RLON),UDBL,VDBL)
        IF (UDBL.NE.1.D12) THEN
          U=REAL(UDBL)
          V=REAL(VDBL)
        ELSE
          U=1.E12
          V=1.E12
        END IF
      END IF
C
      IF (IPRF.EQ. 3.OR.IPRF.EQ. 4.OR.IPRF.EQ. 5.OR.IPRF.EQ. 7.OR.
     +    IPRF.EQ. 8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +    IPRF.EQ.19.OR.IPRF.EQ.21) THEN
        TMP1=DBLE(RLON)-UTPA(5)
        P=REAL(TMP1-SIGN(180.D0,TMP1+180.D0)+SIGN(180.D0,180.D0-TMP1))
        IF ((IPRF.EQ.3.OR.IPRF.EQ.8).AND.ABS(RLAT).GT.89.999) GO TO 200
      ELSE IF (IPRF.EQ.9) THEN
        P=V
      ELSE IF (IPRF.EQ.11.OR.IPRF.EQ.12) THEN
        IF (MPGDDP(DBLE(RLAT),DBLE(RLON),
     +             UTPA(   6),UTPA(   5)).GT.179.99D0) GO TO 200
      ELSE IF (IPRF.EQ.20) THEN
        P=U*SIN(DTOR*REAL(UTPA(4)))+V*COS(DTOR*REAL(UTPA(4)))
      ELSE IF (IPRF.EQ.22) THEN
        P=U*SIN(DTOR*REAL(UTPA(4)))+V*COS(DTOR*REAL(UTPA(4)))
      END IF
C
      GO TO 199
C
C Lambert conformal conic.
C
  102 P=U
      CHI=90.-RSNO*V
      IF (CHI.GE.179.9999) GO TO 200
      R=TAN(DTRH*CHI)**RCSO
      U=U*RCSO*DTOR
      V=-R*RSNO*COS(U)
      U=R*SIN(U)
      GO TO 198
C
C Not Lambert conformal conic.  Calculate constants common to most of
C the other projections.
C
  103 IF (IROD.EQ.0) THEN
        RTMP1=U*DTOR
        RTMP2=V*DTOR
        RSINPH=SIN(RTMP1)
        RSINLA=SIN(RTMP2)
        RCOSPH=COS(RTMP1)
        RCOSLA=COS(RTMP2)
        RTCOS=RCOSLA*RCOSPH
        RCOSA=MAX(-1.,MIN(+1.,RSINLA*RSNO+RTCOS*RCSO))
        RSINA=SQRT(1.-RCOSA*RCOSA)
        IF (RSINA.LT..0001) THEN
          RSINA=0.
          IF (IPRJ.EQ.3.AND.ABS(SALT).GT.1.) THEN
            RSINB=0.
            RCOSB=1.
            GO TO 105
          END IF
          IF (IPRJ.GE.7.OR.RCOSA.LT.0.) GO TO 200
          U=0.
          V=0.
          GO TO 197
        END IF
        RSINB=RCOSLA*RSINPH/RSINA
        RCOSB=(RSINLA*RCSO-RTCOS*RSNO)/RSINA
      ELSE
        TMP1=DBLE(U)*.017453292519943D0
        TMP2=DBLE(V)*.017453292519943D0
        SINPH=SIN(TMP1)
        SINLA=SIN(TMP2)
        COSPH=COS(TMP1)
        COSLA=COS(TMP2)
        TCOS=COSLA*COSPH
        COSA=MAX(-1.D0,MIN(+1.D0,SINLA*DSNO+TCOS*DCSO))
        SINA=SQRT(1.D0-COSA*COSA)
        IF (SINA.LT..0001D0) THEN
          SINA=0.D0
          IF (IPRJ.EQ.3.AND.ABS(SALT).GT.1.) THEN
            SINB=0.D0
            COSB=1.D0
            GO TO 105
          END IF
          IF (IPRJ.GE.7.OR.COSA.LT.0.D0) GO TO 200
          U=0.
          V=0.
          GO TO 197
        END IF
        SINB=COSLA*SINPH/SINA
        COSB=(SINLA*DCSO-TCOS*DSNO)/SINA
      END IF
C
C Jump to code appropriate for the chosen projection.
C
C Projection: ST  OR  LE  GN  AE  CE  ME  MO  RO
C
      GO TO (104,105,106,107,108,109,110,111,112) , IPRJ-1
C
C Stereographic.
C
  104 IF (IROD.EQ.0) THEN
        IF (ABS(RSINA).LT..0001) THEN
          R=RSINA/2.
        ELSE
          R=(1.-RCOSA)/RSINA
        END IF
      ELSE
        IF (ABS(SINA).LT..0001D0) THEN
          R=REAL(SINA/2.D0)
        ELSE
          R=REAL((1.D0-COSA)/SINA)
        END IF
      END IF
      GO TO 196
C
C Orthographic or satellite-view, depending on the value of SALT.
C
  105 IF (IROD.EQ.0) THEN
        IF (ABS(SALT).LE.1.) THEN
          IF (RCOSA.GT.0.) THEN
            R=RSINA
          ELSE
            IF (SALT.GE.0.) GO TO 200
            R=2.-RSINA
          END IF
          GO TO 196
        ELSE
          IF (RCOSA.GT.1./ABS(SALT)) THEN
            R=SRSS*RSINA/(ABS(SALT)-RCOSA)
          ELSE
            IF (SALT.GE.0.) GO TO 200
            R=2.-SRSS*RSINA/(ABS(SALT)-RCOSA)
          END IF
          IF (ALFA.EQ.0.) GO TO 196
          RUTM1=R*(RSINB*RCSR+RCOSB*RSNR)
          RVTM1=R*(RCOSB*RCSR-RSINB*RSNR)
          RUTM2=RUTM1*RCSB+RVTM1*RSNB
          RVTM2=RVTM1*RCSB-RUTM1*RSNB
          IF ((SRSS*RCSA+RUTM2*RSNA)/SRSS.LT.1.E-4) GO TO 200
          RUTM3=SRSS*(RUTM2*RCSA-SRSS*RSNA)/(RUTM2*RSNA+SRSS*RCSA)
          RVTM3=SRSS*RVTM2/(RUTM2*RSNA+SRSS*RCSA)
          U=RUTM3*RCSB-RVTM3*RSNB
          V=RVTM3*RCSB+RUTM3*RSNB
          GO TO 197
        END IF
      ELSE
        IF (ABS(SALT).LE.1.) THEN
          IF (COSA.GT.0.D0) THEN
            R=REAL(SINA)
          ELSE
            IF (SALT.GE.0.) GO TO 200
            R=REAL(2.D0-SINA)
          END IF
          GO TO 196
        ELSE
          IF (COSA.GT.1.D0/ABS(DBLE(SALT))) THEN
            R=REAL(DBLE(SRSS)*SINA/(ABS(DBLE(SALT))-COSA))
          ELSE
            IF (SALT.GE.0.) GO TO 200
            R=REAL(2.D0-DBLE(SRSS)*SINA/(ABS(DBLE(SALT))-COSA))
          END IF
          IF (ALFA.EQ.0.) GO TO 196
          UTM1=DBLE(R)*(SINB*DCSR+COSB*DSNR)
          VTM1=DBLE(R)*(COSB*DCSR-SINB*DSNR)
          UTM2=UTM1*DCSB+VTM1*DSNB
          VTM2=VTM1*DCSB-UTM1*DSNB
          IF ((DBLE(SRSS)*DCSA+UTM2*DSNA)/DBLE(SRSS).LT.1.D-4) GO TO 200
          UTM3=DBLE(SRSS)*(UTM2*DCSA-DBLE(SRSS)*DSNA)/
     +                    (UTM2*DSNA+DBLE(SRSS)*DCSA)
          VTM3=DBLE(SRSS)*VTM2/(UTM2*DSNA+DBLE(SRSS)*DCSA)
          U=REAL(UTM3*DCSB-VTM3*DSNB)
          V=REAL(VTM3*DCSB+UTM3*DSNB)
          GO TO 197
        END IF
      END IF
C
C Lambert equal area.
C
  106 IF (IROD.EQ.0) THEN
        IF (ABS(RCOSA+1.).LT.1.E-6) GO TO 200
        R=(1.+RCOSA)/RSINA
      ELSE
        IF (ABS(COSA+1.D0).LT.1.D-6) GO TO 200
        R=REAL((1.D0+COSA)/SINA)
      END IF
      R=2./SQRT(1.+R*R)
      GO TO 196
C
C Gnomonic.
C
  107 IF (IROD.EQ.0) THEN
        IF (RCOSA.LE..0001) GO TO 200
        R=RSINA/RCOSA
      ELSE
        IF (COSA.LE..0001D0) GO TO 200
        R=REAL(SINA/COSA)
      END IF
      GO TO 196
C
C Azimuthal equidistant.
C
  108 IF (IROD.EQ.0) THEN
        IF (ABS(RCOSA+1.).LT.1.E-6) GO TO 200
        R=ACOS(RCOSA)
      ELSE
        IF (ABS(COSA+1.D0).LT.1.D-6) GO TO 200
        R=REAL(ACOS(COSA))
      END IF
      GO TO 196
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  109 IF (IROD.EQ.0) THEN
        U=ATAN2(RSINB*RCSR+RCOSB*RSNR,RSINB*RSNR-RCOSB*RCSR)*RTOD
        V=90.-ACOS(RCOSA)*RTOD
      ELSE
        U=REAL(ATAN2(SINB*DCSR+COSB*DSNR,SINB*DSNR-COSB*DCSR))*RTOD
        V=90.-REAL(ACOS(COSA))*RTOD
      END IF
      GO TO 197
C
C Mercator, arbitrary pole and orientation.
C
  110 IF (IROD.EQ.0) THEN
        U=ATAN2(RSINB*RCSR+RCOSB*RSNR,RSINB*RSNR-RCOSB*RCSR)
        V=LOG((1.+RCOSA)/RSINA)
      ELSE
        U=REAL(ATAN2(SINB*DCSR+COSB*DSNR,SINB*DSNR-COSB*DCSR))
        V=REAL(LOG((1.D0+COSA)/SINA))
      END IF
      GO TO 197
C
C Mollweide, arbitrary pole and orientation.
C
  111 IF (IROD.EQ.0) THEN
        P=ATAN2(RSINB*RCSR+RCOSB*RSNR,RSINB*RSNR-RCOSB*RCSR)*TOPI
        U=P*RSINA
        V=RCOSA
      ELSE
        P=REAL(ATAN2(SINB*DCSR+COSB*DSNR,SINB*DSNR-COSB*DCSR))*TOPI
        U=P*REAL(SINA)
        V=REAL(COSA)
      END IF
      GO TO 198
C
C Robinson, arbitrary pole and orientation.
C
  112 IF (IROD.EQ.0) THEN
        P=ATAN2(RSINB*RCSR+RCOSB*RSNR,RSINB*RSNR-RCOSB*RCSR)*OOPI
        U=P*RBGLEN(90.-ACOS(RCOSA)*RTOD)
        V=RBGDFE(90.-ACOS(RCOSA)*RTOD)
      ELSE
        P=REAL(ATAN2(SINB*DCSR+COSB*DSNR,SINB*DSNR-COSB*DCSR))*OOPI
        U=P*RBGLEN(90.-REAL(ACOS(COSA))*RTOD)
        V=RBGDFE(90.-REAL(ACOS(COSA))*RTOD)
      END IF
      GO TO 198
C
C Fast-path cylindrical projections (with PLAT=0, ROTA=0 or 180).
C
  113 IF (IPRJ-13) 114,115,116
C
C Fast-path Mercator.
C
  114 IF (ABS(V).GT.89.9999) GO TO 200
      U=U*DTOR
      V=LOG(TAN((V+90.)*DTRH))
      GO TO 197
C
C Fast-path Mollweide.
C
  115 P=U/90.
      V=SIN(V*DTOR)
      U=P*SQRT(1.-V*V)
      GO TO 198
C
C Fast-path Robinson.
C
  116 P=U/180.
      U=P*RBGLEN(V)
      V=RBGDFE(V)
      GO TO 198
C
C Common terminal code for certain projections.
C
  196 IF (IROD.EQ.0) THEN
        U=R*(RSINB*RCSR+RCOSB*RSNR)
        V=R*(RCOSB*RCSR-RSINB*RSNR)
      ELSE
        U=R*REAL(SINB*DCSR+COSB*DSNR)
        V=R*REAL(COSB*DCSR-SINB*DSNR)
      END IF
C
  197 P=U
C
  198 Q=V
C
C Normal exit.
C
  199 RETURN
C
C Projection of point is invisible or undefined.
C
  200 U=1.E12
      P=U
      RETURN
C
C Error exit.
C
  901 CALL SETER ('MAPTRN - USGS PROJECTION WAS NOT INITIALIZED',2,1)
      GO TO 199
C
      END
