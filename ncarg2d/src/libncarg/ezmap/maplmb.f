C
C $Id: maplmb.f,v 1.9 1999-04-19 21:29:50 kennison Exp $
C
      SUBROUTINE MAPLMB
C
C The routine MAPLMB is called by MAPGRD and/or MAPLOT to draw the limb
C lines.
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
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UCEN,VCEN,URNG,VRNG,BLAM,SLAM,
     +                BLOM,SLOM,ISSL,PEPS
      SAVE   /MAPCM2/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE   /MAPCMA/
C
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE   /MAPSAT/
C
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE   /MAPDPS/
C
      COMMON /MAPWNC/ WXMN,WXMX,WYMN,WYMX
      SAVE   /MAPWNC/
C
      COMMON /USGSC1/ UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER IPRF
      SAVE   /USGSC1/
C
C Declare a couple of temporary arrays to hold coordinates of a circle.
C
      DIMENSION TLAT(361),TLON(361)
C
C Define required constants.  SIN1 and COS1 are respectively the sine
C and cosine of one degree.
C
      DATA DTOR / .017453292519943 /
      DATA DTRH / .008726646259971 /
      DATA RTOD / 57.2957795130823 /
      DATA SIN1 / .017452406437283 /
      DATA COS1 / .999847695156390 /
      DATA PI   / 3.14159265358979 /
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
      IF (ICFELL('MAPLMB - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPLMB',2).NE.0) RETURN
      END IF
C
C Reset the color index, dotting, and dash pattern for limb lines.
C
      CALL MAPCHI (4,0,IOR(ISHIFT(32767,1),1))
      IF (ICFELL('MAPLMB',3).NE.0) RETURN
C
C Draw limb lines, the nature of which depends on the projection.
C
C Projection: US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
      GO TO (100,101,110,104,105,110,106,112,112,107,112,
     +                                   112,112,107,112) , IPRJ+1
C
C USGS transformations.
C
  100 IF (IPRF.EQ. 3.OR.IPRF.EQ. 4.OR.IPRF.EQ. 5.OR.IPRF.EQ. 7.OR.
     +    IPRF.EQ. 8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +    IPRF.EQ.19.OR.IPRF.EQ.21) THEN
        IF (IPRF.EQ.3.OR.IPRF.EQ.8.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +      IPRF.EQ.21) THEN
          DLON=GRDR
          RLAT=-89.998
          K=CLING(360./DLON)
          DO 202 I=1,2
            RLON=REAL(UTPA(5))-180.
            CALL MAPIT (RLAT,RLON,0)
            IF (ICFELL('MAPLMB',4).NE.0) RETURN
            DO 201 J=1,K-1
              RLON=RLON+DLON
              CALL MAPIT (RLAT,RLON,1)
              IF (ICFELL('MAPLMB',5).NE.0) RETURN
  201       CONTINUE
            RLON=REAL(UTPA(5))+180.
            CALL MAPIT (RLAT,RLON,2)
            IF (ICFELL('MAPLMB',6).NE.0) RETURN
            CALL MAPIQ
            IF (ICFELL('MAPLMB',7).NE.0) RETURN
            RLAT=89.998
  202     CONTINUE
        END IF
        IF (IPRF.EQ.7) THEN
          DLON= 89.9999
        ELSE
          DLON=179.9999
        END IF
        DLAT=GRDR
        RLON=REAL(UTPA(5))+DLON
        K=CLING(180./DLAT)
        DO 204 I=1,2
          RLAT=-90.
          CALL MAPIT (RLAT,RLON,0)
          IF (ICFELL('MAPLMB',8).NE.0) RETURN
          DO 203 J=1,K-1
            RLAT=RLAT+DLAT
            CALL MAPIT (RLAT,RLON,1)
            IF (ICFELL('MAPLMB',9).NE.0) RETURN
  203     CONTINUE
          RLAT=90.
          CALL MAPIT (RLAT,RLON,2)
          IF (ICFELL('MAPLMB',10).NE.0) RETURN
          CALL MAPIQ
          IF (ICFELL('MAPLMB',11).NE.0) RETURN
          RLON=REAL(UTPA(5))-DLON
  204   CONTINUE
        GO TO 110
      ELSE IF (IPRF.EQ.9) THEN
        DLON=GRDR
        RLAT=-.001
        K=CLING(180./DLON)
        DO 206 I=1,2
          RLON=REAL(UTPA(5))+90.
          CALL MAPIT (RLAT,RLON,0)
          IF (ICFELL('MAPLMB',12).NE.0) RETURN
          DO 205 J=1,K-1
            RLON=RLON+DLON
            CALL MAPIT (RLAT,RLON,1)
            IF (ICFELL('MAPLMB',13).NE.0) RETURN
  205     CONTINUE
          RLON=REAL(UTPA(5))+270.
          CALL MAPIT (RLAT,RLON,2)
          IF (ICFELL('MAPLMB',14).NE.0) RETURN
          CALL MAPIQ
          IF (ICFELL('MAPLMB',15).NE.0) RETURN
          RLAT=.001
  206   CONTINUE
        GO TO 110
      ELSE IF (IPRF.EQ.11.OR.IPRF.EQ.12.OR.IPRF.EQ.14.OR.
     +                       IPRF.EQ.15.OR.IPRF.EQ.23) THEN
        IF (IPRF.EQ.11.OR.IPRF.EQ.12) THEN
          CLAT=REAL(UTPA(6))
          CLON=REAL(UTPA(5))
          CRAD=179.95
        ELSE IF (IPRF.EQ.14) THEN
          CLAT=REAL(UTPA(6))
          CLON=REAL(UTPA(5))
          CRAD=89.999
        ELSE IF (IPRF.EQ.15) THEN
          CLAT=REAL(UTPA(6))
          CLON=REAL(UTPA(5))
          CRAD=RTOD*ACOS(REAL(UTPA(1)/(UTPA(1)+UTPA(3))))-.001
        ELSE IF (IPRF.EQ.23) THEN
          CLAT=  64.
          CLON=-152.
          CRAD=  29.999
        END IF
        CALL NGGCOG (CLAT,CLON,CRAD,TLAT,TLON,361)
        CALL MAPIT (TLAT(1),TLON(1),0)
        IF (ICFELL('MAPLMB',16).NE.0) RETURN
        DO 207 I=2,360
          CALL MAPIT (TLAT(I),TLON(I),1)
          IF (ICFELL('MAPLMB',17).NE.0) RETURN
  207   CONTINUE
        CALL MAPIT (TLAT(361),TLON(361),2)
        IF (ICFELL('MAPLMB',18).NE.0) RETURN
        CALL MAPIQ
        IF (ICFELL('MAPLMB',19).NE.0) RETURN
        GO TO 110
      ELSE IF (IPRF.EQ.20) THEN
        ALPH=.017453292519943*(180.-REAL(UTPA(4)))
        CALL MAPTRN ( 90.,0.,XANP,YANP)
        IF (ICFELL('MAPLMB',20).NE.0) RETURN
        CALL MAPTRN (-90.,0.,XAS1,YAS1)
        IF (ICFELL('MAPLMB',21).NE.0) RETURN
        UNS1=(XAS1-XANP)*COS(ALPH)+(YAS1-YANP)*SIN(ALPH)
        VNS1=(YAS1-YANP)*COS(ALPH)-(XAS1-XANP)*SIN(ALPH)
        XAS2=XANP+VNS1*SIN(ALPH)+UNS1*COS(ALPH)
        YAS2=YANP+UNS1*SIN(ALPH)-VNS1*COS(ALPH)
        DIST=SQRT((XAS2-XAS1)*(XAS2-XAS1)+(YAS2-YAS1)*(YAS2-YAS1))
        IF (VNS1.LT.0.) THEN
          DEPS=-.001*DIST
        ELSE
          DEPS=+.001*DIST
        END IF
        DIST=2.*DIST
        WXMN=UMIN
        WXMX=UMAX
        WYMN=VMIN
        WYMX=VMAX
        CALL MAPWND (XAS1-DIST*COS(ALPH)+DEPS*SIN(ALPH),
     +               YAS1-DIST*SIN(ALPH)-DEPS*COS(ALPH),0)
        IF (ICFELL('MAPLMB',22).NE.0) RETURN
        CALL MAPWND (XAS1+DIST*COS(ALPH)+DEPS*SIN(ALPH),
     +               YAS1+DIST*SIN(ALPH)-DEPS*COS(ALPH),1)
        IF (ICFELL('MAPLMB',23).NE.0) RETURN
        CALL MAPWND (XAS2-DIST*COS(ALPH)-DEPS*SIN(ALPH),
     +               YAS2-DIST*SIN(ALPH)+DEPS*COS(ALPH),0)
        IF (ICFELL('MAPLMB',24).NE.0) RETURN
        CALL MAPWND (XAS2+DIST*COS(ALPH)-DEPS*SIN(ALPH),
     +               YAS2+DIST*SIN(ALPH)+DEPS*COS(ALPH),1)
        IF (ICFELL('MAPLMB',25).NE.0) RETURN
        GO TO 110
      ELSE
        GO TO 110
      END IF
C
C Lambert conformal conic with two standard parallels.
C
  101 DLAT=GRDR
      RLON=PHOC+179.9999
      K=CLING(180./DLAT)
      DO 103 I=1,2
        RLAT=-90.
        CALL MAPIT (RLAT,RLON,0)
        IF (ICFELL('MAPLMB',26).NE.0) RETURN
        DO 102 J=1,K-1
          RLAT=RLAT+DLAT
          CALL MAPIT (RLAT,RLON,1)
          IF (ICFELL('MAPLMB',27).NE.0) RETURN
  102   CONTINUE
        RLAT=90.
        CALL MAPIT (RLAT,RLON,2)
        IF (ICFELL('MAPLMB',28).NE.0) RETURN
        CALL MAPIQ
        IF (ICFELL('MAPLMB',29).NE.0) RETURN
        RLON=PHOC-179.9999
  103 CONTINUE
      GO TO 110
C
C Orthographic (or satellite-view).
C
  104 IF (ABS(SALT).LE.1..OR.ALFA.EQ.0.) THEN
        URAD=1.
        RVTU=1.
        GO TO 108
      ELSE
        SSLT=SALT
        SALT=-ABS(SALT)
        IPEN=0
        RCOSA=1./ABS(SALT)
        RSINA=SQRT(1.-RCOSA*RCOSA)
        DO 111 I=1,361
          RCOSB=COS(DTOR*REAL(I-1))
          RSINB=SIN(DTOR*REAL(I-1))
          RSINPH=RSINA*RSINB
          RCOSPH=RCOSA*RCSO-RSINA*RSNO*RCOSB
          RCOSLA=SQRT(RSINPH*RSINPH+RCOSPH*RCOSPH)
          IF (RCOSLA.NE.0.) THEN
            RSINPH=RSINPH/RCOSLA
            RCOSPH=RCOSPH/RCOSLA
          END IF
          IF (ABS(RSNO).GT.1.E-4) THEN
            RSINLA=(RCOSA-RCOSLA*RCOSPH*RCSO)/RSNO
          ELSE
            RSINLA=RSINA*RCOSB
          END IF
          RLAT=RTOD*ATAN2(RSINLA,RCOSLA)
          RLON=PHOC+RTOD*ATAN2(RSINA*RSINB,RCOSA*RCSO-RSINA*RSNO*RCOSB)
          IF (ABS(RLON).GT.180.) RLON=RLON-SIGN(360.,RLON)
          CALL MAPIT (RLAT,RLON,IPEN)
          IF (ICFELL('MAPLMB',30).NE.0) RETURN
          IPEN=1
  111   CONTINUE
        CALL MAPIQ
        IF (ICFELL('MAPLMB',31).NE.0) RETURN
        SALT=SSLT
        GO TO 110
      END IF
C
C Lambert equal-area.
C
  105 URAD=2.
      RVTU=1.
      GO TO 108
C
C Azimuthal equidistant.
C
  106 URAD=PI
      RVTU=1.
      GO TO 108
C
C Mollweide.
C
  107 URAD=2.
      RVTU=0.5
C
  108 UCIR=URAD
      VCIR=0.
      IVIS=-1
      DO 109 I=1,361
        U=UCIR
        V=RVTU*VCIR
        IF (.NOT.ELPF.AND.
     +      (U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.GT.VMAX)) THEN
          IF (IVIS.EQ.1) THEN
            CALL MAPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
            CALL MAPVP  (UOLD,VOLD,UEDG,VEDG)
            IF (ICFELL('MAPLMB',32).NE.0) RETURN
          END IF
          IVIS=0
        ELSE IF (ELPF.AND.
     +           (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.)) THEN
          IF (IVIS.EQ.1) THEN
            CALL MAPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
            CALL MAPVP  (UOLD,VOLD,UEDG,VEDG)
            IF (ICFELL('MAPLMB',33).NE.0) RETURN
          END IF
          IVIS=0
        ELSE
          IF (IVIS.LT.0) THEN
            IF (IDTL.EQ.0) THEN
              CALL FRSTD (U,V)
              IF (ICFELL('MAPLMB',34).NE.0) RETURN
            ELSE
              DATL=0.
            END IF
            IVIS=1
          ELSE
            IF (IVIS.EQ.0) THEN
              IF (.NOT.ELPF) CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
              IF (     ELPF) CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
              IF (IDTL.EQ.0) THEN
                CALL FRSTD (UOLD,VOLD)
                IF (ICFELL('MAPLMB',35).NE.0) RETURN
              ELSE
                DATL=0.
              END IF
              IVIS=1
            END IF
            CALL MAPVP (UOLD,VOLD,U,V)
            IF (ICFELL('MAPLMB',36).NE.0) RETURN
          END IF
        END IF
        UOLD=U
        VOLD=V
        UTMP=UCIR
        VTMP=VCIR
        UCIR=UTMP*COS1-VTMP*SIN1
        VCIR=UTMP*SIN1+VTMP*COS1
  109 CONTINUE
C
      GO TO 110
C
C Cylindrical equidistant, Mercator, or Robinson.
C
  112 RLAT=-90.
      RLON=-180.
      IVIS=-1
C
      DO 113 I=1,361
        IF (IPRJ.EQ.7.OR.IPRJ.EQ.11) THEN
          U=RLON
          V=RLAT
        ELSE IF (IPRJ.EQ.8.OR.IPRJ.EQ.12) THEN
          U=DTOR*RLON
          V=LOG(TAN((MAX(-89.9999,MIN(+89.9999,RLAT))+90.)*DTRH))
        ELSE
          U=(RLON/180.)*RBGLEN(RLAT)
          V=RBGDFE(RLAT)
        END IF
        IF (I.LE.90) THEN
          RLON=RLON+4.
        ELSE IF (I.LE.180) THEN
          RLAT=RLAT+2.
        ELSE IF (I.LE.270) THEN
          RLON=RLON-4.
        ELSE IF (I.LE.360) THEN
          RLAT=RLAT-2.
        END IF
        IF (.NOT.ELPF.AND.
     +      (U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.GT.VMAX)) THEN
          IF (IVIS.EQ.1) THEN
            CALL MAPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
            CALL MAPVP  (UOLD,VOLD,UEDG,VEDG)
            IF (ICFELL('MAPLMB',37).NE.0) RETURN
          END IF
          IVIS=0
        ELSE IF (ELPF.AND.
     +           (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.)) THEN
          IF (IVIS.EQ.1) THEN
            CALL MAPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
            CALL MAPVP  (UOLD,VOLD,UEDG,VEDG)
            IF (ICFELL('MAPLMB',38).NE.0) RETURN
          END IF
          IVIS=0
        ELSE
          IF (IVIS.LT.0) THEN
            IF (IDTL.EQ.0) THEN
              CALL FRSTD (U,V)
              IF (ICFELL('MAPLMB',39).NE.0) RETURN
            ELSE
              DATL=0.
            END IF
            IVIS=1
          ELSE
            IF (IVIS.EQ.0) THEN
              IF (.NOT.ELPF) CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
              IF (     ELPF) CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
              IF (IDTL.EQ.0) THEN
                CALL FRSTD (UOLD,VOLD)
                IF (ICFELL('MAPLMB',40).NE.0) RETURN
              ELSE
                DATL=0.
              END IF
              IVIS=1
            END IF
            CALL MAPVP (UOLD,VOLD,U,V)
            IF (ICFELL('MAPLMB',41).NE.0) RETURN
          END IF
        END IF
        UOLD=U
        VOLD=V
  113 CONTINUE
C
C Restore the color index, dotting, and dash pattern.
C
  110 CALL MAPCHI (-4,0,0)
      IF (ICFELL('MAPLMB',42).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
