C
C $Id: mdplmb.f,v 1.12 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPLMB
C
C The routine MDPLMB is called by MDPGRD and/or MDPLOT to draw the limb
C lines.
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
        DOUBLE PRECISION CSLS,CSLT,SLTD
        INTEGER ISLT
        SAVE   /MAPCMW/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        COMMON /MAPWNC/  WXMN,WXMX,WYMN,WYMX
        DOUBLE PRECISION WXMN,WXMX,WYMN,WYMX
        SAVE   /MAPWNC/
C
        COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER          IPRF
        SAVE   /USGSC1/
C
C Declare local variables.
C
        INTEGER          I,IPEN,IVIS,J,K
C
        DOUBLE PRECISION ALPH,CLAT,CLON,CRAD,DEPS,DIST,DLAT,DLON,COSA,
     +                   COSB,COSL,COSP,RLAT,RLON,SINA,SINB,SINL,SINP,
     +                   RVTU,SSLT,TLAT(361),TLON(361),U,UCIR,UEDG,UNS1,
     +                   UOLD,URAD,UTMP,V,VCIR,VEDG,VNS1,VOLD,VTMP,X,
     +                   XANP,XAS1,XAS2,YANP,YAS1,YAS2
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
        IF (ICFELL('MDPLMB - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPLMB',2).NE.0) RETURN
        END IF
C
C Reset the color index, dotting, and dash pattern for limb lines.
C
        CALL MDPCHI (4,0,IOR(ISHIFT(32767,1),1))
        IF (ICFELL('MDPLMB',3).NE.0) RETURN
C
C Draw limb lines, the nature of which depends on the projection.
C
C Projection:   US  LC  ST  OR  LE  GN  AE
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (arbitrary)
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (fast-path)
C                       RM
C
        GO TO (100,101,110,104,105,110,106,
     +             112,112,107,112,112,114,115,115,112,
     +             112,112,107,112,112,114,115,115,112,
     +                 112                            ) , IPRJ+1
C
C USGS transformations.
C
  100   IF (IPRF.EQ. 3.OR.IPRF.EQ. 4.OR.IPRF.EQ. 5.OR.IPRF.EQ. 7.OR.
     +      IPRF.EQ. 8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +      IPRF.EQ.19.OR.IPRF.EQ.21) THEN
          IF (IPRF.EQ.3.OR.IPRF.EQ.8.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +        IPRF.EQ.21) THEN
            DLON=GRDR
            RLAT=-89.998D0
            K=CEIL(360.D0/DLON)
            DO 202 I=1,2
              RLON=UTPA(5)-180.D0
              CALL MDPIT (RLAT,RLON,0)
              IF (ICFELL('MDPLMB',4).NE.0) RETURN
              DO 201 J=1,K-1
                RLON=RLON+DLON
                CALL MDPIT (RLAT,RLON,1)
                IF (ICFELL('MDPLMB',5).NE.0) RETURN
  201         CONTINUE
              RLON=UTPA(5)+180.D0
              CALL MDPIT (RLAT,RLON,2)
              IF (ICFELL('MDPLMB',6).NE.0) RETURN
              CALL MDPIQ
              IF (ICFELL('MDPLMB',7).NE.0) RETURN
              RLAT=89.998D0
  202       CONTINUE
          END IF
          IF (IPRF.EQ.7) THEN
            DLON= 89.999999D0
          ELSE
            DLON=179.999999D0
          END IF
          DLAT=GRDR
          RLON=UTPA(5)+DLON
          K=CEIL(180.D0/DLAT)
          DO 204 I=1,2
            RLAT=-90.D0
            CALL MDPIT (RLAT,RLON,0)
            IF (ICFELL('MDPLMB',8).NE.0) RETURN
            DO 203 J=1,K-1
              RLAT=RLAT+DLAT
              CALL MDPIT (RLAT,RLON,1)
              IF (ICFELL('MDPLMB',9).NE.0) RETURN
  203       CONTINUE
            RLAT=90.D0
            CALL MDPIT (RLAT,RLON,2)
            IF (ICFELL('MDPLMB',10).NE.0) RETURN
            CALL MDPIQ
            IF (ICFELL('MDPLMB',11).NE.0) RETURN
            RLON=UTPA(5)-DLON
  204     CONTINUE
          GO TO 110
        ELSE IF (IPRF.EQ.9) THEN
          DLON=GRDR
          RLAT=-.001D0
          K=CEIL(180.D0/DLON)
          DO 206 I=1,2
            RLON=UTPA(5)+90.D0
            CALL MDPIT (RLAT,RLON,0)
            IF (ICFELL('MDPLMB',12).NE.0) RETURN
            DO 205 J=1,K-1
              RLON=RLON+DLON
              CALL MDPIT (RLAT,RLON,1)
              IF (ICFELL('MDPLMB',13).NE.0) RETURN
  205       CONTINUE
            RLON=UTPA(5)+270.D0
            CALL MDPIT (RLAT,RLON,2)
            IF (ICFELL('MDPLMB',14).NE.0) RETURN
            CALL MDPIQ
            IF (ICFELL('MDPLMB',15).NE.0) RETURN
            RLAT=.001D0
  206     CONTINUE
          GO TO 110
        ELSE IF (IPRF.EQ.11.OR.IPRF.EQ.12.OR.IPRF.EQ.14.OR.
     +                         IPRF.EQ.15.OR.IPRF.EQ.23) THEN
          IF (IPRF.EQ.11.OR.IPRF.EQ.12) THEN
            CLAT=UTPA(6)
            CLON=UTPA(5)
            CRAD=179.95D0
          ELSE IF (IPRF.EQ.14) THEN
            CLAT=UTPA(6)
            CLON=UTPA(5)
            CRAD=89.999D0
          ELSE IF (IPRF.EQ.15) THEN
            CLAT=UTPA(6)
            CLON=UTPA(5)
            CRAD=RTOD*ACOS(UTPA(1)/(UTPA(1)+UTPA(3)))-.001D0
          ELSE IF (IPRF.EQ.23) THEN
            CLAT=  64.D0
            CLON=-152.D0
            CRAD=  29.999D0
          END IF
          CALL MDGCOG (CLAT,CLON,CRAD,TLAT,TLON,361)
          CALL MDPIT (TLAT(1),TLON(1),0)
          IF (ICFELL('MDPLMB',16).NE.0) RETURN
          DO 207 I=2,360
            CALL MDPIT (TLAT(I),TLON(I),1)
            IF (ICFELL('MDPLMB',17).NE.0) RETURN
  207     CONTINUE
          CALL MDPIT (TLAT(361),TLON(361),2)
          IF (ICFELL('MDPLMB',18).NE.0) RETURN
          CALL MDPIQ
          IF (ICFELL('MDPLMB',19).NE.0) RETURN
          GO TO 110
        ELSE IF (IPRF.EQ.20) THEN
          ALPH=DTOR*(180.D0-UTPA(4))
          CALL MDPTRN (+90.D0,0.D0,XANP,YANP)
          IF (ICFELL('MDPLMB',20).NE.0) RETURN
          CALL MDPTRN (-90.D0,0.D0,XAS1,YAS1)
          IF (ICFELL('MDPLMB',21).NE.0) RETURN
          UNS1=(XAS1-XANP)*COS(ALPH)+(YAS1-YANP)*SIN(ALPH)
          VNS1=(YAS1-YANP)*COS(ALPH)-(XAS1-XANP)*SIN(ALPH)
          XAS2=XANP+VNS1*SIN(ALPH)+UNS1*COS(ALPH)
          YAS2=YANP+UNS1*SIN(ALPH)-VNS1*COS(ALPH)
          DIST=SQRT((XAS2-XAS1)*(XAS2-XAS1)+(YAS2-YAS1)*(YAS2-YAS1))
          IF (VNS1.LT.0.D0) THEN
            DEPS=-.001D0*DIST
          ELSE
            DEPS=+.001D0*DIST
          END IF
          DIST=2.D0*DIST
          WXMN=UMIN
          WXMX=UMAX
          WYMN=VMIN
          WYMX=VMAX
          CALL MDPWND (XAS1-DIST*COS(ALPH)+DEPS*SIN(ALPH),
     +                 YAS1-DIST*SIN(ALPH)-DEPS*COS(ALPH),0)
          IF (ICFELL('MDPLMB',22).NE.0) RETURN
          CALL MDPWND (XAS1+DIST*COS(ALPH)+DEPS*SIN(ALPH),
     +                 YAS1+DIST*SIN(ALPH)-DEPS*COS(ALPH),1)
          IF (ICFELL('MDPLMB',23).NE.0) RETURN
          CALL MDPWND (XAS2-DIST*COS(ALPH)-DEPS*SIN(ALPH),
     +                 YAS2-DIST*SIN(ALPH)+DEPS*COS(ALPH),0)
          IF (ICFELL('MDPLMB',24).NE.0) RETURN
          CALL MDPWND (XAS2+DIST*COS(ALPH)-DEPS*SIN(ALPH),
     +                 YAS2+DIST*SIN(ALPH)+DEPS*COS(ALPH),1)
          IF (ICFELL('MDPLMB',25).NE.0) RETURN
          GO TO 110
        ELSE
          GO TO 110
        END IF
C
C Lambert conformal conic with two standard parallels.
C
  101   DLAT=GRDR
        RLON=PLNO+179.999999D0
        K=CEIL(180.D0/DLAT)
        DO 103 I=1,2
          RLAT=-90.D0
          CALL MDPIT (RLAT,RLON,0)
          IF (ICFELL('MDPLMB',26).NE.0) RETURN
          DO 102 J=1,K-1
            RLAT=RLAT+DLAT
            CALL MDPIT (RLAT,RLON,1)
            IF (ICFELL('MDPLMB',27).NE.0) RETURN
  102     CONTINUE
          RLAT=90.D0
          CALL MDPIT (RLAT,RLON,2)
          IF (ICFELL('MDPLMB',28).NE.0) RETURN
          CALL MDPIQ
          IF (ICFELL('MDPLMB',29).NE.0) RETURN
          RLON=PLNO-179.999999D0
  103   CONTINUE
        GO TO 110
C
C Orthographic (or satellite-view).
C
  104   IF (ABS(SALT).LE.1.D0.OR.ALFA.EQ.0.D0) THEN
          URAD=1.D0
          RVTU=1.D0
          GO TO 108
        ELSE
          SSLT=SALT
          SALT=-ABS(SALT)
          IPEN=0
          COSA=1.D0/ABS(SALT)
          SINA=SQRT(1.D0-COSA*COSA)
          DO 111 I=1,361
            COSB=COS(DTOR*DBLE(I-1))
            SINB=SIN(DTOR*DBLE(I-1))
            SINL=SINA*SINB
            COSL=COSA*COSO-SINA*SINO*COSB
            COSP=SQRT(SINL*SINL+COSL*COSL)
            IF (COSP.NE.0.D0) THEN
              SINL=SINL/COSP
              COSL=COSL/COSP
            END IF
            IF (ABS(SINO).GT..000001D0) THEN
              SINP=(COSA-COSP*COSL*COSO)/SINO
            ELSE
              SINP=SINA*COSB
            END IF
            RLAT=RTOD*ATAN2(SINP,COSP)
            RLON=PLNO+RTOD*ATAN2(SINA*SINB,
     +                           COSA*COSO-SINA*SINO*COSB)
            IF (ABS(RLON).GT.180.D0) RLON=RLON-SIGN(360.D0,RLON)
            CALL MDPIT (RLAT,RLON,IPEN)
            IF (ICFELL('MDPLMB',30).NE.0) RETURN
            IPEN=1
  111     CONTINUE
          CALL MDPIQ
          IF (ICFELL('MDPLMB',31).NE.0) RETURN
          SALT=SSLT
          GO TO 110
        END IF
C
C Lambert equal-area.
C
  105   URAD=2.D0
        RVTU=1.D0
        GO TO 108
C
C Azimuthal equidistant.
C
  106   URAD=PI
        RVTU=1.D0
        GO TO 108
C
C Mollweide-type.
C
  107   URAD=2.D0
        RVTU=0.5D0
        GO TO 108
C
C Aitoff.
C
  114   URAD=PI
        RVTU=.5D0
        GO TO 108
C
C Hammer and true Mollweide.
C
  115   URAD=TSRT
        RVTU=.5D0
        GO TO 108
C
C Limb is a ellipse.  URAD is half the length of the horizontal axis
C and RVTU is the ratio of V to U.
C
  108   UCIR=URAD
        VCIR=0.D0
        IVIS=-1
        DO 109 I=1,361
          U=UCIR-UOFF
          V=RVTU*VCIR-VOFF
          IF (.NOT.ELPF.AND.
     +        (U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.GT.VMAX)) THEN
            IF (IVIS.EQ.1) THEN
              CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
              CALL MDPVP  (UOLD,VOLD,UEDG,VEDG)
              IF (ICFELL('MDPLMB',32).NE.0) RETURN
            END IF
            IVIS=0
          ELSE IF (ELPF.AND.
     +             (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)) THEN
            IF (IVIS.EQ.1) THEN
              CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
              CALL MDPVP  (UOLD,VOLD,UEDG,VEDG)
              IF (ICFELL('MDPLMB',33).NE.0) RETURN
            END IF
            IVIS=0
          ELSE
            IF (IVIS.LT.0) THEN
              IF (IDTL.EQ.0) THEN
                CALL FRSTD (REAL(U),REAL(V))
                IF (ICFELL('MDPLMB',34).NE.0) RETURN
              ELSE
                DATL=0.D0
              END IF
              IVIS=1
            ELSE
              IF (IVIS.EQ.0) THEN
                IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
                IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
                IF (IDTL.EQ.0) THEN
                  CALL FRSTD (REAL(UOLD),REAL(VOLD))
                  IF (ICFELL('MDPLMB',35).NE.0) RETURN
                ELSE
                  DATL=0.D0
                END IF
                IVIS=1
              END IF
              CALL MDPVP (UOLD,VOLD,U,V)
              IF (ICFELL('MDPLMB',36).NE.0) RETURN
            END IF
          END IF
          UOLD=U
          VOLD=V
          UTMP=UCIR
          VTMP=VCIR
          UCIR=UTMP*COS1-VTMP*SIN1
          VCIR=UTMP*SIN1+VTMP*COS1
  109   CONTINUE
C
        GO TO 110
C
C Cylindrical equidistant, Mercator, Robinson, cylindrical equal-area,
C Winkel tripel.
C
  112   RLAT=-90.D0
        RLON=-180.D0
        IVIS=-1
C
        DO 113 I=1,361
          IF (IPRJ.EQ.7.OR.IPRJ.EQ.16) THEN
            U=RLON-UOFF
            V=RLAT/CSLT-VOFF
          ELSE IF (IPRJ.EQ.8.OR.IPRJ.EQ.17.OR.IPRJ.EQ.25) THEN
            U=DTOR*RLON-UOFF
            V=LOG(TAN((MAX(-89.999999D0,
     +                 MIN(+89.999999D0,RLAT))+90.D0)*DTRH))-VOFF
            IF (IPRJ.EQ.25) THEN
              UTMP=U*COSR+V*SINR
              VTMP=V*COSR-U*SINR
              U=UTMP
              V=VTMP
            END IF
          ELSE IF (IPRJ.EQ.11.OR.IPRJ.EQ.20) THEN
            U=DTOR*RLON-UOFF
            V=SIN(DTOR*RLAT)/CSLS-VOFF
          ELSE IF (IPRJ.EQ.15.OR.IPRJ.EQ.24) THEN
            CALL WTPROJ (DTOR*RLAT,DTOR*RLON,U,V,CSLT)
            U=U-UOFF
            V=V-VOFF
          ELSE
            U=(RLON/180.D0)*RBGLEN(RLAT)-UOFF
            V=RBGDFE(RLAT)-VOFF
          END IF
          IF (I.LE.90) THEN
            RLON=RLON+4.D0
          ELSE IF (I.LE.180) THEN
            RLAT=RLAT+2.D0
          ELSE IF (I.LE.270) THEN
            RLON=RLON-4.D0
          ELSE IF (I.LE.360) THEN
            RLAT=RLAT-2.D0
          END IF
          IF (.NOT.ELPF.AND.
     +        (U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.GT.VMAX)) THEN
            IF (IVIS.EQ.1) THEN
              CALL MDPTRP (UOLD,VOLD,U,V,UEDG,VEDG)
              CALL MDPVP  (UOLD,VOLD,UEDG,VEDG)
              IF (ICFELL('MDPLMB',37).NE.0) RETURN
            END IF
            IVIS=0
          ELSE IF (ELPF.AND.
     +             (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.D0)) THEN
            IF (IVIS.EQ.1) THEN
              CALL MDPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
              CALL MDPVP  (UOLD,VOLD,UEDG,VEDG)
              IF (ICFELL('MDPLMB',38).NE.0) RETURN
            END IF
            IVIS=0
          ELSE
            IF (IVIS.LT.0) THEN
              IF (IDTL.EQ.0) THEN
                CALL FRSTD (REAL(U),REAL(V))
                IF (ICFELL('MDPLMB',39).NE.0) RETURN
              ELSE
                DATL=0.D0
              END IF
              IVIS=1
            ELSE
              IF (IVIS.EQ.0) THEN
                IF (.NOT.ELPF) CALL MDPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
                IF (     ELPF) CALL MDPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
                IF (IDTL.EQ.0) THEN
                  CALL FRSTD (REAL(UOLD),REAL(VOLD))
                  IF (ICFELL('MDPLMB',40).NE.0) RETURN
                ELSE
                  DATL=0.D0
                END IF
                IVIS=1
              END IF
              CALL MDPVP (UOLD,VOLD,U,V)
              IF (ICFELL('MDPLMB',41).NE.0) RETURN
            END IF
          END IF
          UOLD=U
          VOLD=V
  113   CONTINUE
C
C Restore the color index, dotting, and dash pattern.
C
  110   CALL MDPCHI (-4,0,0)
        IF (ICFELL('MDPLMB',42).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
