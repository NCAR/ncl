C
C $Id: maplmb.f,v 1.5 1994-05-03 21:17:30 kennison Exp $
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
      SAVE /MAPCM1/
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE /MAPDP1/
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE /MAPCM2/
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
C
C Define required constants.  SIN1 and COS1 are respectively the sine
C and cosine of one degree.
C
      DATA DTOR / .017453292519943 /
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
      IF (ICFELL('MAPLMB - UNCLEARED PRIOR ERROR',1).NE.0) THEN
        IIER=-1
        RETURN
      END IF
C
C Reset the color index, dotting, and dash pattern for limb lines.
C
      CALL MAPCHI (4,0,IOR(ISHIFT(32767,1),1))
      IF (ICFELL('MAPLMB',2).NE.0) RETURN
C
C Draw limb lines, the nature of which depends on the projection.
C
      GO TO (101,110,104,105,110,106,110,110,107,110,110,107) , IPRJ
C
C Lambert conformal conic with two standard parallels.
C
  101 DLAT=GRDR
      RLON=PHOC+179.9999
      K=CLING(180./DLAT)
      DO 103 I=1,2
        RLAT=-90.
        CALL MAPIT (RLAT,RLON,0)
        IF (ICFELL('MAPLMB',3).NE.0) RETURN
        DO 102 J=1,K-1
          RLAT=RLAT+DLAT
          CALL MAPIT (RLAT,RLON,1)
          IF (ICFELL('MAPLMB',4).NE.0) RETURN
  102   CONTINUE
        RLAT=RLAT+DLAT
        CALL MAPIT (RLAT,RLON,2)
        IF (ICFELL('MAPLMB',5).NE.0) RETURN
        CALL MAPIQ
        IF (ICFELL('MAPLMB',6).NE.0) RETURN
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
          IF (ICFELL('MAPLMB',7).NE.0) RETURN
          IPEN=1
  111   CONTINUE
        CALL MAPIQ
        IF (ICFELL('MAPLMB',8).NE.0) RETURN
        SALT=SSLT
        GO TO 110
      END IF
C
C Lambert equal area.
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
            IF (ICFELL('MAPLMB',9).NE.0) RETURN
          END IF
          IVIS=0
        ELSE IF (ELPF.AND.
     +           (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.)) THEN
          IF (IVIS.EQ.1) THEN
            CALL MAPTRE (UOLD,VOLD,U,V,UEDG,VEDG)
            CALL MAPVP  (UOLD,VOLD,UEDG,VEDG)
            IF (ICFELL('MAPLMB',10).NE.0) RETURN
          END IF
          IVIS=0
        ELSE
          IF (IVIS.LT.0) THEN
            IF (IDTL.EQ.0) THEN
              CALL FRSTD (U,V)
              IF (ICFELL('MAPLMB',11).NE.0) THEN
                IIER=-1
                RETURN
              END IF
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
                IF (ICFELL('MAPLMB',12).NE.0) THEN
                  IIER=-1
                  RETURN
                END IF
              ELSE
                DATL=0.
              END IF
              IVIS=1
            END IF
            CALL MAPVP (UOLD,VOLD,U,V)
            IF (ICFELL('MAPLMB',13).NE.0) RETURN
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
C Restore the color index, dotting, and dash pattern.
C
  110 CALL MAPCHI (-4,0,0)
      IF (ICFELL('MAPLMB',14).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
