C
C $Id: mdptri.f,v 1.13 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPTRI (UVAL,VVAL,RLAT,RLON)
C
        DOUBLE PRECISION UVAL,VVAL,RLAT,RLON
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
        COMMON /MAPCM6/  UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM,ELPM
        DOUBLE PRECISION UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM
        LOGICAL ELPM
        SAVE   /MAPCM6/
C
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
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
C Declare local variables.
C
        DOUBLE PRECISION COSA,COSB,COSL,COSP,RLAP,RLOP,SINA,SINB,SINL,
     +                   SINP,TMP1,TMP2,UTMP,UTM1,UTM2,UTM3,VTMP,VTM1,
     +                   VTM2,VTM3,VVTM,XVAL,YVAL,ZVAL
C
        REAL             SLAT,SLON
C
C Declare function types.
C
        DOUBLE PRECISION RBGLEN,RBIDFE
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPTRI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPTRI',2).NE.0) RETURN
        END IF
C
C Check for a point outside the perimeter.  Return 1.D12's for such
C points.
C
        IF (ELPM) THEN
          IF (((UVAL-UCNM)/URNM)**2+
     +        ((VVAL-VCNM)/VRNM)**2.GT.1.000002D0) GO TO 301
        ELSE
          IF (UVAL.LT.UMNM.OR.UVAL.GT.UMXM.OR.
     +        VVAL.LT.VMNM.OR.VVAL.GT.VMXM) GO TO 301
        END IF
C
C The point is inside the perimeter.  Compute its actual coordinates.
C
        UTMP=UVAL+UOFF
        VTMP=VVAL+VOFF
C
C Jump to the proper piece of code, depending on the projection type.
C
C Projection:   US  LC  ST  OR  LE  GN  AE
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (arbitrary)
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (fast-path)
C                       RM
C
        GO TO (100,101,102,103,104,105,106,
     +             107,108,109,110,111,112,113,114,115,
     +             116,117,118,119,120,121,122,123,124,
     +                 125                            ) , IPRJ+1
C
C USGS transformations.
C
  100   IF (IROD.EQ.0) THEN
          CALL MDUTIS (REAL(UTMP),REAL(VTMP),SLAT,SLON)
          IF (SLAT.NE.1.E12) THEN
            RLAT=DBLE(SLAT)
            RLON=DBLE(SLON)
          ELSE
            RLAT=1.D12
            RLON=1.D12
          END IF
        ELSE
          CALL MDUTID (UTMP,VTMP,RLAT,RLON)
        END IF
        IF (RLAT.NE.1.D12) GO TO 202
        RETURN
C
C Lambert conformal conic.
C
  101   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) THEN
          RLAT=SINO*90.D0
          TMP1=0.D0
          TMP2=1.D0
        ELSE
          RLAT=SINO*(90.D0-RTDD*ATAN(R**(1.D0/COSO)))
          TMP1=UTMP/R
          TMP2=-SINO*VTMP/R
        END IF
        RLON=PLNO+RTOD*ATAN2(TMP1,TMP2)/COSO
        IF (ABS(RLON-PLNO).GT.180.D0) GO TO 301
        GO TO 202
C
C Stereographic.
C
  102   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        SINB=(UTMP*COSR-VTMP*SINR)/R
        COSB=(UTMP*SINR+VTMP*COSR)/R
        SINA=2.D0*R/(1.D0+R*R)
        COSA=(1.D0-R*R)/(1.D0+R*R)
        GO TO 199
C
C Orthographic or satellite-view (depending on the value of SALT).
C
  103   IF (ABS(SALT).LE.1.D0) THEN
          R=SQRT(UTMP*UTMP+VTMP*VTMP)
          IF (R.LT..000001D0) GO TO 198
          SINB=(UTMP*COSR-VTMP*SINR)/R
          COSB=(UTMP*SINR+VTMP*COSR)/R
          IF (R.LE.1.D0) THEN
            SINA=R
            COSA=SQRT(1.D0-SINA*SINA)
          ELSE
            IF (SALT.GE.0.D0.OR.R.GT.2.D0) GO TO 301
            SINA=2.D0-R
            COSA=-SQRT(1.D0-SINA*SINA)
          END IF
          GO TO 199
        ELSE
          IF (ALFA.EQ.0.D0) THEN
            UTM1=UTMP
            VTM1=VTMP
          ELSE
            UTM3=UTMP*DCSB+VTMP*DSNB
            VTM3=VTMP*DCSB-UTMP*DSNB
            UTM2=SRSS*(SRSS*DSNA+UTM3*DCSA)/(SRSS*DCSA-UTM3*DSNA)
            IF ((SRSS*DCSA+UTM2*DSNA)/SRSS.LT..0001D0) GO TO 301
            VTM2=SRSS*VTM3/(SRSS*DCSA-UTM3*DSNA)
            UTM1=UTM2*DCSB-VTM2*DSNB
            VTM1=UTM2*DSNB+VTM2*DCSB
          END IF
          R=SQRT(UTM1*UTM1+VTM1*VTM1)
          IF (R.LT..000001D0) GO TO 198
          SINB=(UTM1*COSR-VTM1*SINR)/R
          COSB=(UTM1*SINR+VTM1*COSR)/R
          IF (R.LE.1.D0) THEN
            COSA=(R*R*ABS(SALT)+SSMO*SQRT(1.D0-R*R))/(R*R+SSMO)
          ELSE
            IF (SALT.GE.0.D0.OR.R.GT.2.D0) GO TO 301
            R=2.D0-R
            COSA=(R*R*ABS(SALT)-SSMO*SQRT(1.D0-R*R))/(R*R+SSMO)
          END IF
          COSA=MAX(-1.D0,MIN(+1.D0,COSA))
          SINA=SQRT(1.D0-COSA*COSA)
          GO TO 199
        END IF
C
C Lambert equal-area.
C
  104   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        IF (R.GT.2.D0) GO TO 301
        SINB=(UTMP*COSR-VTMP*SINR)/R
        COSB=(UTMP*SINR+VTMP*COSR)/R
        COSA=MAX(-1.D0,MIN(+1.D0,1.D0-R*R/2.D0))
        SINA=SQRT(1.D0-COSA*COSA)
        GO TO 199
C
C Gnomonic.
C
  105   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        SINB=(UTMP*COSR-VTMP*SINR)/R
        COSB=(UTMP*SINR+VTMP*COSR)/R
        COSA=MAX(-1.D0,MIN(+1.D0,1.D0/SQRT(1.D0+R*R)))
        SINA=SQRT(1.D0-COSA*COSA)
        GO TO 199
C
C Azimuthal equidistant.
C
  106   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        IF (R.GT.PI) GO TO 301
        SINB=(UTMP*COSR-VTMP*SINR)/R
        COSB=(UTMP*SINR+VTMP*COSR)/R
        COSA=COS(R)
        SINA=SQRT(1.D0-COSA*COSA)
        GO TO 199
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  107   IF (ABS(UTMP).GT.180.D0.OR.ABS(VTMP).GT.90.D0/CSLT) GO TO 301
        RLAP=CSLT*VTMP*DTOR
        RLOP=UTMP*DTOR
        GO TO 200
C
C Mercator, arbitrary pole and orientation.
C
  108   IF (ABS(UTMP).GT.PI) GO TO 301
        RLAP=2.D0*ATAN(EXP(VTMP))-PIOT
        RLOP=UTMP
        GO TO 200
C
C Mollweide-type, arbitrary pole and orientation.
C
  109   IF (ABS(VTMP).GT.1.D0) GO TO 301
        RLAP=ASIN(VTMP)
        IF (1.D0-VTMP*VTMP.NE.0.D0) THEN
          RLOP=PIOT*UTMP/SQRT(1.D0-VTMP*VTMP)
          IF (RTOD*ABS(RLOP).GT.180.D0) GO TO 301
        ELSE
          IF (UTMP.NE.0.D0) GO TO 301
          RLOP=0.D0
        END IF
        GO TO 200
C
C Robinson, arbitrary pole and orientation.
C
  110   IF (ABS(VTMP).GT..5072D0) GO TO 301
        VVTM=RBIDFE(VTMP)
        IF (ABS(UTMP).GT.RBGLEN(VVTM)) GO TO 301
        RLAP=DTOR*VVTM
        RLOP=PI*UTMP/RBGLEN(VVTM)
        GO TO 200
C
C Cylindrical equal-area, arbitrary pole and orientation.
C
  111   IF (ABS(UTMP).GT.PI.OR.ABS(VTMP).GT.1.D0/CSLS) GO TO 301
        RLAP=ASIN(VTMP*CSLS)
        RLOP=UTMP
        GO TO 200
C
C Aitoff, arbitrary pole and orientation.
C
  112   CALL AIPRIN (UTMP,VTMP,RLAP,RLOP)
        IF (RLAP.EQ.1.D12) GO TO 301
        GO TO 200
C
C Hammer, arbitrary pole and orientation.
C
  113   CALL HAPRIN (UTMP,VTMP,RLAP,RLOP)
        IF (RLAP.EQ.1.D12) GO TO 301
        GO TO 200
C
C True Mollweide, arbitrary pole and orientation.
C
  114   CALL MOPRIN (UTMP,VTMP,RLAP,RLOP)
        IF (RLAP.EQ.1.D12) GO TO 301
        GO TO 200
C
C Winkel tripel, arbitrary pole and orientation.
C
  115   CALL WTPRIN (UTMP,VTMP,RLAP,RLOP,CSLT)
        IF (RLAP.EQ.1.D12) GO TO 301
        GO TO 200
C
C Cylindrical equidistant, fast-path.
C
  116   IF (ABS(UTMP).GT.180.D0.OR.ABS(VTMP).GT.90.D0/CSLT) GO TO 301
        RLAT=CSLT*VTMP
        RLON=PLNO+UTMP
        GO TO 201
C
C Mercator, fast-path.
C
  117   IF (ABS(UTMP).GT.PI) GO TO 301
        RLAT=RTDD*ATAN(EXP(VTMP))-90.D0
        RLON=PLNO+RTOD*UTMP
        GO TO 201
C
C Mollweide-type, fast-path.
C
  118   IF (ABS(VTMP).GT.1.D0) GO TO 301
        RLAT=ASIN(VTMP)*RTOD
        IF (1.D0-VTMP*VTMP.NE.0.D0) THEN
          RLON=PLNO+90.D0*UTMP/SQRT(1.D0-VTMP*VTMP)
          IF (ABS(RLON-PLNO).GT.180.D0) GO TO 301
        ELSE
          IF (UTMP.NE.0.D0) GO TO 301
          RLON=PLNO
        END IF
        GO TO 201
C
C Robinson, fast-path.
C
  119   IF (ABS(VTMP).GT..5072D0) GO TO 301
        VVTM=RBIDFE(VTMP)
        IF (ABS(UTMP).GT.RBGLEN(VVTM)) GO TO 301
        RLAT=VVTM
        RLON=PLNO+180.D0*UTMP/RBGLEN(VVTM)
        GO TO 201
C
C Cylindrical equal-area, fast-path.
C
  120   IF (ABS(UTMP).GT.PI.OR.ABS(VTMP).GT.1.D0/CSLS) GO TO 301
        RLAT=RTOD*ASIN(VTMP*CSLS)
        RLON=PLNO+RTOD*UTMP
        GO TO 201
C
C Aitoff, fast-path.
C
  121   CALL AIPRIN (UTMP,VTMP,RLAT,RLON)
        IF (RLAT.EQ.1.D12) GO TO 301
        RLAT=RTOD*RLAT
        RLON=PLNO+RTOD*RLON
        GO TO 201
C
C Hammer, fast-path.
C
  122   CALL HAPRIN (UTMP,VTMP,RLAT,RLON)
        IF (RLAT.EQ.1.D12) GO TO 301
        RLAT=RTOD*RLAT
        RLON=PLNO+RTOD*RLON
        GO TO 201
C
C True Mollweide, fast-path.
C
  123   CALL MOPRIN (UTMP,VTMP,RLAT,RLON)
        IF (RLAT.EQ.1.D12) GO TO 301
        RLAT=RTOD*RLAT
        RLON=PLNO+RTOD*RLON
        GO TO 201
C
C Winkel tripel, fast-path.
C
  124   CALL WTPRIN (UTMP,VTMP,RLAT,RLON,CSLT)
        IF (RLAT.EQ.1.D12) GO TO 301
        RLAT=RTOD*RLAT
        RLON=PLNO+RTOD*RLON
        GO TO 201
C
C Rotated Mercator.
C
  125   UTM1=UTMP*COSR-VTMP*SINR
        VTM1=VTMP*COSR+UTMP*SINR
        RLAT=RTDD*ATAN(EXP(VTM1))-90.D0
        RLON=PLNO+RTOD*UTM1
        GO TO 202
C
C The following code is common to all of the azimuthal projections when
C the "radius" R is within epsilon of zero.
C
  198   SINB=0.D0
        COSB=1.D0
        SINA=0.D0
        COSA=1.D0
C
C The following code is common to all of the azimuthal projections.
C
  199   SINL=SINA*SINB
        COSL=COSA*COSO-SINA*SINO*COSB
        COSP=SQRT(SINL*SINL+COSL*COSL)
C
        IF (COSP.NE.0.D0) THEN
          SINL=SINL/COSP
          COSL=COSL/COSP
        END IF
        IF (ABS(SINO).GT..000001D0) THEN
          SINP=(COSA-COSP*COSL*COSO)/SINO
        ELSE
          SINP=SINA*COSB
        END IF
C
        IF (SINP.NE.0.D0.OR.COSP.NE.0.D0) THEN
          RLAT=RTOD*ATAN2(SINP,COSP)
        ELSE
          RLAT=0.D0
        END IF
        IF (SINA*SINB.NE.0.D0.OR.COSA*COSO-SINA*SINO*COSB.NE.0.D0) THEN
          RLON=PLNO+RTOD*ATAN2(SINA*SINB,COSA*COSO-SINA*SINO*COSB)
        ELSE
          RLON=0.D0
        END IF
C
        GO TO 202
C
C The following code is common to all of the cylindrical and mixed
C projections.
C
  200   XVAL=COS(RLOP)*COS(RLAP)*COSO-
     +                    (SIN(RLAP)*COSR+SIN(RLOP)*COS(RLAP)*SINR)*SINO
        YVAL=SIN(RLOP)*COS(RLAP)*COSR-SIN(RLAP)*SINR
        ZVAL=(SIN(RLAP)*COSR+SIN(RLOP)*COS(RLAP)*SINR)*COSO+
     +                                          COS(RLOP)*COS(RLAP)*SINO
        RLAT=RTOD*ASIN(MAX(-1.D0,MIN(+1.D0,ZVAL)))
        RLON=RTOD*ATAN2(YVAL,XVAL)+PLNO
C
        GO TO 202
C
C The following code is common to all the fast-path projections.  If the
C rotation angle is 180, negate the output values of RLAT and RLON.
C
  201   IF (ABS(ROTA).GT.179.999999D0) THEN
          RLAT=-RLAT
          RLON=PLNO-(RLON-PLNO)
        END IF
C
        GO TO 202
C
C Done.
C
  202   IF (ABS(RLON).GT.180.D0) RLON=RLON-SIGN(360.D0,RLON)
C
        GO TO 302
C
C Inverse is not defined; return the values that signal that.
C
  301   RLAT=1.D12
        RLON=1.D12
C
  302   RETURN
C
      END
