C
C $Id: mdptri.f,v 1.3 2002-08-19 21:52:45 kennison Exp $
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
      SUBROUTINE MDPTRI (UVAL,VVAL,RLAT,RLON)
C
        DOUBLE PRECISION UVAL,VVAL,RLAT,RLON
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
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
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
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
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
C Declare local variables.
C
        DOUBLE PRECISION ANGA,ANGU,RCOSA,RCOSB,RCOSLA,RCOSPH,RCOSU,
     +                   RSINA,RSINB,RSINLA,RSINPH,RSINU,TMP1,TMP2,
     +                   UTMP,UTM1,UTM2,UTM3,VTMP,VTM1,VTM2,VTM3,VVTM
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
C Projection: US  LC  ST  OR  LE  GN  AE  CE  ME  MO  RO
C
        GO TO (100,101,102,103,104,105,106,107,108,109,110,
     +                                     111,112,113,114) , IPRJ+1
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
        IF (RLAT.NE.1.D12) GO TO 201
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
        RLON=PHOC+RTOD*ATAN2(TMP1,TMP2)/COSO
        IF (ABS(RLON-PHOC).GT.180.D0) GO TO 301
        GO TO 201
C
C Stereographic.
C
  102   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RSINA=2.D0*R/(1.D0+R*R)
        RCOSA=(1.D0-R*R)/(1.D0+R*R)
        GO TO 199
C
C Orthographic or satellite-view (depending on the value of SALT).
C
  103   IF (ABS(SALT).LE.1.D0) THEN
          R=SQRT(UTMP*UTMP+VTMP*VTMP)
          IF (R.LT..000001D0) GO TO 198
          RSINB=(UTMP*COSR-VTMP*SINR)/R
          RCOSB=(UTMP*SINR+VTMP*COSR)/R
          IF (R.LE.1.D0) THEN
            RSINA=R
            RCOSA=SQRT(1.D0-RSINA*RSINA)
          ELSE
            IF (SALT.GE.0.D0.OR.R.GT.2.D0) GO TO 301
            RSINA=2.D0-R
            RCOSA=-SQRT(1.D0-RSINA*RSINA)
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
          RSINB=(UTM1*COSR-VTM1*SINR)/R
          RCOSB=(UTM1*SINR+VTM1*COSR)/R
          IF (R.LE.1.D0) THEN
            RCOSA=(R*R*ABS(SALT)+SSMO*SQRT(1.D0-R*R))/(R*R+SSMO)
          ELSE
            IF (SALT.GE.0.D0.OR.R.GT.2.D0) GO TO 301
            R=2.D0-R
            RCOSA=(R*R*ABS(SALT)-SSMO*SQRT(1.D0-R*R))/(R*R+SSMO)
          END IF
          RCOSA=MAX(-1.D0,MIN(+1.D0,RCOSA))
          RSINA=SQRT(1.D0-RCOSA*RCOSA)
          GO TO 199
        END IF
C
C Lambert equal-area.
C
  104   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        IF (R.GT.2.D0) GO TO 301
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RCOSA=MAX(-1.D0,MIN(+1.D0,1.D0-R*R/2.D0))
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        GO TO 199
C
C Gnomonic.
C
  105   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RCOSA=MAX(-1.D0,MIN(+1.D0,1.D0/SQRT(1.D0+R*R)))
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        GO TO 199
C
C Azimuthal equidistant.
C
  106   R=SQRT(UTMP*UTMP+VTMP*VTMP)
        IF (R.LT..000001D0) GO TO 198
        IF (R.GT.PI) GO TO 301
        RSINB=(UTMP*COSR-VTMP*SINR)/R
        RCOSB=(UTMP*SINR+VTMP*COSR)/R
        RCOSA=COS(R)
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        GO TO 199
C
C Cylindrical equidistant, arbitrary pole and orientation.
C
  107   IF (ABS(UTMP).GT.180.D0.OR.ABS(VTMP).GT.90.D0) GO TO 301
        ANGA=DTOR*(90.D0-VTMP)
        RSINA=SIN(ANGA)
        RCOSA=COS(ANGA)
        ANGU=DTOR*UTMP
        RSINU=SIN(ANGU)
        RCOSU=COS(ANGU)
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Mercator, arbitrary pole and orientation.
C
  108   IF (ABS(UTMP).GT.PI) GO TO 301
        RSINA=SIN(PI-2.D0*ATAN(EXP(VTMP)))
        RCOSA=COS(PI-2.D0*ATAN(EXP(VTMP)))
        RSINU=SIN(UTMP)
        RCOSU=COS(UTMP)
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Mollweide, arbitrary pole and orientation.
C
  109   IF (ABS(VTMP).GT.1.D0) GO TO 301
        RCOSA=VTMP
        RSINA=SQRT(1.D0-RCOSA*RCOSA)
        IF (RSINA.NE.0.D0) THEN
          IF (ABS(UTMP/RSINA).GT.2.D0) GO TO 301
          ANGU=PIOT*UTMP/RSINA
          RSINU=SIN(ANGU)
          RCOSU=COS(ANGU)
        ELSE
          IF (UTMP.NE.0.D0) GO TO 301
          RSINU=0.D0
          RCOSU=1.D0
        END IF
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Robinson, arbitrary pole and orientation.
C
  110   IF (ABS(VTMP).GT..5072D0) GO TO 301
        VVTM=RBIDFE(VTMP)
        IF (ABS(UTMP).GT.RBGLEN(VVTM)) GO TO 301
        ANGA=PIOT-DTOR*VVTM
        RSINA=SIN(ANGA)
        RCOSA=COS(ANGA)
        ANGU=PI*UTMP/RBGLEN(VVTM)
        RSINU=SIN(ANGU)
        RCOSU=COS(ANGU)
        RSINB=RSINU*COSR+RCOSU*SINR
        RCOSB=RSINU*SINR-RCOSU*COSR
        GO TO 199
C
C Cylindrical equidistant, fast path.
C
  111   IF (ABS(UTMP).GT.180.D0.OR.ABS(VTMP).GT.90.D0) GO TO 301
        RLAT=VTMP
        RLON=PHOC+UTMP
        GO TO 200
C
C Mercator, fast path.
C
  112   IF (ABS(UTMP).GT.PI) GO TO 301
        RLAT=RTDD*ATAN(EXP(VTMP))-90.D0
        RLON=PHOC+RTOD*UTMP
        GO TO 200
C
C Mollweide, fast path.
C
  113   IF (ABS(VTMP).GT.1.D0) GO TO 301
        RLAT=ASIN(VTMP)*RTOD
        IF (1.D0-VTMP*VTMP.NE.0.D0) THEN
          RLON=PHOC+90.D0*UTMP/SQRT(1.D0-VTMP*VTMP)
          IF (ABS(RLON-PHOC).GT.180.D0) GO TO 301
        ELSE
          IF (UTMP.NE.0.D0) GO TO 301
          RLON=PHOC
        END IF
        GO TO 200
C
C Robinson, fast path.
C
  114   IF (ABS(VTMP).GT..5072D0) GO TO 301
        VVTM=RBIDFE(VTMP)
        IF (ABS(UTMP).GT.RBGLEN(VVTM)) GO TO 301
        RLAT=VVTM
        RLON=PHOC+180.D0*UTMP/RBGLEN(VVTM)
        GO TO 200
C
C The following code is common to all of the azimuthal projections when
C the "radius" R is within epsilon of zero.
C
  198   RSINB=0.D0
        RCOSB=1.D0
        RSINA=0.D0
        RCOSA=1.D0
C
C The following code is common to all of the azimuthal projections.
C
  199   RSINPH=RSINA*RSINB
        RCOSPH=RCOSA*COSO-RSINA*SINO*RCOSB
        RCOSLA=SQRT(RSINPH*RSINPH+RCOSPH*RCOSPH)
C
        IF (RCOSLA.NE.0.D0) THEN
          RSINPH=RSINPH/RCOSLA
          RCOSPH=RCOSPH/RCOSLA
        END IF
        IF (ABS(SINO).GT..000001D0) THEN
          RSINLA=(RCOSA-RCOSLA*RCOSPH*COSO)/SINO
        ELSE
          RSINLA=RSINA*RCOSB
        END IF
C
        IF (RSINLA.NE.0.D0.OR.RCOSLA.NE.0.D0) THEN
          RLAT=RTOD*ATAN2(RSINLA,RCOSLA)
        ELSE
          RLAT=0.D0
        END IF
        IF (RSINA*RSINB.NE.0.D0.OR.
     +      RCOSA*COSO-RSINA*SINO*RCOSB.NE.0.D0) THEN
          RLON=PHOC+RTOD*ATAN2(RSINA*RSINB,RCOSA*COSO-RSINA*SINO*RCOSB)
        ELSE
          RLON=0.D0
        END IF
C
        GO TO 201
C
C The following code is common to all the fast-path projections.  If the
C rotation angle is 180, negate the output values of RLAT and RLON.
C
  200   IF (ABS(ROTA).GT.179.999999D0) THEN
          RLAT=-RLAT
          RLON=-RLON
        END IF
C
        GO TO 201
C
C Done.
C
  201   IF (ABS(RLON).GT.180.D0) RLON=RLON-SIGN(360.D0,RLON)
C
        RETURN
C
C Inverse is not defined; return the values that signal that.
C
  301   RLAT=1.D12
        RLON=1.D12
C
        RETURN
C
      END
