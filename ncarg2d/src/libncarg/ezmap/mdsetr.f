C
C $Id: mdsetr.f,v 1.3 2001-11-02 22:37:18 kennison Exp $
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
      SUBROUTINE MDSETR (WHCH,RVAL)
C
        CHARACTER*(*) WHCH
        REAL          RVAL
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
        SAVE   /MAPCM0/
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
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        IF (ICFELL('MDSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
        IF      (WHCH(1:2).EQ.'DD'.OR.WHCH(1:2).EQ.'dd') THEN
          DDTS=DBLE(RVAL)
          DBTD=DDTS/DSCA
        ELSE IF (WHCH(1:2).EQ.'GD'.OR.WHCH(1:2).EQ.'gd') THEN
          GRDR=MAX(.001D0,MIN(10.D0,DBLE(RVAL)))
        ELSE IF (WHCH(1:2).EQ.'GN'.OR.WHCH(1:2).EQ.'gn') THEN
          GRLO=DBLE(RVAL)
        ELSE IF (WHCH(1:2).EQ.'GP'.OR.WHCH(1:2).EQ.'gp') THEN
          GRPO=MAX(0.D0,MIN(90360.D0,DBLE(RVAL)))
        ELSE IF (WHCH(1:2).EQ.'GR'.OR.WHCH(1:2).EQ.'gr') THEN
          GRID=DBLE(RVAL)
        ELSE IF (WHCH(1:2).EQ.'GT'.OR.WHCH(1:2).EQ.'gt') THEN
          GRLA=DBLE(RVAL)
        ELSE IF (WHCH(1:2).EQ.'MV'.OR.WHCH(1:2).EQ.'mv') THEN
          DPLT=DBLE(RVAL)
          DPSQ=DPLT*DPLT
        ELSE IF (WHCH(1:2).EQ.'OT'.OR.WHCH(1:2).EQ.'ot') THEN
          OTOL=MAX(0.D0,DBLE(RVAL))
        ELSE IF (WHCH(1:2).EQ.'RE'.OR.WHCH(1:2).EQ.'re') THEN
          PLTR=DBLE(RVAL)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
          SALT=DBLE(RVAL)
          IF (ABS(SALT).GT.1.D0) THEN
            SSMO=SALT*SALT-1.D0
            SRSS=SQRT(SSMO)
          END IF
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S1'.OR.WHCH(1:2).EQ.'s1') THEN
          ALFA=ABS(DBLE(RVAL))
          DSNA=SIN(DTOR*ALFA)
          DCSA=COS(DTOR*ALFA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'S2'.OR.WHCH(1:2).EQ.'s2') THEN
          BETA=DBLE(RVAL)
          DSNB=SIN(DTOR*BETA)
          DCSB=COS(DTOR*BETA)
          INTF=.TRUE.
        ELSE IF (WHCH(1:2).EQ.'SR'.OR.WHCH(1:2).EQ.'sr') THEN
          SRCH=MAX(.001D0,MIN(10.D0,DBLE(RVAL)))
          INTF=.TRUE.
        ELSE
          GO TO 901
        END IF
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   CALL MDPCEM ('MDSETR - UNKNOWN PARAMETER NAME ',WHCH,2,1)
        RETURN
C
      END
