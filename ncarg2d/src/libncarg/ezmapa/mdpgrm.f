C
C $Id: mdpgrm.f,v 1.2 2002-02-25 18:06:20 kennison Exp $
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
      SUBROUTINE MDPGRM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
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
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
      DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG
      INTEGER          ISSL
      SAVE   /MAPCM2/
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
C Declare local variables.
C
      DOUBLE PRECISION BLAT,BLON,DLAT,DLON,GLAT,GLON,OLAT,RLAT,RLON,
     +                 SLAT,SLON,U,V,X,XLAT,XLON
C
C Declare arithmetic statement functions.
C
      DOUBLE PRECISION CEIL,FLOR
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
      IF (ICFELL('MDPGRM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MDPINT
      IF (ICFELL('MDPGRM',2).NE.0) RETURN
10000 CONTINUE
C
C If the grid is suppressed, do nothing.
C
      IF (GRID.LE.0.D0) RETURN
C
C Otherwise, set the latitude and longitude grid spacings.
C
      GLAT=GRID
      GLON=GRID
      IF (GRLA.GT.0.D0) GLAT=GRLA
      IF (GRLO.GT.0.D0) GLON=GRLO
C
C Reset the color index and dash pattern for the grid.
C
      CALL MDPCHM (2,IDSH,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',3).NE.0) RETURN
C
C Transfer the latitude/longitude limits computed by MDPINT to local,
C modifiable variables.
C
      SLAT=SLAM
      BLAT=BLAM
      SLON=SLOM
      BLON=BLOM
C
C For certain azimuthal projections centered at a pole, the latitude
C limit furthest from the pole needs adjustment to make it projectable
C and visible.  Otherwise, we have trouble with portions of meridians
C disappearing.
C
      IF (.NOT.(IPRJ.EQ.3.OR.IPRJ.EQ.4.OR.IPRJ.EQ.6)) GO TO 10001
      IF (.NOT.(PHIA.GT.+89.999999D0)) GO TO 10002
      SLAT=SLAT+SRCH
      IF (IPRJ.EQ.3) SLAT=SLAT+SRCH
10002 CONTINUE
      IF (.NOT.(PHIA.LT.-89.999999D0)) GO TO 10003
      BLAT=BLAT-SRCH
      IF (IPRJ.EQ.3) BLAT=BLAT-SRCH
10003 CONTINUE
10001 CONTINUE
C
C RLON is the smallest longitude for which a meridian is to be drawn,
C XLON the biggest.  Avoid drawing a given meridian twice.
C
      RLON=GLON*FLOR(SLON/GLON)
      XLON=GLON*CEIL(BLON/GLON)
C
      IF (.NOT.(XLON-RLON.GT.359.999999D0)) GO TO 10004
      IF (.NOT.(IPRJ.EQ.1)) GO TO 10005
      RLON=GLON*CEIL((PHOC-179.999999D0)/GLON)
      XLON=GLON*FLOR((PHOC+179.999999D0)/GLON)
      GO TO 10006
10005 CONTINUE
      IF (.NOT.(IPRJ.GE.2.AND.IPRJ.LE.10)) GO TO 10007
      XLON=XLON-GLON
      IF (XLON-RLON.GT.359.999999D0) XLON=XLON-GLON
10006 CONTINUE
10007 CONTINUE
10004 CONTINUE
C
C OLAT is the latitude at which meridians that do not extend all the
C way to the poles are to stop.
C
      IF (.NOT.(IPRJ.EQ.11.OR.IPRJ.EQ.12.OR.IPRJ.EQ.14)) GO TO 10008
      OLAT=90.D0
      GO TO 10009
10008 CONTINUE
      IF (.NOT.(INT(GRPO/1000.D0).EQ.0)) GO TO 10010
      OLAT=GLAT*FLOR(89.999999D0/GLAT)
      GO TO 10011
10010 CONTINUE
      OLAT=GLAT*FLOR(MIN(89.999999D0,DINT(GRPO/1000.D0))/GLAT)
10011 CONTINUE
10009 CONTINUE
C
C Draw the meridians.
C
      RLON=RLON-GLON
  101 RLON=RLON+GLON
      XLAT=OLAT
      IF (.NOT.(MOD(GRPO,1000.D0).GT.0.D0)) GO TO 10012
      IF (MOD(RLON,MOD(GRPO,1000.D0)).EQ.0.D0) XLAT=90.D0
10012 CONTINUE
      RLAT=MAX(SLAT,-XLAT)
      XLAT=MIN(BLAT, XLAT)
      DLAT=(XLAT-RLAT)/CEIL((XLAT-RLAT)/GRDR)
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',4).NE.0) RETURN
  102 RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',5).NE.0) RETURN
      IF (RLAT.LT.XLAT-.5D0*DLAT) GO TO 102
      CALL MDPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',6).NE.0) RETURN
      IF (RLON.LT.XLON-.5D0*GLON) GO TO 101
C
C Round the latitude limits to appropriate multiples of GLAT.
C
      SLAT=GLAT*FLOR(SLAT/GLAT)
      IF (SLAT.LE.-90.D0) SLAT=SLAT+GLAT
      BLAT=GLAT*CEIL(BLAT/GLAT)
      IF (BLAT.GE.+90.D0) BLAT=BLAT-GLAT
C
C If a fast-path cylindrical equidistant projection is in use and either
C or both of the poles is within the (rectangular) perimeter, arrange
C for the parallels at -90 and/or +90 to be drawn.
C
      IF (.NOT.(IPRJ.EQ.11)) GO TO 10013
      CALL MDPTRN (-90.D0,PHOC,U,V)
      IF (ICFELL('MDPGRM',7).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                SLAT=SLAT-GLAT
      CALL MDPTRN (+90.D0,PHOC,U,V)
      IF (ICFELL('MDPGRM',8).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                BLAT=BLAT+GLAT
10013 CONTINUE
C
C Draw the parallels.
C
      XLAT=SLAT-GLAT
  103 XLAT=XLAT+GLAT
      RLAT=MAX(-90.D0,MIN(+90.D0,XLAT))
      IF (.NOT.(DINT(GRPO/1000.D0).EQ.0.D0.OR.ABS(RLAT).LE.DINT(GRPO/100
     +0.D0))) GO TO 10014
      RLON=FLOR(SLON)
      XLON=MIN(CEIL(BLON),RLON+360.D0)
      DLON=(XLON-RLON)/CEIL((XLON-RLON)/GRDR)
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',9).NE.0) RETURN
  104 RLON=RLON+DLON
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',10).NE.0) RETURN
      IF (RLON.LT.XLON-.5D0*DLON) GO TO 104
      CALL MDPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',11).NE.0) RETURN
10014 CONTINUE
      IF (XLAT.LT.BLAT-.5D0*GLAT) GO TO 103
C
C Restore the color index, and dash pattern.
C
      CALL MDPCHM (-2,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',12).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
