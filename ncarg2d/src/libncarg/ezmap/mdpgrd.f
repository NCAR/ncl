C
C $Id: mdpgrd.f,v 1.11 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPGRD
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
C Declare local variables.
C
        DOUBLE PRECISION BLAT,BLON,DLAT,DLON,GLAT,GLON,OLAT,RLAT,
     +                   RLON,SLAT,SLON,U,V,X,XLAT,XLON
C
C Declare the type of two local arithmetic statement functions and the
C argument used with them.
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
        IF (ICFELL('MDPGRD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPGRD',2).NE.0) RETURN
        END IF
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
C Reset the color index, dotting, and dash pattern for the grid.
C
        CALL MDPCHI (2,0,IDSH)
        IF (ICFELL('MDPGRD',3).NE.0) RETURN
C
C Transfer the latitude/longitude limits computed by MAPINT to local,
C modifiable variables.
C
        BLAT=BLAM
        BLON=BLOM
        SLAT=SLAM
        SLON=SLOM
C
C For certain azimuthal projections centered at a pole, the latitude
C limit furthest from the pole needs adjustment to make it projectable
C and visible.  Otherwise, we have trouble with portions of meridians
C disappearing.
C
        IF (IPRJ.EQ.3.OR.IPRJ.EQ.4.OR.IPRJ.EQ.6) THEN
          IF (PLTO.GT.+89.999999D0) THEN
            SLAT=SLAT+SRCH
            IF (IPRJ.EQ.3) SLAT=SLAT+SRCH
          END IF
          IF (PLTO.LT.-89.999999D0) THEN
            BLAT=BLAT-SRCH
            IF (IPRJ.EQ.3) BLAT=BLAT-SRCH
          END IF
        END IF
C
C RLON is the smallest longitude for which a meridian is to be drawn,
C XLON the biggest.  Avoid drawing a given meridian twice.
C
        RLON=GLON*FLOR(SLON/GLON)
        XLON=GLON*CEIL(BLON/GLON)
C
        IF (XLON-RLON.GT.359.999999D0) THEN
          IF (IPRJ.EQ.1) THEN
            RLON=GLON*CEIL((PLNO-179.999999D0)/GLON)
            XLON=GLON*FLOR((PLNO+179.999999D0)/GLON)
          ELSE IF (IPRJ.GE.2.AND.IPRJ.LE.15) THEN
            XLON=XLON-GLON
            IF (XLON-RLON.GT.359.999999D0) XLON=XLON-GLON
          END IF
        END IF
C
C OLAT is the latitude at which meridians that do not extend all the
C way to the poles are to stop.
C
        IF (IPRJ.EQ.16.OR.IPRJ.EQ.17.OR.IPRJ.EQ.19.OR.IPRJ.EQ.20.OR.
     +                                  IPRJ.EQ.24.OR.IPRJ.EQ.25) THEN
          OLAT=90.D0
        ELSE
          IF (DINT(GRPO/1000.D0).EQ.0.D0) THEN
            OLAT=GLAT*FLOR(89.999999D0/GLAT)
          ELSE
            OLAT=GLAT*FLOR(MIN(89.999999D0,DINT(GRPO/1000.D0))/GLAT)
          END IF
        END IF
C
C Draw the meridians.
C
        RLON=RLON-GLON
  101   RLON=RLON+GLON
        XLAT=OLAT
        IF (MOD(GRPO,1000.D0).GT.0.D0) THEN
          IF (MOD(RLON,MOD(GRPO,1000.D0)).EQ.0.D0) XLAT=90.D0
        END IF
        RLAT=MAX(SLAT,-XLAT)
        XLAT=MIN(BLAT, XLAT)
        DLAT=(XLAT-RLAT)/CEIL((XLAT-RLAT)/GRDR)
        CALL MDPIT (RLAT,RLON,0)
        IF (ICFELL('MDPGRD',4).NE.0) RETURN
  102   RLAT=RLAT+DLAT
        CALL MDPIT (RLAT,RLON,1)
        IF (ICFELL('MDPGRD',5).NE.0) RETURN
        IF (RLAT.LT.XLAT-.5D0*DLAT) GO TO 102
        CALL MDPIQ
        IF (ICFELL('MDPGRD',6).NE.0) RETURN
        IF (RLON.LT.XLON-.5D0*GLON) GO TO 101
C
C Round the latitude limits to appropriate multiples of GLAT.
C
        SLAT=GLAT*FLOR(SLAT/GLAT)
        IF (SLAT.LE.-90.D0) SLAT=SLAT+GLAT
        BLAT=GLAT*CEIL(BLAT/GLAT)
        IF (BLAT.GE.+90.D0) BLAT=BLAT-GLAT
C
C If a fast-path cylindrical equidistant or cylindrical equal-area
C projection is in use and either or both of the poles is within the
C (rectangular) perimeter, arrange for the parallels at -90 and/or +90
C to be drawn.
C
        IF (IPRJ.EQ.16.OR.IPRJ.EQ.20) THEN
          CALL MDPTRN (-90.D0,PLNO,U,V)
          IF (ICFELL('MDPGRD',7).NE.0) RETURN
          IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                    SLAT=SLAT-GLAT
          CALL MDPTRN (+90.D0,PLNO,U,V)
          IF (ICFELL('MDPGRD',8).NE.0) RETURN
          IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                    BLAT=BLAT+GLAT
        END IF
C
C Draw the parallels.
C
        XLAT=SLAT-GLAT
  103   XLAT=XLAT+GLAT
        RLAT=MAX(-90.D0,MIN(+90.D0,XLAT))
        IF (DINT(GRPO/1000.D0).EQ.0.D0.OR.
     +      ABS(RLAT).LE.DINT(GRPO/1000.D0)) THEN
          RLON=FLOR(SLON)
          XLON=MIN(CEIL(BLON),RLON+360.D0)
          DLON=(XLON-RLON)/CEIL((XLON-RLON)/GRDR)
          CALL MDPIT (RLAT,RLON,0)
          IF (ICFELL('MDPGRD',9).NE.0) RETURN
  104     RLON=RLON+DLON
          CALL MDPIT (RLAT,RLON,1)
          IF (ICFELL('MDPGRD',10).NE.0) RETURN
          IF (RLON.LT.XLON-.5D0*DLON) GO TO 104
          CALL MDPIQ
          IF (ICFELL('MDPGRD',11).NE.0) RETURN
        END IF
        IF (XLAT.LT.BLAT-.5D0*GLAT) GO TO 103
C
C Restore the color index, dotting, and dash pattern.
C
        CALL MDPCHI (-2,0,0)
        IF (ICFELL('MDPGRD',12).NE.0) RETURN
C
C Draw the limb lines.
C
        CALL MDPLMB
        IF (ICFELL('MDPGRD',13).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
