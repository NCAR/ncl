C
C $Id: mapgrd.f,v 1.10 1999-04-02 22:59:29 kennison Exp $
C
      SUBROUTINE MAPGRD
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
C Define required constants.
C
      DATA DTOR / .017453292519943 /
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
      IF (ICFELL('MAPGRD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPGRD',2).NE.0) RETURN
      END IF
C
C If the grid is suppressed, do nothing.
C
      IF (GRID.LE.0.) RETURN
C
C Otherwise, set the latitude and longitude grid spacings.
C
      GLAT=GRID
      GLON=GRID
      IF (GRLA.GT.0.) GLAT=GRLA
      IF (GRLO.GT.0.) GLON=GRLO
C
C Reset the color index, dotting, and dash pattern for the grid.
C
      CALL MAPCHI (2,0,IDSH)
      IF (ICFELL('MAPGRD',3).NE.0) RETURN
C
C Transfer the latitude/longitude limits computed by MAPINT to local,
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
      IF (IPRJ.EQ.3.OR.IPRJ.EQ.4.OR.IPRJ.EQ.6) THEN
        IF (PHIA.GT.+89.9999) THEN
          SLAT=SLAT+SRCH
          IF (IPRJ.EQ.3) SLAT=SLAT+SRCH
        END IF
        IF (PHIA.LT.-89.9999) THEN
          BLAT=BLAT-SRCH
          IF (IPRJ.EQ.3) BLAT=BLAT-SRCH
        END IF
      END IF
C
C RLON is the smallest longitude for which a meridian is to be drawn,
C XLON the biggest.  Avoid drawing a given meridian twice.
C
      RLON=GLON*FLOOR(SLON/GLON)
      XLON=GLON*CLING(BLON/GLON)
C
      IF (XLON-RLON.GT.359.9999) THEN
        IF (IPRJ.EQ.1) THEN
          RLON=GLON*CLING((PHOC-179.9999)/GLON)
          XLON=GLON*FLOOR((PHOC+179.9999)/GLON)
        ELSE IF (IPRJ.GE.2.AND.IPRJ.LE.10) THEN
          XLON=XLON-GLON
          IF (XLON-RLON.GT.359.9999) XLON=XLON-GLON
        END IF
      END IF
C
C OLAT is the latitude at which meridians that do not extend all the
C way to the poles are to stop.
C
      IF (IPRJ.EQ.11.OR.IPRJ.EQ.12.OR.IPRJ.EQ.14) THEN
        OLAT=90.
      ELSE
        IF (INT(GRPO/1000.).EQ.0) THEN
          OLAT=GLAT*FLOOR(89.9999/GLAT)
        ELSE
          OLAT=GLAT*FLOOR(MIN(89.9999,REAL(INT(GRPO/1000.)))/GLAT)
        END IF
      END IF
C
C Draw the meridians.
C
      RLON=RLON-GLON
  101 RLON=RLON+GLON
      XLAT=OLAT
      IF (MOD(GRPO,1000.).GT.0.) THEN
        IF (MOD(RLON,MOD(GRPO,1000.)).EQ.0.) XLAT=90.
      END IF
      RLAT=MAX(SLAT,-XLAT)
      XLAT=MIN(BLAT,XLAT)
      DLAT=(XLAT-RLAT)/CLING((XLAT-RLAT)/GRDR)
      CALL MAPIT (RLAT,RLON,0)
      IF (ICFELL('MAPGRD',4).NE.0) RETURN
  102 RLAT=RLAT+DLAT
      CALL MAPIT (RLAT,RLON,1)
      IF (ICFELL('MAPGRD',5).NE.0) RETURN
      IF (RLAT.LT.XLAT-.5*DLAT) GO TO 102
      IF (RLON.LT.XLON-.5*GLON) GO TO 101
C
C Round the latitude limits to appropriate multiples of GLAT.
C
      SLAT=GLAT*FLOOR(SLAT/GLAT)
      IF (SLAT.LE.-90.) SLAT=SLAT+GLAT
      BLAT=GLAT*CLING(BLAT/GLAT)
      IF (BLAT.GE.+90.) BLAT=BLAT-GLAT
C
C If a fast-path cylindrical equidistant projection is in use and either
C or both of the poles is within the (rectangular) perimeter, arrange
C for the parallels at -90 and/or +90 to be drawn.
C
      IF (IPRJ.EQ.11) THEN
        CALL MAPTRN (-90.,PHOC,U,V)
        IF (ICFELL('MAPGRD',6).NE.0) RETURN
        IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                    SLAT=SLAT-GLAT
        CALL MAPTRN (+90.,PHOC,U,V)
        IF (ICFELL('MAPGRD',7).NE.0) RETURN
        IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                    BLAT=BLAT+GLAT
      END IF
C
C Draw the parallels.
C
      XLAT=SLAT-GLAT
  103 XLAT=XLAT+GLAT
      RLAT=MAX(-90.,MIN(+90.,XLAT))
      IF (INT(GRPO/1000.).EQ.0.OR.ABS(RLAT).LE.REAL(INT(GRPO/1000.)))
     +                                                           THEN
        RLON=FLOOR(SLON)
        XLON=MIN(CLING(BLON),RLON+360.)
        DLON=(XLON-RLON)/CLING((XLON-RLON)/GRDR)
        CALL MAPIT (RLAT,RLON,0)
        IF (ICFELL('MAPGRD',8).NE.0) RETURN
  104   RLON=RLON+DLON
        CALL MAPIT (RLAT,RLON,1)
        IF (ICFELL('MAPGRD',9).NE.0) RETURN
        IF (RLON.LT.XLON-.5*DLON) GO TO 104
      END IF
      IF (XLAT.LT.BLAT-.5*GLAT) GO TO 103
C
C Restore the color index, dotting, and dash pattern.
C
      CALL MAPCHI (-2,0,0)
      IF (ICFELL('MAPGRD',10).NE.0) RETURN
C
C Draw the limb lines.
C
      CALL MAPLMB
      IF (ICFELL('MAPGRD',11).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
