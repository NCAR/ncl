C
C $Id: mapgrd.f,v 1.2 1993-12-21 00:32:47 kennison Exp $
C
      SUBROUTINE MAPGRD
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
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
C
C Define local logical flags.
C
      LOGICAL IMF,IPF
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
C If EZMAP needs initialization or if an error has occurred since the
C last initialization, do nothing.
C
      IF (INTF) RETURN
      IF (IIER.NE.0) RETURN
C
C If the grid is suppressed, do nothing.
C
      IF (GRID.LE.0.) RETURN
C
C Reset the color index, dotting, and dash pattern for the grid.
C
      CALL MAPCHI (2,0,IDSH)
      IF (ICFELL('MAPGRD',1).NE.0) RETURN
C
C Set the flags IMF and IPF, which are true if and only if meridians and
C parallels, respectively, are straight lines and it is "safe" to draw
C them using long line segments.  What we have to be sure of is that at
C least one of the two endpoints of each meridian, or its midpoint, will
C be visible.  (If two points are invisible, MAPIT draws nothing, even
C though the line joining them may be visible along part of its length.)
C
      IF (IPRJ.GE.1.AND.IPRJ.LE.6) THEN
        IF (ELPF) THEN
          IMF=(UCEN/URNG)**2+(VCEN/VRNG)**2.LT.1.
        ELSE
          IMF=UMIN*UMAX.LT.0..AND.VMIN*VMAX.LT.0.
        END IF
        IF (IPRJ.NE.1) IMF=IMF.AND.ABS(PHIA).GE.89.9999
      ELSE IF ((IPRJ.EQ.10.OR.IPRJ.EQ.11).AND.ISSL.NE.0) THEN
        IMF=.TRUE.
      ELSE
        IMF=.FALSE.
      END IF
C
      IPF=(IPRJ.EQ.10.OR.IPRJ.EQ.11.OR.(IPRJ.EQ.12.AND.ILTS.EQ.1)).AND.
     +    ISSL.NE.0
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
      RLON=GRID*FLOOR(SLON/GRID)
      XLON=GRID*CLING(BLON/GRID)
      IF (XLON-RLON.GT.359.9999) THEN
        IF (IPRJ.EQ.1) THEN
          RLON=GRID*CLING((PHIO-179.9999)/GRID)
          XLON=GRID*FLOOR((PHIO+179.9999)/GRID)
        ELSE IF (IPRJ.GE.2.AND.IPRJ.LE.9) THEN
          XLON=XLON-GRID
          IF (XLON-RLON.GT.359.9999) XLON=XLON-GRID
        END IF
      END IF
C
C OLAT is the latitude at which meridians which are not multiples of 90
C are to stop.  (Except on certain fast-path cylindrical projections,
C only the meridians at longitudes which are multiples of 90 run all
C the way to the poles.  This avoids a lot of clutter.)
C
      IF (IPRJ.EQ.10.OR.IPRJ.EQ.11) THEN
        OLAT=90.
      ELSE
        OLAT=GRID*FLOOR(89.9999/GRID)
      END IF
C
C Draw the meridians.
C
      RLON=RLON-GRID
  101 RLON=RLON+GRID
      XLAT=OLAT
      IF (AMOD(RLON,90.).EQ.0.) XLAT=90.
      RLAT=MAX(SLAT,-XLAT)
      XLAT=MIN(BLAT,XLAT)
      IF (IMF) THEN
        DLAT=.5*(XLAT-RLAT)
      ELSE
        DLAT=(XLAT-RLAT)/CLING((XLAT-RLAT)/GRDR)
      END IF
      CALL MAPIT (RLAT,RLON,0)
      IF (ICFELL('MAPGRD',2).NE.0) RETURN
  102 RLAT=RLAT+DLAT
      CALL MAPIT (RLAT,RLON,1)
      IF (ICFELL('MAPGRD',3).NE.0) RETURN
      IF (RLAT.LT.XLAT-.9999) GO TO 102
      IF (RLON.LT.XLON-.9999) GO TO 101
C
C Round the latitude limits to appropriate multiples of GRID.
C
      SLAT=GRID*FLOOR(SLAT/GRID)
      IF (SLAT.LE.-90.) SLAT=SLAT+GRID
      BLAT=GRID*CLING(BLAT/GRID)
      IF (BLAT.GE.90.) BLAT=BLAT-GRID
C
C If a fast-path cylindrical equidistant projection is in use and either
C or both of the poles is within the (rectangular) perimeter, arrange
C for the parallels at -90 and/or +90 to be drawn.
C
      IF (IPRJ.EQ.10) THEN
        CALL MAPTRN (-90.,PHIO,U,V)
        IF (ICFELL('MAPGRD',4).NE.0) RETURN
        IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                    SLAT=SLAT-GRID
        CALL MAPTRN (90.,PHIO,U,V)
        IF (ICFELL('MAPGRD',5).NE.0) RETURN
        IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                    BLAT=BLAT+GRID
      END IF
C
C Draw the parallels.
C
      XLAT=SLAT-GRID
  103 XLAT=XLAT+GRID
      RLAT=MAX(-90.,MIN(90.,XLAT))
      RLON=FLOOR(SLON)
      XLON=MIN(CLING(BLON),RLON+360.)
      IF (IPF) THEN
        DLON=.5*(XLON-RLON)
      ELSE
        DLON=(XLON-RLON)/CLING((XLON-RLON)/GRDR)
      END IF
      CALL MAPIT (RLAT,RLON,0)
      IF (ICFELL('MAPGRD',6).NE.0) RETURN
  104 RLON=RLON+DLON
      CALL MAPIT (RLAT,RLON,1)
      IF (ICFELL('MAPGRD',7).NE.0) RETURN
      IF (RLON.LT.XLON-.9999) GO TO 104
      IF (XLAT.LT.BLAT-.9999) GO TO 103
C
C Restore the color index, dotting, and dash pattern.
C
      CALL MAPCHI (-2,0,0)
      IF (ICFELL('MAPGRD',8).NE.0) RETURN
C
C Draw the limb lines.
C
      CALL MAPLMB
      IF (ICFELL('MAPGRD',9).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
