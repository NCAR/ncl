C
C $Id: mapgrm.f,v 1.11 1999-04-02 23:00:27 kennison Exp $
C
      SUBROUTINE MAPGRM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      DIMENSION IAM(*),XCS(*),YCS(*),IAI(*),IAG(*)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE   /MAPCM1/
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
      IF (ICFELL('MAPGRM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MAPINT
      IF (ICFELL('MAPGRM',2).NE.0) RETURN
10000 CONTINUE
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
C Reset the color index and dash pattern for the grid.
C
      CALL MAPCHM (2,IDSH,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',3).NE.0) RETURN
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
      IF (.NOT.(IPRJ.EQ.3.OR.IPRJ.EQ.4.OR.IPRJ.EQ.6)) GO TO 10001
      IF (.NOT.(PHIA.GT.+89.9999)) GO TO 10002
      SLAT=SLAT+SRCH
      IF (IPRJ.EQ.3) SLAT=SLAT+SRCH
10002 CONTINUE
      IF (.NOT.(PHIA.LT.-89.9999)) GO TO 10003
      BLAT=BLAT-SRCH
      IF (IPRJ.EQ.3) BLAT=BLAT-SRCH
10003 CONTINUE
10001 CONTINUE
C
C RLON is the smallest longitude for which a meridian is to be drawn,
C XLON the biggest.  Avoid drawing a given meridian twice.
C
      RLON=GLON*FLOOR(SLON/GLON)
      XLON=GLON*CLING(BLON/GLON)
C
      IF (.NOT.(XLON-RLON.GT.359.9999)) GO TO 10004
      IF (.NOT.(IPRJ.EQ.1)) GO TO 10005
      RLON=GLON*CLING((PHOC-179.9999)/GLON)
      XLON=GLON*FLOOR((PHOC+179.9999)/GLON)
      GO TO 10006
10005 CONTINUE
      IF (.NOT.(IPRJ.GE.2.AND.IPRJ.LE.10)) GO TO 10007
      XLON=XLON-GLON
      IF (XLON-RLON.GT.359.9999) XLON=XLON-GLON
10006 CONTINUE
10007 CONTINUE
10004 CONTINUE
C
C OLAT is the latitude at which meridians that do not extend all the
C way to the poles are to stop.
C
      IF (.NOT.(IPRJ.EQ.11.OR.IPRJ.EQ.12.OR.IPRJ.EQ.14)) GO TO 10008
      OLAT=90.
      GO TO 10009
10008 CONTINUE
      IF (.NOT.(INT(GRPO/1000.).EQ.0)) GO TO 10010
      OLAT=GLAT*FLOOR(89.9999/GLAT)
      GO TO 10011
10010 CONTINUE
      OLAT=GLAT*FLOOR(MIN(89.9999,REAL(INT(GRPO/1000.)))/GLAT)
10011 CONTINUE
10009 CONTINUE
C
C Draw the meridians.
C
      RLON=RLON-GLON
  101 RLON=RLON+GLON
      XLAT=OLAT
      IF (.NOT.(MOD(GRPO,1000.).GT.0.)) GO TO 10012
      IF (MOD(RLON,MOD(GRPO,1000.)).EQ.0.) XLAT=90.
10012 CONTINUE
      RLAT=MAX(SLAT,-XLAT)
      XLAT=MIN(BLAT,XLAT)
      DLAT=(XLAT-RLAT)/CLING((XLAT-RLAT)/GRDR)
      CALL MAPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',4).NE.0) RETURN
  102 RLAT=RLAT+DLAT
      CALL MAPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',5).NE.0) RETURN
      IF (RLAT.LT.XLAT-.5*DLAT) GO TO 102
      CALL MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',6).NE.0) RETURN
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
      IF (.NOT.(IPRJ.EQ.11)) GO TO 10013
      CALL MAPTRN (-90.,PHOC,U,V)
      IF (ICFELL('MAPGRM',7).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                  SLAT=SLAT-GLAT
      CALL MAPTRN (+90.,PHOC,U,V)
      IF (ICFELL('MAPGRM',8).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                  BLAT=BLAT+GLAT
10013 CONTINUE
C
C Draw the parallels.
C
      XLAT=SLAT-GLAT
  103 XLAT=XLAT+GLAT
      RLAT=MAX(-90.,MIN(+90.,XLAT))
      IF (.NOT.(INT(GRPO/1000.).EQ.0.OR.ABS(RLAT).LE.REAL(INT(GRPO/1000.
     +)))) GO TO 10014
      RLON=FLOOR(SLON)
      XLON=MIN(CLING(BLON),RLON+360.)
      DLON=(XLON-RLON)/CLING((XLON-RLON)/GRDR)
      CALL MAPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',9).NE.0) RETURN
  104 RLON=RLON+DLON
      CALL MAPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',10).NE.0) RETURN
      IF (RLON.LT.XLON-.5*DLON) GO TO 104
      CALL MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',11).NE.0) RETURN
10014 CONTINUE
      IF (XLAT.LT.BLAT-.5*GLAT) GO TO 103
C
C Restore the color index, and dash pattern.
C
      CALL MAPCHM (-2,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',12).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
