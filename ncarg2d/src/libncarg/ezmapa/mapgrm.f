C
C $Id: mapgrm.f,v 1.8 1995-12-13 18:32:07 kennison Exp $
C
      SUBROUTINE MAPGRM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      DIMENSION IAM(*),XCS(*),YCS(*),IAI(*),IAG(*)
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE /MAPCM1/
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
      IF (.NOT.(ICFELL('MAPGRM - UNCLEARED PRIOR ERROR',1).NE.0))
     +GO TO 10000
      IIER=-1
      RETURN
10000 CONTINUE
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
C Reset the color index and dash pattern for the grid.
C
      CALL MAPCHM (2,IDSH,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',2).NE.0) RETURN
C
C Set the flags IMF and IPF, which are true if and only if meridians and
C parallels, respectively, are straight lines and it is "safe" to draw
C them using long line segments.  What we have to be sure of is that at
C least one of the two endpoints of each meridian, or its midpoint, will
C be visible.  (If two points are invisible, MAPITM draws nothing, even
C though the line joining them may be visible along part of its length.)
C
      IF (.NOT.(IPRJ.GE.1.AND.IPRJ.LE.6)) GO TO 10001
      IF (.NOT.(ELPF)) GO TO 10002
      IMF=(UCEN/URNG)**2+(VCEN/VRNG)**2.LT.1.
      GO TO 10003
10002 CONTINUE
      IMF=UMIN*UMAX.LT.0..AND.VMIN*VMAX.LT.0.
10003 CONTINUE
      IF (IPRJ.NE.1) IMF=IMF.AND.ABS(PHIA).GE.89.9999
      GO TO 10004
10001 CONTINUE
      IF (.NOT.((IPRJ.EQ.10.OR.IPRJ.EQ.11).AND.ISSL.NE.0)) GO TO 10005
      IMF=.TRUE.
      GO TO 10004
10005 CONTINUE
      IMF=.FALSE.
10004 CONTINUE
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
      IF (.NOT.(IPRJ.EQ.3.OR.IPRJ.EQ.4.OR.IPRJ.EQ.6)) GO TO 10006
      IF (.NOT.(PHIA.GT.+89.9999)) GO TO 10007
      SLAT=SLAT+SRCH
      IF (IPRJ.EQ.3) SLAT=SLAT+SRCH
10007 CONTINUE
      IF (.NOT.(PHIA.LT.-89.9999)) GO TO 10008
      BLAT=BLAT-SRCH
      IF (IPRJ.EQ.3) BLAT=BLAT-SRCH
10008 CONTINUE
10006 CONTINUE
C
C RLON is the smallest longitude for which a meridian is to be drawn,
C XLON the biggest.  Avoid drawing a given meridian twice.
C
      RLON=GRID*FLOOR(SLON/GRID)
      XLON=GRID*CLING(BLON/GRID)
      IF (.NOT.(XLON-RLON.GT.359.9999)) GO TO 10009
      IF (.NOT.(IPRJ.EQ.1)) GO TO 10010
      RLON=GRID*CLING((PHOC-179.9999)/GRID)
      XLON=GRID*FLOOR((PHOC+179.9999)/GRID)
      GO TO 10011
10010 CONTINUE
      IF (.NOT.(IPRJ.GE.2.AND.IPRJ.LE.9)) GO TO 10012
      XLON=XLON-GRID
      IF (XLON-RLON.GT.359.9999) XLON=XLON-GRID
10011 CONTINUE
10012 CONTINUE
10009 CONTINUE
C
C OLAT is the latitude at which meridians which are not multiples of 90
C are to stop.  (Except on certain fast-path cylindrical projections,
C only the meridians at longitudes which are multiples of 90 run all
C the way to the poles.  This avoids a lot of clutter.)
C
      IF (.NOT.(IPRJ.EQ.10.OR.IPRJ.EQ.11)) GO TO 10013
      OLAT=90.
      GO TO 10014
10013 CONTINUE
      OLAT=GRID*FLOOR(89.9999/GRID)
10014 CONTINUE
C
C Draw the meridians.
C
      RLON=RLON-GRID
  101 RLON=RLON+GRID
      XLAT=OLAT
      IF (AMOD(RLON,90.).EQ.0.) XLAT=90.
      RLAT=AMAX1(SLAT,-XLAT)
      XLAT=AMIN1(BLAT,XLAT)
      IF (.NOT.(IMF)) GO TO 10015
      DLAT=.2*(XLAT-RLAT)
      GO TO 10016
10015 CONTINUE
      DLAT=(XLAT-RLAT)/CLING((XLAT-RLAT)/GRDR)
10016 CONTINUE
      CALL MAPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',3).NE.0) RETURN
  102 RLAT=RLAT+DLAT
      CALL MAPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',4).NE.0) RETURN
      IF (RLAT.LT.XLAT-.5*DLAT) GO TO 102
      CALL MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',5).NE.0) RETURN
      IF (RLON.LT.XLON-.5*GRID) GO TO 101
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
      IF (.NOT.(IPRJ.EQ.10)) GO TO 10017
      CALL MAPTRN (-90.,PHOC,U,V)
      IF (ICFELL('MAPGRM',6).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                  SLAT=SLAT-GRID
      CALL MAPTRN (90.,PHOC,U,V)
      IF (ICFELL('MAPGRM',7).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                  BLAT=BLAT+GRID
10017 CONTINUE
C
C Draw the parallels.
C
      XLAT=SLAT-GRID
  103 XLAT=XLAT+GRID
      RLAT=AMAX1(-90.,AMIN1(90.,XLAT))
      RLON=FLOOR(SLON)
      XLON=AMIN1(CLING(BLON),RLON+360.)
      IF (.NOT.(IPF)) GO TO 10018
      DLON=.2*(XLON-RLON)
      GO TO 10019
10018 CONTINUE
      DLON=(XLON-RLON)/CLING((XLON-RLON)/GRDR)
10019 CONTINUE
      CALL MAPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',8).NE.0) RETURN
  104 RLON=RLON+DLON
      CALL MAPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',9).NE.0) RETURN
      IF (RLON.LT.XLON-.5*DLON) GO TO 104
      CALL MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',10).NE.0) RETURN
      IF (XLAT.LT.BLAT-.5*GRID) GO TO 103
C
C Restore the color index, and dash pattern.
C
      CALL MAPCHM (-2,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPGRM',11).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
