C
C $Id: mapita.f,v 1.9 1999-04-02 23:00:28 kennison Exp $
C
      SUBROUTINE MAPITA (XLAT,XLON,IFST,IAMP,IGRP,IDLT,IDRT)
C
      DIMENSION IAMP(*)
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
      COMMON /MAPCM8/ P,Q,R
      SAVE   /MAPCM8/
C
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE   /MAPCMA/
C
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE   /MAPCMC/
C
      COMMON /USGSC1/ IPRF,UTPA(15),UUMN,UUMX,UVMN,UVMX
      DOUBLE PRECISION UTPA
      SAVE   /USGSC1/
C
      DIMENSION RLATI(100),RLONI(100)
C
      SAVE XLTO,XLNO,RMLO,IVSO,RLTO,RLNO,POLD,UOLD,VOLD
C
      DATA XLTO,XLNO,RMLO,IVSO,RLTO,RLNO,POLD,UOLD,VOLD / 3*0.,0,5*0. /
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPITA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MAPINT
      IF (ICFELL('MAPITA',2).NE.0) RETURN
10000 CONTINUE
C
C Initialize the variables that control interpolation.
C
      IOPI=0
      NOPI=0
C
C If the projection is one of those in which great distortion occurs at
C points opposite the pole, see if some points need to be interpolated.
C
C Note that I originally did this for four different projections, but it
C really needs to be done only for the Lambert Equal-Area and Azimuthal
C Equidistant projections, as these are the only two that are sometimes
C used to show the entire globe.  When I added the USGS projections, I
C did not bother to provide interpolation for the other two projections
C (the Gnomonic and the Stereographic), but I also did not remove the
C interpolation that was being done for the forms of these projections
C that we already had (03/11/99).
C
      IF (.NOT.((IPRJ.EQ.0.AND.(IPRF.EQ.3.OR.IPRF.EQ.8.OR.IPRF.EQ.11.OR.
     +IPRF.EQ.12)).OR.IPRJ.EQ.2.OR.IPRJ.EQ.4.OR.IPRJ.EQ.5.OR.IPRJ.EQ.6))
     +GO TO 10001
      IF (.NOT.(IPRJ.EQ.0)) GO TO 10002
      IF (.NOT.(IPRF.EQ.3.OR.IPRF.EQ.8)) GO TO 10003
      CPLT=0.
      SPLT=1.
      CPLN=1.
      SPLN=0.
      IF (.NOT.(IPRF.EQ.8.AND.UTPA(9).EQ.0.D0)) GO TO 10004
      DTST=REAL(UTPA(3))
      GO TO 10005
10004 CONTINUE
      DTST=.5*REAL(UTPA(3)+UTPA(4))
10005 CONTINUE
      IF (.NOT.(DTST.GT.0.)) GO TO 10006
      DTST=90.-DTST
      GO TO 10007
10006 CONTINUE
      SPLT=-SPLT
      DTST=DTST+90.
10007 CONTINUE
      GO TO 10008
10003 CONTINUE
      CPLT=COS(DTOR*REAL(UTPA(6)))
      SPLT=SIN(DTOR*REAL(UTPA(6)))
      CPLN=COS(DTOR*REAL(UTPA(5)))
      SPLN=SIN(DTOR*REAL(UTPA(5)))
10008 CONTINUE
      GO TO 10009
10002 CONTINUE
      CPLT=COS(DTOR*PHIA)
      SPLT=SIN(DTOR*PHIA)
      CPLN=COS(DTOR*PHOC)
      SPLN=SIN(DTOR*PHOC)
10009 CONTINUE
      CXLT=COS(DTOR*XLAT)
      SXLT=SIN(DTOR*XLAT)
      CXLN=COS(DTOR*XLON)
      SXLN=SIN(DTOR*XLON)
      APTX=2.*RTOD*ASIN(SQRT((CXLT*CXLN-CPLT*CPLN)**2+
     +                       (CXLT*SXLN-CPLT*SPLN)**2+
     +                       (SXLT     -SPLT     )**2)/2.)
      IF (.NOT.(IPRJ.EQ.0.AND.(IPRF.EQ.3.OR.IPRF.EQ.8))) GO TO 10010
      IF (.NOT.(APTX.LE.DTST)) GO TO 10011
      RMUL=MAX(1.,MIN(51.,51.*((DTST-APTX)/DTST)**8))
      GO TO 10012
10011 CONTINUE
      RMUL=MAX(1.,MIN(51.,51.*((APTX-DTST)/(180.-DTST))**8))
10012 CONTINUE
      GO TO 10013
10010 CONTINUE
      IF (.NOT.(IPRJ.EQ.2)) GO TO 10014
      IF (.NOT.(APTX.LE.179.9999)) GO TO 10015
      RMUL=MIN(101.,1./(1.+COS(DTOR*APTX)))
      GO TO 10016
10015 CONTINUE
      RMUL=101.
10016 CONTINUE
      GO TO 10013
10014 CONTINUE
      IF (.NOT.((IPRJ.EQ.0.AND.IPRF.EQ.11).OR.IPRJ.EQ.4)) GO TO 10017
      IF (.NOT.(APTX.LE.179.9999)) GO TO 10018
      RMUL=MIN(101.,2./SQRT(2.*(1.+COS(DTOR*APTX))))
      GO TO 10019
10018 CONTINUE
      RMUL=101.
10019 CONTINUE
      GO TO 10013
10017 CONTINUE
      IF (.NOT.(IPRJ.EQ.5)) GO TO 10020
      IF (.NOT.(APTX.LT.89.9999)) GO TO 10021
      RMUL=MIN(21.,1./COS(DTOR*APTX))
      GO TO 10022
10021 CONTINUE
      RMUL=21.
10022 CONTINUE
      GO TO 10013
10020 CONTINUE
      IF (.NOT.((IPRJ.EQ.0.AND.IPRF.EQ.12).OR.IPRJ.EQ.6)) GO TO 10023
      IF (.NOT.(APTX.LE..001)) GO TO 10024
      RMUL=1.
      GO TO 10025
10024 CONTINUE
      IF (.NOT.(APTX.LT.179.9999)) GO TO 10026
      RMUL=MIN(101.,DTOR*APTX/SIN(DTOR*APTX))
      GO TO 10025
10026 CONTINUE
      RMUL=101.
10025 CONTINUE
10013 CONTINUE
10023 CONTINUE
      IF (.NOT.(IFST.NE.0)) GO TO 10027
      NOPI=MAX(0,MIN(50,INT(MAX(RMLO,RMUL))/2))
      IF (NOPI.NE.0)
     +          CALL MAPGCI (XLTO,XLNO,XLAT,XLON,NOPI,RLATI,RLONI)
10027 CONTINUE
      XLTO=XLAT
      XLNO=XLON
      RMLO=RMUL
10001 CONTINUE
C
C Return here for the next interpolated point.
C
  101 CONTINUE
      IF (.NOT.(IOPI.LT.NOPI)) GO TO 10028
      IOPI=IOPI+1
      RLAT=RLATI(IOPI)
      RLON=RLONI(IOPI)
      GO TO 10029
10028 CONTINUE
      IOPI=IOPI+1
      RLAT=XLAT
      RLON=XLON
10029 CONTINUE
C
C Project the point (RLAT,RLON) to (UNEW,VNEW), using the routine
C MAPTRA, which returns 1.E12 for UNEW and VNEW in areas outside the
C perimeter and on the "wrong" side of a limb line.  Also save PNEW
C for "crossover" testing.
C
      CALL MAPTRA (RLAT,RLON,UNEW,VNEW)
      IF (ICFELL('MAPITA',3).NE.0) RETURN
      PNEW=P
C
C If the new point is invisible, we only have to draw something if it's
C not a first point and the last point was visible, in which case we
C interpolate to find a point at the edge of the visible area and then
C extend the line we're drawing to that point.  In any case, we jump to
C save information about the new point and get another.
C
      IF (.NOT.(UNEW.GE.1.E12)) GO TO 10030
      IVIS=0
      IF (.NOT.(IFST.NE.0.AND.IVSO.NE.0)) GO TO 10031
      CALL MPITVE (RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTE,RLNE,PAPE,UCOE,VCOE)
      IF (ICFELL('MAPITA',4).NE.0) RETURN
      XCRD=UCOE
      YCRD=VCOE
      L10033=    1
      GO TO 10033
10032 CONTINUE
10031 CONTINUE
      GO TO 103
10030 CONTINUE
C
C Otherwise, the new point is visible; things get more complicated.
C
      IVIS=1
C
C If the new point is a first point, initialize a new set of line draws,
C then jump to save information about the new point and get another.
C
      IF (.NOT.(IFST.EQ.0)) GO TO 10034
      XCRD=UNEW
      YCRD=VNEW
      L10036=    1
      GO TO 10036
10035 CONTINUE
      GO TO 103
10034 CONTINUE
C
C Otherwise, the new point is visible and it's not a first point; if the
C last point was invisible, find a point at the edge of the visible area
C and start a new set of line draws there.
C
      IF (.NOT.(IVSO.EQ.0)) GO TO 10037
      CALL MPITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLTO,RLNO,POLD,UOLD,VOLD)
      IF (ICFELL('MAPITA',5).NE.0) RETURN
      IVSO=1
      XCRD=UOLD
      YCRD=VOLD
      L10036=    2
      GO TO 10036
10038 CONTINUE
10037 CONTINUE
C
C The new point is visible; so was the old one.  If the projection type
C is one of those for which "crossover" is not possible, just jump to
C extend the line to the new point.
C
      IF (JPRJ.GE.2.AND.JPRJ.LE.6) GO TO 102
C
      IF (JPRJ.EQ.0.AND.(IPRF.EQ. 6.OR.IPRF.EQ.10.OR.IPRF.EQ.11.OR.
     +                   IPRF.EQ.12.OR.IPRF.EQ.13.OR.IPRF.EQ.14.OR.
     +                   IPRF.EQ.15.OR.IPRF.EQ.23)) GO TO 102
C
C Test for "crossover"; if it has not occurred, jump to extend the line.
C
      IF (ABS(PNEW-POLD).LT.PEPS) GO TO 102
C
C The new and old points are both visible and "crossover" has occurred.
C We must extend the line to one edge of the map and restart it at the
C other edge.
C
      CALL MPITVE (RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTE,RLNE,PAPE,UCOE,VCOE)
      IF (ICFELL('MAPITA',6).NE.0) RETURN
      XCRD=UCOE
      YCRD=VCOE
      L10033=    2
      GO TO 10033
10039 CONTINUE
C
      CALL MPITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLTO,RLNO,POLD,UOLD,VOLD)
      IF (ICFELL('MAPITA',7).NE.0) RETURN
C
C Start a new series of line draws with the old point.
C
      IVSO=1
      XCRD=UOLD
      YCRD=VOLD
      L10036=    3
      GO TO 10036
10040 CONTINUE
C
C Continue the line to the new point.
C
  102 XCRD=UNEW
      YCRD=VNEW
      L10033=    3
      GO TO 10033
10041 CONTINUE
C
C Save information about the current point for the next call.
C
  103 IVSO=IVIS
      RLTO=RLAT
      RLNO=RLON
      POLD=PNEW
      UOLD=UNEW
      VOLD=VNEW
C
C If interpolation is taking place, loop back for the next point.
C
  104 IF (IOPI.LE.NOPI) GO TO 101
C
C Done.
C
      RETURN
C
C The following internal procedure is invoked to start a line.
C
10036 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10042
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPITA',8).NE.0) RETURN
10042 CONTINUE
      XCRA(1)=XCRD
      YCRA(1)=YCRD
      NCRA=1
      GO TO (10035,10038,10040) , L10036
C
C The following internal procedure is invoked to continue a line.
C
10033 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10043
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MAPITA',9).NE.0) RETURN
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10043 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=XCRD
      YCRA(NCRA)=YCRD
      GO TO (10032,10039,10041) , L10033
C
      END
