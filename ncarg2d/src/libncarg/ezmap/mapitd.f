C
C $Id: mapitd.f,v 1.5 1999-04-19 21:29:49 kennison Exp $
C
      SUBROUTINE MAPITD (XLAT,XLON,IFST)
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
      COMMON /USGSC1/ UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER IPRF
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
      IF (ICFELL('MAPITD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPITD',2).NE.0) RETURN
      END IF
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
      IF ((IPRJ.EQ.0.AND.(IPRF.EQ. 3.OR.IPRF.EQ. 8.OR.
     +                    IPRF.EQ.11.OR.IPRF.EQ.12)).OR.
     +    IPRJ.EQ.2.OR.IPRJ.EQ.4.OR.IPRJ.EQ.5.OR.IPRJ.EQ.6) THEN
        IF (IPRJ.EQ.0) THEN
          IF (IPRF.EQ.3.OR.IPRF.EQ.8) THEN
            CPLT=0.
            SPLT=1.
            CPLN=1.
            SPLN=0.
            IF (IPRF.EQ.8.AND.UTPA(9).EQ.0.D0) THEN
              DTST=REAL(UTPA(3))
            ELSE
              DTST=.5*REAL(UTPA(3)+UTPA(4))
            END IF
            IF (DTST.GT.0.) THEN
              DTST=90.-DTST
            ELSE
              SPLT=-SPLT
              DTST=DTST+90.
            END IF
          ELSE
            CPLT=COS(DTOR*REAL(UTPA(6)))
            SPLT=SIN(DTOR*REAL(UTPA(6)))
            CPLN=COS(DTOR*REAL(UTPA(5)))
            SPLN=SIN(DTOR*REAL(UTPA(5)))
          END IF
        ELSE
          CPLT=COS(DTOR*PHIA)
          SPLT=SIN(DTOR*PHIA)
          CPLN=COS(DTOR*PHOC)
          SPLN=SIN(DTOR*PHOC)
        END IF
        CXLT=COS(DTOR*XLAT)
        SXLT=SIN(DTOR*XLAT)
        CXLN=COS(DTOR*XLON)
        SXLN=SIN(DTOR*XLON)
        APTX=2.*RTOD*ASIN(SQRT((CXLT*CXLN-CPLT*CPLN)**2+
     +                         (CXLT*SXLN-CPLT*SPLN)**2+
     +                         (SXLT     -SPLT     )**2)/2.)
        IF (IPRJ.EQ.0.AND.(IPRF.EQ.3.OR.IPRF.EQ.8)) THEN
          IF (APTX.LE.DTST) THEN
            RMUL=MAX(1.,MIN(51.,51.*((DTST-APTX)/DTST)**8))
          ELSE
            RMUL=MAX(1.,MIN(51.,51.*((APTX-DTST)/(180.-DTST))**8))
          END IF
        ELSE IF (IPRJ.EQ.2) THEN
          IF (APTX.LE.179.9999) THEN
            RMUL=MIN(101.,1./(1.+COS(DTOR*APTX)))
          ELSE
            RMUL=101.
          END IF
        ELSE IF ((IPRJ.EQ.0.AND.IPRF.EQ.11).OR.IPRJ.EQ.4) THEN
          IF (APTX.LE.179.9999) THEN
            RMUL=MIN(101.,2./SQRT(2.*(1.+COS(DTOR*APTX))))
          ELSE
            RMUL=101.
          END IF
        ELSE IF (IPRJ.EQ.5) THEN
          IF (APTX.LT.89.9999) THEN
            RMUL=MIN(21.,1./COS(DTOR*APTX))
          ELSE
            RMUL=21.
          END IF
        ELSE IF ((IPRJ.EQ.0.AND.IPRF.EQ.12).OR.IPRJ.EQ.6) THEN
          IF (APTX.LE..001) THEN
            RMUL=1.
          ELSE IF (APTX.LT.179.9999) THEN
            RMUL=MIN(101.,DTOR*APTX/SIN(DTOR*APTX))
          ELSE
            RMUL=101.
          END IF
        END IF
        IF (IFST.NE.0) THEN
          NOPI=MAX(0,MIN(50,INT(MAX(RMLO,RMUL))/2))
          IF (NOPI.NE.0)
     +              CALL MAPGCI (XLTO,XLNO,XLAT,XLON,NOPI,RLATI,RLONI)
        END IF
        XLTO=XLAT
        XLNO=XLON
        RMLO=RMUL
      END IF
C
C Return here for the next interpolated point.
C
  101 IF (IOPI.LT.NOPI) THEN
        IOPI=IOPI+1
        RLAT=RLATI(IOPI)
        RLON=RLONI(IOPI)
      ELSE
        IOPI=IOPI+1
        RLAT=XLAT
        RLON=XLON
      END IF
C
C Project the point (RLAT,RLON) to (UNEW,VNEW), using the routine
C MAPTRA, which returns 1.E12 for UNEW and VNEW in areas outside the
C perimeter and on the "wrong" side of a limb line.  Also save PNEW
C for "crossover" testing.
C
      CALL MAPTRA (RLAT,RLON,UNEW,VNEW)
      IF (ICFELL('MAPITD',3).NE.0) RETURN
      PNEW=P
C
C If the new point is invisible, we only have to draw something if it's
C not a first point and the last point was visible, in which case we
C interpolate to find a point at the edge of the visible area and then
C extend the line we're drawing to that point.  In any case, we jump to
C save information about the new point and get another.
C
      IF (UNEW.GE.1.E12) THEN
        IVIS=0
        IF (IFST.NE.0.AND.IVSO.NE.0) THEN
          CALL MPITVE (RLTO,RLNO,POLD,UOLD,VOLD,
     +                 RLAT,RLON,PNEW,UNEW,VNEW,
     +                 RLTE,RLNE,PAPE,UCOE,VCOE)
          IF (ICFELL('MAPITD',4).NE.0) RETURN
          CALL MAPVPD (UOLD,VOLD,UCOE,VCOE)
          IF (ICFELL('MAPITD',5).NE.0) RETURN
        END IF
        GO TO 103
      END IF
C
C Otherwise, the new point is visible; things get more complicated.
C
      IVIS=1
C
C If the new point is a first point, initialize a new set of line draws,
C then jump to save information about the new point and get another.
C
      IF (IFST.EQ.0) THEN
        IF (IDTL.EQ.0) THEN
          CALL DPFRST (UNEW,VNEW)
          IF (ICFELL('MAPITD',6).NE.0) RETURN
        ELSE
          DATL=0.
        END IF
        GO TO 103
      END IF
C
C Otherwise, the new point is visible and it's not a first point; if the
C last point was invisible, find a point at the edge of the visible area
C and start a new set of line draws there.
C
      IF (IVSO.EQ.0) THEN
        CALL MPITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +               RLTO,RLNO,POLD,UOLD,VOLD,
     +               RLTO,RLNO,POLD,UOLD,VOLD)
        IF (ICFELL('MAPITD',7).NE.0) RETURN
        IVSO=1
        IF (IDTL.EQ.0) THEN
          CALL DPFRST (UOLD,VOLD)
          IF (ICFELL('MAPITD',8).NE.0) RETURN
        ELSE
          DATL=0.
        END IF
      END IF
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
      IF (ICFELL('MAPITD',9).NE.0) RETURN
      CALL MAPVPD (UOLD,VOLD,UCOE,VCOE)
      IF (ICFELL('MAPITD',10).NE.0) RETURN
C
      CALL MPITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLTO,RLNO,POLD,UOLD,VOLD)
      IF (ICFELL('MAPITD',11).NE.0) RETURN
C
C Start a new series of line draws with the old point.
C
      IVSO=1
      IF (IDTL.EQ.0) THEN
        CALL DPFRST (UOLD,VOLD)
        IF (ICFELL('MAPITD',12).NE.0) RETURN
      ELSE
        DATL=0.
      END IF
C
C Continue the line to the new point.  If it's too close to the old
C point and output of it isn't forced, skip it.
C
  102 IF ((IOPI.LE.NOPI.OR.IFST.LT.2).AND.
     +    ((UNEW-UOLD)**2+(VNEW-VOLD)**2)*DSSQ.LE.DPSQ)GO TO 104
      CALL MAPVPD (UOLD,VOLD,UNEW,VNEW)
      IF (ICFELL('MAPITD',13).NE.0) RETURN
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
      END
