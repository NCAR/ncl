C
C $Id: mdpit.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPIT (XLAT,XLON,IFST)
C
        DOUBLE PRECISION XLAT,XLON
        INTEGER          IFST
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
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER          IPRF
        SAVE   /USGSC1/
C
C Declare local variables.
C
        DOUBLE PRECISION APTX,CPLN,CPLT,CXLN,CXLT,DTST,PAPE,PNEW,POLD,
     +                   RLAT,RLATI(100),RLNE,RLNO,RLON,RLONI(100),RLTE,
     +                   RLTO,RMLO,RMUL,SPLN,SPLT,SXLN,SXLT,UCOE,UNEW,
     +                   UOLD,VCOE,VNEW,VOLD,XLNO,XLTO
C
        INTEGER          IOPI,IVIS,IVSO,NOPI
C
        SAVE             IVSO,POLD,RLNO,RLTO,RMLO,UOLD,VOLD,XLNO,XLTO
C
        DATA IVSO,POLD,RLNO,RLTO,RMLO,UOLD,VOLD,XLNO,XLTO / 0,8*0.D0 /
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPIT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPIT',2).NE.0) RETURN
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
     +                      IPRF.EQ.11.OR.IPRF.EQ.12)).OR.
     +      IPRJ.EQ.2.OR.IPRJ.EQ.4.OR.IPRJ.EQ.5.OR.IPRJ.EQ.6) THEN
          IF (IPRJ.EQ.0) THEN
            IF (IPRF.EQ.3.OR.IPRF.EQ.8) THEN
              CPLT=0.D0
              SPLT=1.D0
              CPLN=1.D0
              SPLN=0.D0
              IF (IPRF.EQ.8.AND.UTPA(9).EQ.0.D0) THEN
                DTST=UTPA(3)
              ELSE
                DTST=.5D0*UTPA(3)+UTPA(4)
              END IF
              IF (DTST.GT.0.D0) THEN
                DTST=90.D0-DTST
              ELSE
                SPLT=-SPLT
                DTST=DTST+90.D0
              END IF
            ELSE
              CPLT=COS(DTOR*UTPA(6))
              SPLT=SIN(DTOR*UTPA(6))
              CPLN=COS(DTOR*UTPA(5))
              SPLN=SIN(DTOR*UTPA(5))
            END IF
          ELSE
            CPLT=COS(DTOR*PLTO)
            SPLT=SIN(DTOR*PLTO)
            CPLN=COS(DTOR*PLNO)
            SPLN=SIN(DTOR*PLNO)
          END IF
          CXLT=COS(DTOR*XLAT)
          SXLT=SIN(DTOR*XLAT)
          CXLN=COS(DTOR*XLON)
          SXLN=SIN(DTOR*XLON)
          APTX=2.D0*RTOD*ASIN(SQRT((CXLT*CXLN-CPLT*CPLN)**2+
     +                             (CXLT*SXLN-CPLT*SPLN)**2+
     +                             (SXLT     -SPLT     )**2)/2.D0)
          IF (IPRJ.EQ.0.AND.(IPRF.EQ.3.OR.IPRF.EQ.8)) THEN
            IF (APTX.LE.DTST) THEN
              RMUL=MAX(1.D0,MIN(51.D0,51.D0*((DTST-APTX)/DTST)**8))
            ELSE
              RMUL=MAX(1.D0,MIN(51.D0,51.D0*((APTX-DTST)/
     +                                               (180.D0-DTST))**8))
            END IF
          ELSE IF (IPRJ.EQ.2) THEN
            IF (APTX.LE.179.999D0) THEN
              RMUL=MIN(101.D0,1.D0/(1.D0+COS(DTOR*APTX)))
            ELSE
              RMUL=101.D0
            END IF
          ELSE IF ((IPRJ.EQ.0.AND.IPRF.EQ.11).OR.IPRJ.EQ.4) THEN
            IF (APTX.LE.179.999D0) THEN
              RMUL=MIN(101.D0,2.D0/SQRT(2.D0*(1.D0+COS(DTOR*APTX))))
            ELSE
              RMUL=101.D0
            END IF
          ELSE IF (IPRJ.EQ.5) THEN
            IF (APTX.LT.89.999D0) THEN
              RMUL=MIN(21.D0,1.D0/COS(DTOR*APTX))
            ELSE
              RMUL=21.D0
            END IF
          ELSE IF ((IPRJ.EQ.0.AND.IPRF.EQ.12).OR.IPRJ.EQ.6) THEN
            IF (APTX.LE..00001D0) THEN
              RMUL=1.D0
            ELSE IF (APTX.LT.179.999D0) THEN
              RMUL=MIN(101.D0,DTOR*APTX/SIN(DTOR*APTX))
            ELSE
              RMUL=101.D0
            END IF
          END IF
          IF (IFST.NE.0) THEN
            NOPI=MAX(0,MIN(50,INT(MAX(RMLO,RMUL))/2))
            IF (NOPI.NE.0)
     +                CALL MDPGCI (XLTO,XLNO,XLAT,XLON,NOPI,RLATI,RLONI)
          END IF
          XLTO=XLAT
          XLNO=XLON
          RMLO=RMUL
        END IF
C
C Return here for the next interpolated point.
C
  101   IF (IOPI.LT.NOPI) THEN
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
C MDPTRA, which returns 1.D12 for UNEW and VNEW in areas outside the
C perimeter and on the "wrong" side of a limb line.  Also save PNEW
C for "crossover" testing.
C
        CALL MDPTRA (RLAT,RLON,UNEW,VNEW)
        IF (ICFELL('MDPIT',3).NE.0) RETURN
        PNEW=P
C
C If the new point is invisible, we only have to draw something if it's
C not a first point and the last point was visible, in which case we
C interpolate to find a point at the edge of the visible area and then
C extend the line we're drawing to that point.  In any case, we jump to
C save information about the new point and get another.
C
        IF (UNEW.GE.1.D12) THEN
          IVIS=0
          IF (IFST.NE.0.AND.IVSO.NE.0) THEN
            CALL MDITVE (RLTO,RLNO,POLD,UOLD,VOLD,
     +                   RLAT,RLON,PNEW,UNEW,VNEW,
     +                   RLTE,RLNE,PAPE,UCOE,VCOE)
            IF (ICFELL('MDPIT',4).NE.0) RETURN
            CALL MDPVP  (UOLD,VOLD,UCOE,VCOE)
            IF (ICFELL('MDPIT',5).NE.0) RETURN
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
            CALL FRSTD (REAL(UNEW),REAL(VNEW))
            IF (ICFELL('MDPIT',6).NE.0) RETURN
          ELSE
            DATL=0.D0
          END IF
          GO TO 103
        END IF
C
C Otherwise, the new point is visible and it's not a first point; if the
C last point was invisible, find a point at the edge of the visible area
C and start a new set of line draws there.
C
        IF (IVSO.EQ.0) THEN
          CALL MDITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +                 RLTO,RLNO,POLD,UOLD,VOLD,
     +                 RLTO,RLNO,POLD,UOLD,VOLD)
          IF (ICFELL('MDPIT',7).NE.0) RETURN
          IVSO=1
          IF (IDTL.EQ.0) THEN
            CALL FRSTD (REAL(UOLD),REAL(VOLD))
            IF (ICFELL('MDPIT',8).NE.0) RETURN
          ELSE
            DATL=0.D0
          END IF
        END IF
C
C The new point is visible; so was the old one.  If the projection type
C is one of those for which "crossover" is not possible, just jump to
C extend the line to the new point.
C
        IF (IPRJ.GE.2.AND.IPRJ.LE.6) GO TO 102
C
        IF (IPRJ.EQ.0.AND.(IPRF.EQ. 6.OR.IPRF.EQ.10.OR.IPRF.EQ.11.OR.
     +                     IPRF.EQ.12.OR.IPRF.EQ.13.OR.IPRF.EQ.14.OR.
     +                     IPRF.EQ.15.OR.IPRF.EQ.23)) GO TO 102
C
C Test for "crossover"; if it has not occurred, jump to extend the line.
C
        IF (ABS(PNEW-POLD).LT.PEPS) GO TO 102
C
C The new and old points are both visible and "crossover" has occurred.
C We must extend the line to one edge of the map and restart it at the
C other edge.
C
        CALL MDITVE (RLTO,RLNO,POLD,UOLD,VOLD,
     +               RLAT,RLON,PNEW,UNEW,VNEW,
     +               RLTE,RLNE,PAPE,UCOE,VCOE)
        IF (ICFELL('MDPIT',9).NE.0) RETURN
        CALL MDPVP  (UOLD,VOLD,UCOE,VCOE)
        IF (ICFELL('MDPIT',10).NE.0) RETURN
C
        CALL MDITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +               RLTO,RLNO,POLD,UOLD,VOLD,
     +               RLTO,RLNO,POLD,UOLD,VOLD)
        IF (ICFELL('MDPIT',11).NE.0) RETURN
C
C Start a new series of line draws with the old point.
C
        IVSO=1
        IF (IDTL.EQ.0) THEN
          CALL FRSTD (REAL(UOLD),REAL(VOLD))
          IF (ICFELL('MDPIT',12).NE.0) RETURN
        ELSE
          DATL=0.D0
        END IF
C
C Continue the line to the new point.  If it's too close to the old
C point and output of it isn't forced, skip it.
C
  102   IF ((IOPI.LE.NOPI.OR.IFST.LT.2).AND.
     +      ((UNEW-UOLD)**2+(VNEW-VOLD)**2)*DSSQ.LE.DPSQ)GO TO 104
        CALL MDPVP (UOLD,VOLD,UNEW,VNEW)
        IF (ICFELL('MDPIT',13).NE.0) RETURN
C
C Save information about the current point for the next call.
C
  103   IVSO=IVIS
        RLTO=RLAT
        RLNO=RLON
        POLD=PNEW
        UOLD=UNEW
        VOLD=VNEW
C
C If interpolation is taking place, loop back for the next point.
C
  104   IF (IOPI.LE.NOPI) GO TO 101
C
C Done.
C
        RETURN
C
      END
