C
C $Id: mapit.f,v 1.5 1994-05-03 21:17:27 kennison Exp $
C
      SUBROUTINE MAPIT (XLAT,XLON,IFST)
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
      COMMON /MAPCM8/ P,Q,R
      SAVE /MAPCM8/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
C
      DIMENSION CPRJ(3)
C
      DIMENSION RLATI(100),RLONI(100)
C
      SAVE IVSO,POLD,QOLD,UOLD,VOLD,XLTO,XLNO,RMLO
C
      DATA CPRJ / 360.,6.28318530717959,4. /
C
      DATA IVSO,POLD,QOLD,UOLD,VOLD / 0,0.,0.,0.,0. /
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPIT - UNCLEARED PRIOR ERROR',1).NE.0) THEN
        IIER=-1
        RETURN
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
      IF (IPRJ.EQ.2.OR.IPRJ.EQ.4.OR.IPRJ.EQ.5.OR.IPRJ.EQ.6) THEN
        CPLT=COS(DTOR*PHIA)
        SPLT=SIN(DTOR*PHIA)
        CPLN=COS(DTOR*PHOC)
        SPLN=SIN(DTOR*PHOC)
        CXLT=COS(DTOR*XLAT)
        SXLT=SIN(DTOR*XLAT)
        CXLN=COS(DTOR*XLON)
        SXLN=SIN(DTOR*XLON)
        APTX=2.*RTOD*ASIN(SQRT((CXLT*CXLN-CPLT*CPLN)**2+
     +                         (CXLT*SXLN-CPLT*SPLN)**2+
     +                         (SXLT     -SPLT     )**2)/2.)
        IF (IPRJ.EQ.2) THEN
          IF (APTX.LE.179.999) THEN
            RMUL=2./(1.+COS(DTOR*APTX))
          ELSE
            RMUL=201.
          END IF
        ELSE IF (IPRJ.EQ.4) THEN
          IF (APTX.LE.179.999) THEN
            RMUL=2./SQRT(2.*(1.+COS(DTOR*APTX)))
          ELSE
            RMUL=201.
          END IF
        ELSE IF (IPRJ.EQ.5) THEN
          IF (APTX.LT.89.999) THEN
            RMUL=1./COS(DTOR*APTX)
          ELSE
            RMUL=201.
          END IF
        ELSE IF (IPRJ.EQ.6) THEN
          IF (APTX.LE..001) THEN
            RMUL=1.
          ELSE IF (APTX.LT.179.999) THEN
            RMUL=DTOR*APTX/SIN(DTOR*APTX)
          ELSE
            RMUL=201.
          END IF
        END IF
        IF (IFST.NE.0) THEN
          NOPI=MAX(0,MIN(100,(INT(MAX(RMLO,RMUL))-1)/2))
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
  100 IF (IOPI.LT.NOPI) THEN
        IOPI=IOPI+1
        RLAT=RLATI(IOPI)
        RLON=RLONI(IOPI)
      ELSE
        IOPI=IOPI+1
        RLAT=XLAT
        RLON=XLON
      END IF
C
C Project the point (RLAT,RLON) to (U,V).
C
      CALL MAPTRN (RLAT,RLON,U,V)
      IF (ICFELL('MAPIT',2).NE.0) RETURN
C
C For the sake of efficiency, execute one of two parallel algorithms,
C depending on whether an elliptical or a rectangular perimeter is in
C use.  (That way, we test ELPF only once.)
C
      IF (ELPF) THEN
C
C Elliptical - assume the new point is visible until we find otherwise.
C
        IVIS=1
C
C See if the new point is invisible.
C
        IF (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.) THEN
C
C The new point is invisible.  Reset the visibility flag.
C
          IVIS=0
C
C If the new point is a "first point" or if the last point was not
C visible or if the new point is invisible because its projection is
C undefined, draw nothing.  The possible existence of a visible segment
C along the line joining two invisible points is intentionally ignored,
C for reasons of efficiency.  For this reason, objects should not be
C drawn using long line segments.
C
          IF (IFST.EQ.0.OR.IVSO.EQ.0.OR.U.GE.1.E12) GO TO 108
C
C Otherwise, the new point is not a "first point", the last point was
C visible, and the projection of the new point is defined, so we need
C to continue the line.  First, if there's a cross-over problem, move
C the new point to its alternate position.  This may make it visible.
C
          IF (ABS(P-POLD).GT.UEPS.OR.ABS(Q-QOLD).GT.VEPS) THEN
C
            IF (JPRJ.GE.7) THEN
              P=P-SIGN(CPRJ(JPRJ-6),P)
              U=P
              IF (JPRJ.EQ.9) U=U*SQRT(1.-V*V)
            ELSE
              GO TO 108
            END IF
C
            IF (((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.) THEN
              IVIS=1
              GO TO 107
            END IF
C
          END IF
C
C If it's still invisible, interpolate to the edge of the frame, extend
C the line to that point, and quit.
C
          CALL MAPTRE (UOLD,VOLD,U,V,UINT,VINT)
          CALL MAPVP (UOLD,VOLD,UINT,VINT)
          IF (ICFELL('MAPIT',3).NE.0) RETURN
          GO TO 108
C
        END IF
C
C The new point is visible.  If it's the first point of a line, go start
C a new line.
C
        IF (IFST.EQ.0.OR.UOLD.GE.1.E12) GO TO 106
C
C The new point is visible, but it's not the first point of a line.
C Check for cross-over problems.
C
        IF (ABS(P-POLD).GT.UEPS.OR.ABS(Q-QOLD).GT.VEPS) GO TO 101
C
C The new point is visible, it's not the first point of a line, and
C there are no cross-over problems.  If the old point was invisible,
C jump to draw the visible portion of the line from the old point to
C the new one.
C
        IF (IVSO.EQ.0) GO TO 102
C
C The new point is visible, it's not the first point of a line, there
C are no cross-over problems, and the last point was visible.  Jump to
C just continue the line.
C
        GO TO 107
C
C We have the most difficult case.  The new point is visible, it's not
C the first point of a line, and there is a cross-over problem.  None,
C one, or two segments may need to be drawn.
C
  101   IF (JPRJ.LT.7) GO TO 106
C
C If the old point was visible, generate the alternate projection of the
C new point and draw the visible portion of the line segment joining the
C old point to the alternate projection point.
C
        IF (IVSO.NE.0) THEN
C
          UTMP=P-SIGN(CPRJ(JPRJ-6),P)
          VTMP=Q
          IF (JPRJ.EQ.9) UTMP=UTMP*SQRT(1.-VTMP*VTMP)
C
          IF (((UTMP-UCEN)/URNG)**2+((VTMP-VCEN)/VRNG)**2.GT.1.) THEN
            CALL MAPTRE (UOLD,VOLD,UTMP,VTMP,UTMP,VTMP)
          END IF
C
          CALL MAPVP (UOLD,VOLD,UTMP,VTMP)
          IF (ICFELL('MAPIT',4).NE.0) RETURN
C
        END IF
C
C Now generate an alternate projection of the old point close to the new
C one and draw the visible portion of the line segment joining it to the
C new point.
C
        UOLD=POLD-SIGN(CPRJ(JPRJ-6),POLD)
        IF (JPRJ.EQ.9) UOLD=UOLD*SQRT(1.-VOLD*VOLD)
C
        IF (((UOLD-UCEN)/URNG)**2+((VOLD-VCEN)/VRNG)**2.LE.1.) GO TO 105
C
C Move (UOLD,VOLD) by interpolating to the edge of the frame.
C
  102   CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
C
      ELSE
C
C Rectangular - repeat the above code, changing the tests for a point's
C being inside/outside the perimeter.  Commenting will be abbreviated.
C
        IVIS=1
C
        IF (U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.GT.VMAX) THEN
C
          IVIS=0
C
          IF (IFST.EQ.0.OR.IVSO.EQ.0.OR.U.GE.1.E12) GO TO 108
C
          IF (ABS(P-POLD).GT.UEPS.OR.ABS(Q-QOLD).GT.VEPS) THEN
C
            IF (JPRJ.GE.7) THEN
              P=P-SIGN(CPRJ(JPRJ-6),P)
              U=P
              IF (JPRJ.EQ.9) U=U*SQRT(1.-V*V)
            ELSE
              GO TO 108
            END IF
C
            IF (U.GE.UMIN.AND.U.LE.UMAX.AND.
     +          V.GE.VMIN.AND.V.LE.VMAX) THEN
              IVIS=1
              GO TO 107
            END IF
          END IF
C
          CALL MAPTRP (UOLD,VOLD,U,V,UINT,VINT)
          CALL MAPVP (UOLD,VOLD,UINT,VINT)
          IF (ICFELL('MAPIT',5).NE.0) RETURN
          GO TO 108
C
        END IF
C
        IF (IFST.EQ.0.OR.UOLD.GE.1.E12) GO TO 106
C
        IF (ABS(P-POLD).GT.UEPS.OR.ABS(Q-QOLD).GT.VEPS) GO TO 103
C
        IF (IVSO.EQ.0) GO TO 104
C
        GO TO 107
C
  103   IF (JPRJ.LT.7) GO TO 106
C
        IF (IVSO.NE.0) THEN
C
          UTMP=P-SIGN(CPRJ(JPRJ-6),P)
          VTMP=Q
          IF (JPRJ.EQ.9) UTMP=UTMP*SQRT(1.-VTMP*VTMP)
C
          IF (UTMP.LT.UMIN.OR.UTMP.GT.UMAX.OR.
     +        VTMP.LT.VMIN.OR.VTMP.GT.VMAX) THEN
            CALL MAPTRP (UOLD,VOLD,UTMP,VTMP,UTMP,VTMP)
          END IF
C
          CALL MAPVP (UOLD,VOLD,UTMP,VTMP)
          IF (ICFELL('MAPIT',6).NE.0) RETURN
        END IF
C
        UOLD=POLD-SIGN(CPRJ(JPRJ-6),POLD)
        IF (JPRJ.EQ.9) UOLD=UOLD*SQRT(1.-VOLD*VOLD)
C
        IF (UOLD.GE.UMIN.AND.UOLD.LE.UMAX.AND.
     +      VOLD.GE.VMIN.AND.VOLD.LE.VMAX) GO TO 105
C
  104   CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
C
      END IF
C
C Draw the visible portion of the line joining the old point to the new.
C
  105 IF (IDTL.EQ.0) THEN
        CALL FRSTD (UOLD,VOLD)
        IF (ICFELL('MAPIT',7).NE.0) THEN
          IIER=-1
          RETURN
        END IF
      ELSE
        DATL=0.
      END IF
C
      CALL MAPVP (UOLD,VOLD,U,V)
      IF (ICFELL('MAPIT',8).NE.0) RETURN
C
      GO TO 108
C
C Start a new line.
C
  106 IF (IDTL.EQ.0) THEN
        CALL FRSTD (U,V)
        IF (ICFELL('MAPIT',9).NE.0) THEN
          IIER=-1
          RETURN
        END IF
      ELSE
        DATL=0.
      END IF
C
      GO TO 108
C
C Continue the line.  If this point is too close to the last, skip it.
C
  107 IF (IFST.LT.2.AND.((U-UOLD)**2+(V-VOLD)**2)*DSSQ.LE.DPSQ)GO TO 109
C
      CALL MAPVP (UOLD,VOLD,U,V)
      IF (ICFELL('MAPIT',10).NE.0) RETURN
C
C Save information about the current point for the next call and quit.
C
  108 IVSO=IVIS
      POLD=P
      QOLD=Q
      UOLD=U
      VOLD=V
C
C If interpolation is taking place, loop back for the next point.
C
  109 IF (IOPI.LE.NOPI) GO TO 100
C
      RETURN
C
      END
