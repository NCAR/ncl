C
C	$Id: mapita.f,v 1.1.1.1 1992-04-17 22:32:16 ncargd Exp $
C
C
C The subroutine MAPITA.
C --- ---------- ------
C
      SUBROUTINE MAPITA (XLAT,XLON,IFST,IAMP,IGRP,IDLT,IDRT)
      DIMENSION IAMP(*)
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
      COMMON /MAPCM8/ P,Q,R
      SAVE /MAPCM8/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
      COMMON /MAPCMC/ IGI1,IGI2,NOVS,XCRA(100),YCRA(100),NCRA
      SAVE /MAPCMC/
C
      DIMENSION CPRJ(3)
C
      DIMENSION RLATI(100),RLONI(100)
C
      SAVE IVSO,POLD,QOLD,UOLD,VOLD,RLTO,RLNO,XLTO,XLNO,RMLO
C
      DATA CPRJ / 360.,6.28318530717959,4. /
C
      DATA IVSO,POLD,QOLD,UOLD,VOLD / 0,0.,0.,0.,0. /
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C Initialize the variables that control interpolation.
C
      IOPI=0
      NOPI=0
C
C If the projection is one of those in which great distortion occurs at
C points opposite the pole, see if some points need to be interpolated.
C
      IF (.NOT.(IPRJ.EQ.2.OR.IPRJ.EQ.4.OR.IPRJ.EQ.5.OR.IPRJ.EQ.6))
     +GO TO 10000
      CPLT=COS(DTOR*PHIA)
      SPLT=SIN(DTOR*PHIA)
      CPLN=COS(DTOR*PHIO)
      SPLN=SIN(DTOR*PHIO)
      CXLT=COS(DTOR*XLAT)
      SXLT=SIN(DTOR*XLAT)
      CXLN=COS(DTOR*XLON)
      SXLN=SIN(DTOR*XLON)
      APTX=2.*RTOD*ASIN(SQRT((CXLT*CXLN-CPLT*CPLN)**2+
     +                       (CXLT*SXLN-CPLT*SPLN)**2+
     +                       (SXLT     -SPLT     )**2)/2.)
      IF (.NOT.(IPRJ.EQ.2)) GO TO 10001
      IF (.NOT.(APTX.LE.179.999)) GO TO 10002
      RMUL=2./(1.+COS(DTOR*APTX))
      GO TO 10003
10002 CONTINUE
      RMUL=201.
10003 CONTINUE
      GO TO 10004
10001 CONTINUE
      IF (.NOT.(IPRJ.EQ.4)) GO TO 10005
      IF (.NOT.(APTX.LE.179.999)) GO TO 10006
      RMUL=2./SQRT(2.*(1.+COS(DTOR*APTX)))
      GO TO 10007
10006 CONTINUE
      RMUL=201.
10007 CONTINUE
      GO TO 10004
10005 CONTINUE
      IF (.NOT.(IPRJ.EQ.5)) GO TO 10008
      IF (.NOT.(APTX.LT.89.999)) GO TO 10009
      RMUL=1./COS(DTOR*APTX)
      GO TO 10010
10009 CONTINUE
      RMUL=201.
10010 CONTINUE
      GO TO 10004
10008 CONTINUE
      IF (.NOT.(IPRJ.EQ.6)) GO TO 10011
      IF (.NOT.(APTX.LE..001)) GO TO 10012
      RMUL=1.
      GO TO 10013
10012 CONTINUE
      IF (.NOT.(APTX.LT.179.999)) GO TO 10014
      RMUL=DTOR*APTX/SIN(DTOR*APTX)
      GO TO 10013
10014 CONTINUE
      RMUL=201.
10013 CONTINUE
10004 CONTINUE
10011 CONTINUE
      IF (.NOT.(IFST.NE.0)) GO TO 10015
      NOPI=MAX(0,MIN(100,(INT(MAX(RMLO,RMUL))-1)/2))
      IF (NOPI.NE.0)
     +          CALL MAPGCI (XLTO,XLNO,XLAT,XLON,NOPI,RLATI,RLONI)
10015 CONTINUE
      XLTO=XLAT
      XLNO=XLON
      RMLO=RMUL
10000 CONTINUE
C
C Return here for the next interpolated point.
C
  100 CONTINUE
      IF (.NOT.(IOPI.LT.NOPI)) GO TO 10016
      IOPI=IOPI+1
      RLAT=RLATI(IOPI)
      RLON=RLONI(IOPI)
      GO TO 10017
10016 CONTINUE
      IOPI=IOPI+1
      RLAT=XLAT
      RLON=XLON
10017 CONTINUE
C
C Project the point (RLAT,RLON) to (U,V).
C
      CALL MAPTRN (RLAT,RLON,U,V)
C
C For the sake of efficiency, execute one of two parallel algorithms,
C depending on whether an elliptical or a rectangular perimeter is in
C use.  (That way, we test ELPF only once.)
C
      IF (.NOT.(ELPF)) GO TO 10018
C
C Elliptical - assume the new point is visible until we find otherwise.
C
      IVIS=1
C
C See if the new point is invisible.
C
      IF (.NOT.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.GT.1.))
     +GO TO 10019
C
C The new point is invisible.  Reset the visibility flag.
C
      IVIS=0
C
C If the new point is a "first point" or if the last point was not
C visible, draw nothing.  The possible existence of a visible segment
C along the line joining two invisible points is intentionally ignored,
C for reasons of efficiency.  For this reason, objects should not be
C drawn using long line segments.
C
      IF (IFST.EQ.0.OR.IVSO.EQ.0) GO TO 108
C
C If the new point is invisible because its projection is undefined,
C use a binary-halving technique to find a visible point close to the
C edge and extend the line to it.
C
      IF (.NOT.(U.GE.1.E12)) GO TO 10020
      RLTV=RLTO
      RLNV=RLNO
      RLTI=RLAT
      RLNI=RLON
      UINT=UOLD
      VINT=VOLD
      ASSIGN 10021 TO L10022
      GO TO 10022
10021 CONTINUE
      XCRD=UINT
      YCRD=VINT
      ASSIGN 10023 TO L10024
      GO TO 10024
10023 CONTINUE
      GO TO 108
10020 CONTINUE
C
C Otherwise, the new point is not a "first point", the last point was
C visible, and the projection of the new point is defined, so we need
C to continue the line.  First, if there's a cross-over problem, move
C the new point to its alternate position.  This may make it visible.
C
      IF (.NOT.(ABS(P-POLD).GT.UEPS.OR.ABS(Q-QOLD).GT.VEPS)) GO TO 10025
C
      IF (.NOT.(JPRJ.GE.7)) GO TO 10026
      P=P-SIGN(CPRJ(JPRJ-6),P)
      U=P
      IF (JPRJ.EQ.9) U=U*SQRT(1.-V*V)
      GO TO 10027
10026 CONTINUE
      GO TO 108
10027 CONTINUE
C
      IF (.NOT.(((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +GO TO 10028
      IVIS=1
      GO TO 107
10028 CONTINUE
C
10025 CONTINUE
C
C If it's still invisible, interpolate to the edge of the frame, extend
C the line to that point, and quit.
C
      CALL MAPTRE (UOLD,VOLD,U,V,UINT,VINT)
      XCRD=UINT
      YCRD=VINT
      ASSIGN 10029 TO L10024
      GO TO 10024
10029 CONTINUE
      GO TO 108
C
10019 CONTINUE
C
C The new point is visible.  If it's the first point of a line, go start
C a new line.
C
      IF (IFST.EQ.0) GO TO 106
C
C If the last point was invisible because its projection was undefined,
C use binary halving to find a point in between and start the new line
C there.
C
      IF (.NOT.(UOLD.GE.1.E12)) GO TO 10030
      RLTV=RLAT
      RLNV=RLON
      RLTI=RLTO
      RLNI=RLNO
      UINT=U
      VINT=V
      ASSIGN 10031 TO L10022
      GO TO 10022
10031 CONTINUE
      XCRD=UINT
      YCRD=VINT
      ASSIGN 10032 TO L10033
      GO TO 10033
10032 CONTINUE
      XCRD=U
      YCRD=V
      ASSIGN 10034 TO L10024
      GO TO 10024
10034 CONTINUE
      GO TO 108
10030 CONTINUE
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
  101 IF (JPRJ.LT.7) GO TO 106
C
C If the old point was visible, generate the alternate projection of the
C new point and draw the visible portion of the line segment joining the
C old point to the alternate projection point.
C
      IF (.NOT.(IVSO.NE.0)) GO TO 10035
C
      UTMP=P-SIGN(CPRJ(JPRJ-6),P)
      VTMP=Q
      IF (JPRJ.EQ.9) UTMP=UTMP*SQRT(1.-VTMP*VTMP)
C
      IF (.NOT.(((UTMP-UCEN)/URNG)**2+((VTMP-VCEN)/VRNG)**2.GT.1.))
     +GO TO 10036
      CALL MAPTRE (UOLD,VOLD,UTMP,VTMP,UTMP,VTMP)
10036 CONTINUE
C
      XCRD=UTMP
      YCRD=VTMP
      ASSIGN 10037 TO L10024
      GO TO 10024
10037 CONTINUE
C
10035 CONTINUE
C
C Now generate an alternate projection of the old point close to the new
C one and draw the visible portion of the line segment joining it to the
C new point.
C
      UOLD=POLD-SIGN(CPRJ(JPRJ-6),POLD)
      IF (JPRJ.EQ.9) UOLD=UOLD*SQRT(1.-VOLD*VOLD)
C
      IF (((UOLD-UCEN)/URNG)**2+((VOLD-VCEN)/VRNG)**2.LE.1.)
     +                                                     GO TO 105
C
C Move (UOLD,VOLD) by interpolating to the edge of the frame.
C
  102 CALL MAPTRE (U,V,UOLD,VOLD,UOLD,VOLD)
C
      GO TO 10038
10018 CONTINUE
C
C Rectangular - repeat the above code, changing the tests for a point's
C being inside/outside the perimeter.  Commenting will be abbreviated.
C
      IVIS=1
C
      IF (.NOT.(U.LT.UMIN.OR.U.GT.UMAX.OR.V.LT.VMIN.OR.V.GT.VMAX))
     +GO TO 10039
C
      IVIS=0
C
      IF (IFST.EQ.0.OR.IVSO.EQ.0) GO TO 108
C
      IF (.NOT.(U.GE.1.E12)) GO TO 10040
      RLTV=RLTO
      RLNV=RLNO
      RLTI=RLAT
      RLNI=RLON
      UINT=UOLD
      VINT=VOLD
      ASSIGN 10041 TO L10022
      GO TO 10022
10041 CONTINUE
      XCRD=UINT
      YCRD=VINT
      ASSIGN 10042 TO L10024
      GO TO 10024
10042 CONTINUE
      GO TO 108
10040 CONTINUE
C
      IF (.NOT.(ABS(P-POLD).GT.UEPS.OR.ABS(Q-QOLD).GT.VEPS)) GO TO 10043
C
      IF (.NOT.(JPRJ.GE.7)) GO TO 10044
      P=P-SIGN(CPRJ(JPRJ-6),P)
      U=P
      IF (JPRJ.EQ.9) U=U*SQRT(1.-V*V)
      GO TO 10045
10044 CONTINUE
      GO TO 108
10045 CONTINUE
C
      IF (.NOT.(U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX))
     +GO TO 10046
      IVIS=1
      GO TO 107
10046 CONTINUE
10043 CONTINUE
C
      CALL MAPTRP (UOLD,VOLD,U,V,UINT,VINT)
      XCRD=UINT
      YCRD=VINT
      ASSIGN 10047 TO L10024
      GO TO 10024
10047 CONTINUE
      GO TO 108
C
10039 CONTINUE
C
      IF (IFST.EQ.0) GO TO 106
C
      IF (.NOT.(UOLD.GE.1.E12)) GO TO 10048
      RLTV=RLAT
      RLNV=RLON
      RLTI=RLTO
      RLNI=RLNO
      UINT=U
      VINT=V
      ASSIGN 10049 TO L10022
      GO TO 10022
10049 CONTINUE
      XCRD=UINT
      YCRD=VINT
      ASSIGN 10050 TO L10033
      GO TO 10033
10050 CONTINUE
      XCRD=U
      YCRD=V
      ASSIGN 10051 TO L10024
      GO TO 10024
10051 CONTINUE
      GO TO 108
10048 CONTINUE
C
      IF (ABS(P-POLD).GT.UEPS.OR.ABS(Q-QOLD).GT.VEPS) GO TO 103
C
      IF (IVSO.EQ.0) GO TO 104
C
      GO TO 107
C
  103 IF (JPRJ.LT.7) GO TO 106
C
      IF (.NOT.(IVSO.NE.0)) GO TO 10052
C
      UTMP=P-SIGN(CPRJ(JPRJ-6),P)
      VTMP=Q
      IF (JPRJ.EQ.9) UTMP=UTMP*SQRT(1.-VTMP*VTMP)
C
      IF (.NOT.(UTMP.LT.UMIN.OR.UTMP.GT.UMAX.OR.VTMP.LT.VMIN.OR.VTMP.GT.
     +VMAX)) GO TO 10053
      CALL MAPTRP (UOLD,VOLD,UTMP,VTMP,UTMP,VTMP)
10053 CONTINUE
C
      XCRD=UTMP
      YCRD=VTMP
      ASSIGN 10054 TO L10024
      GO TO 10024
10054 CONTINUE
10052 CONTINUE
C
      UOLD=POLD-SIGN(CPRJ(JPRJ-6),POLD)
      IF (JPRJ.EQ.9) UOLD=UOLD*SQRT(1.-VOLD*VOLD)
C
      IF (UOLD.GE.UMIN.AND.UOLD.LE.UMAX.AND.
     +    VOLD.GE.VMIN.AND.VOLD.LE.VMAX) GO TO 105
C
  104 CALL MAPTRP (U,V,UOLD,VOLD,UOLD,VOLD)
C
10038 CONTINUE
C
C Draw the visible portion of the line joining the old point to the new.
C
  105 XCRD=UOLD
      YCRD=VOLD
      ASSIGN 10055 TO L10033
      GO TO 10033
10055 CONTINUE
C
      XCRD=U
      YCRD=V
      ASSIGN 10056 TO L10024
      GO TO 10024
10056 CONTINUE
C
      GO TO 108
C
C Start a new line.
C
  106 XCRD=U
      YCRD=V
      ASSIGN 10057 TO L10033
      GO TO 10033
10057 CONTINUE
C
      GO TO 108
C
C Continue the line.  If the point is an interpolated one and it is too
C close to the last one, skip it.
C
  107 CONTINUE
      IF (.NOT.(IOPI.LE.NOPI)) GO TO 10058
      IF (ABS(U-UOLD).LT..001*(UMAX-UMIN).AND.
     +    ABS(V-VOLD).LT..001*(VMAX-VMIN)) GO TO 100
10058 CONTINUE
C
      XCRD=U
      YCRD=V
      ASSIGN 10059 TO L10024
      GO TO 10024
10059 CONTINUE
C
C Save information about the current point for the next call and quit.
C
  108 IVSO=IVIS
      POLD=P
      QOLD=Q
      UOLD=U
      VOLD=V
      RLTO=RLAT
      RLNO=RLON
C
C If interpolation is taking place, loop back for the next point.
C
      IF (IOPI.LE.NOPI) GO TO 100
C
      RETURN
C
C The following internal procedure, given a visible point and an
C invisible point, uses a binary-halving technique to find a point
C in between them, near the edge of visibility.
C
10022 CONTINUE
      PSAV=P
      QSAV=Q
      ITMP=0
10060 CONTINUE
      CALL MAPGCI (RLTV,RLNV,RLTI,RLNI,1,RLATI,RLONI)
      RLTH=RLATI(1)
      RLNH=RLONI(1)
      CALL MAPTRN (RLTH,RLNH,UINH,VINH)
      IF (.NOT.(UINH.LT.1.E12)) GO TO 10061
      RLTV=RLTH
      RLNV=RLNH
      UINT=UINH
      VINT=VINH
      GO TO 10062
10061 CONTINUE
      RLTI=RLTH
      RLNI=RLNH
10062 CONTINUE
      ITMP=ITMP+1
      IF (ITMP.GE.64.OR.(RLTV.EQ.RLTI.AND.RLNV.EQ.RLNI)) GO TO 10063
      GO TO 10060
10063 CONTINUE
      P=PSAV
      Q=QSAV
      GO TO L10022 , (10049,10041,10031,10021)
C
C The following internal procedure is invoked to start a line.
C
10033 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10064
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
10064 CONTINUE
      XCRA(1)=XCRD
      YCRA(1)=YCRD
      NCRA=1
      GO TO L10033 , (10057,10055,10050,10032)
C
C The following internal procedure is invoked to continue a line.
C
10024 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10065
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10065 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=XCRD
      YCRA(NCRA)=YCRD
      GO TO L10024 , (10059,10056,10054,10051,10047,10042,10037,10034,10
     +029,10023)
C
      END
