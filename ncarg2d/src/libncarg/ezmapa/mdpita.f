C
C $Id: mdpita.f,v 1.1 2001-08-16 23:10:22 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE MDPITA (XLAT,XLON,IFST,IAMP,IGRP,IDLT,IDRT)
C
      DOUBLE PRECISION XLAT,XLON
      INTEGER          IFST,IAMP(*),IGRP,IDLT,IDRT
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
      DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOT,RTDD,RTOD,SIN1,TOPI
      SAVE   /MAPCM0/
C
      COMMON /MAPCM1/  COSO,COSR,PHOC,SINO,SINR,IPRJ,IROD
      DOUBLE PRECISION COSO,COSR,PHOC,SINO,SINR
      INTEGER          IPRJ,IROD
      SAVE   /MAPCM1/
C
      COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
      DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG
      INTEGER          ISSL
      SAVE   /MAPCM2/
C
      COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                 PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                 ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
      DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                 PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW
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
      COMMON /MAPCMC/  IGI1,IGI2,NCRA,NOVS,XCRA(100),YCRA(100)
      INTEGER          IGI1,IGI2,NCRA,NOVS
      REAL             XCRA,YCRA
      SAVE   /MAPCMC/
C
      COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
      DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
      INTEGER IPRF
      SAVE   /USGSC1/
C
C Declare local variables.
C
      DOUBLE PRECISION APTX,CPLN,CPLT,CXLN,CXLT,DTST,PAPE,PNEW,POLD,
     +                 RLAT,RLATI(100),RLNE,RLNO,RLON,RLONI(100),RLTE,
     +                 RLTO,RMLO,RMUL,SPLN,SPLT,SXLN,SXLT,UCOE,UNEW,
     +                 UOLD,VCOE,VNEW,VOLD,XCRD,XLNO,XLTO,YCRD
C
      INTEGER          IOPI,IVIS,IVSO,NOPI
C
      SAVE             IVSO,POLD,RLNO,RLTO,RMLO,UOLD,VOLD,XLNO,XLTO
C
      DATA IVSO,POLD,RLNO,RLTO,RMLO,UOLD,VOLD,XLNO,XLTO / 0,8*0.D0 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MDPITA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MDPINT
      IF (ICFELL('MDPITA',2).NE.0) RETURN
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
      CPLT=0.D0
      SPLT=1.D0
      CPLN=1.D0
      SPLN=0.D0
      IF (.NOT.(IPRF.EQ.8.AND.UTPA(9).EQ.0.D0)) GO TO 10004
      DTST=UTPA(3)
      GO TO 10005
10004 CONTINUE
      DTST=.5D0*UTPA(3)+UTPA(4)
10005 CONTINUE
      IF (.NOT.(DTST.GT.0.D0)) GO TO 10006
      DTST=90.D0-DTST
      GO TO 10007
10006 CONTINUE
      SPLT=-SPLT
      DTST=DTST+90.D0
10007 CONTINUE
      GO TO 10008
10003 CONTINUE
      CPLT=COS(DTOR*UTPA(6))
      SPLT=SIN(DTOR*UTPA(6))
      CPLN=COS(DTOR*UTPA(5))
      SPLN=SIN(DTOR*UTPA(5))
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
      APTX=2.D0*RTOD*ASIN(SQRT((CXLT*CXLN-CPLT*CPLN)**2+
     +                         (CXLT*SXLN-CPLT*SPLN)**2+
     +                         (SXLT     -SPLT     )**2)/2.D0)
      IF (.NOT.(IPRJ.EQ.0.AND.(IPRF.EQ.3.OR.IPRF.EQ.8))) GO TO 10010
      IF (.NOT.(APTX.LE.DTST)) GO TO 10011
      RMUL=MAX(1.D0,MIN(51.D0,51.D0*((DTST-APTX)/DTST)**8))
      GO TO 10012
10011 CONTINUE
      RMUL=MAX(1.D0,MIN(51.D0,51.D0*((APTX-DTST)/
     +                                  (180.D0-DTST))**8))
10012 CONTINUE
      GO TO 10013
10010 CONTINUE
      IF (.NOT.(IPRJ.EQ.2)) GO TO 10014
      IF (.NOT.(APTX.LE.179.999D0)) GO TO 10015
      RMUL=MIN(101.D0,1.D0/(1.D0+COS(DTOR*APTX)))
      GO TO 10016
10015 CONTINUE
      RMUL=101.D0
10016 CONTINUE
      GO TO 10013
10014 CONTINUE
      IF (.NOT.((IPRJ.EQ.0.AND.IPRF.EQ.11).OR.IPRJ.EQ.4)) GO TO 10017
      IF (.NOT.(APTX.LE.179.999D0)) GO TO 10018
      RMUL=MIN(101.D0,2.D0/SQRT(2.D0*(1.D0+COS(DTOR*APTX))))
      GO TO 10019
10018 CONTINUE
      RMUL=101.D0
10019 CONTINUE
      GO TO 10013
10017 CONTINUE
      IF (.NOT.(IPRJ.EQ.5)) GO TO 10020
      IF (.NOT.(APTX.LT.89.999D0)) GO TO 10021
      RMUL=MIN(21.D0,1.D0/COS(DTOR*APTX))
      GO TO 10022
10021 CONTINUE
      RMUL=21.D0
10022 CONTINUE
      GO TO 10013
10020 CONTINUE
      IF (.NOT.((IPRJ.EQ.0.AND.IPRF.EQ.12).OR.IPRJ.EQ.6)) GO TO 10023
      IF (.NOT.(APTX.LE..00001D0)) GO TO 10024
      RMUL=1.D0
      GO TO 10025
10024 CONTINUE
      IF (.NOT.(APTX.LT.179.999D0)) GO TO 10026
      RMUL=MIN(101.D0,DTOR*APTX/SIN(DTOR*APTX))
      GO TO 10025
10026 CONTINUE
      RMUL=101.D0
10025 CONTINUE
10013 CONTINUE
10023 CONTINUE
      IF (.NOT.(IFST.NE.0)) GO TO 10027
      NOPI=MAX(0,MIN(50,INT(MAX(RMLO,RMUL))/2))
      IF (NOPI.NE.0)
     +          CALL MDPGCI (XLTO,XLNO,XLAT,XLON,NOPI,RLATI,RLONI)
10027 CONTINUE
      XLTO=XLAT
      XLNO=XLON
      RMLO=RMUL
10001 CONTINUE
C
C If there is not enough room in the area map for all the saved points,
C plus all the interpolated points, plus a few more points, times a
C fudge factor, don't change anything - just return an overflow error
C to the caller.
C
      IF (.NOT.((NCRA+NOPI+5)*16.GE.IAMP(6)-IAMP(5)-1)) GO TO 10028
      CALL SETER ('MDPITA - AREA-MAP ARRAY OVERFLOW',3,1)
      RETURN
10028 CONTINUE
C
C Return here for the next interpolated point.
C
  101 CONTINUE
      IF (.NOT.(IOPI.LT.NOPI)) GO TO 10029
      IOPI=IOPI+1
      RLAT=RLATI(IOPI)
      RLON=RLONI(IOPI)
      GO TO 10030
10029 CONTINUE
      IOPI=IOPI+1
      RLAT=XLAT
      RLON=XLON
10030 CONTINUE
C
C Project the point (RLAT,RLON) to (UNEW,VNEW), using the routine
C MDPTRA, which returns 1.D12 for UNEW and VNEW in areas outside the
C perimeter and on the "wrong" side of a limb line.  Also save PNEW
C for "crossover" testing.
C
      CALL MDPTRA (RLAT,RLON,UNEW,VNEW)
      IF (ICFELL('MDPITA',4).NE.0) RETURN
      PNEW=P
C
C If the new point is invisible, we only have to draw something if it's
C not a first point and the last point was visible, in which case we
C interpolate to find a point at the edge of the visible area and then
C extend the line we're drawing to that point.  In any case, we jump to
C save information about the new point and get another.
C
      IF (.NOT.(UNEW.GE.1.D12)) GO TO 10031
      IVIS=0
      IF (.NOT.(IFST.NE.0.AND.IVSO.NE.0)) GO TO 10032
      CALL MDITVE (RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTE,RLNE,PAPE,UCOE,VCOE)
      IF (ICFELL('MDPITA',5).NE.0) RETURN
      XCRD=UCOE
      YCRD=VCOE
      L10034=    1
      GO TO 10034
10033 CONTINUE
10032 CONTINUE
      GO TO 103
10031 CONTINUE
C
C Otherwise, the new point is visible; things get more complicated.
C
      IVIS=1
C
C If the new point is a first point, initialize a new set of line draws,
C then jump to save information about the new point and get another.
C
      IF (.NOT.(IFST.EQ.0)) GO TO 10035
      XCRD=UNEW
      YCRD=VNEW
      L10037=    1
      GO TO 10037
10036 CONTINUE
      GO TO 103
10035 CONTINUE
C
C Otherwise, the new point is visible and it's not a first point; if the
C last point was invisible, find a point at the edge of the visible area
C and start a new set of line draws there.
C
      IF (.NOT.(IVSO.EQ.0)) GO TO 10038
      CALL MDITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLTO,RLNO,POLD,UOLD,VOLD)
      IF (ICFELL('MDPITA',6).NE.0) RETURN
      IVSO=1
      XCRD=UOLD
      YCRD=VOLD
      L10037=    2
      GO TO 10037
10039 CONTINUE
10038 CONTINUE
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
      CALL MDITVE (RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTE,RLNE,PAPE,UCOE,VCOE)
      IF (ICFELL('MDPITA',7).NE.0) RETURN
      XCRD=UCOE
      YCRD=VCOE
      L10034=    2
      GO TO 10034
10040 CONTINUE
C
      CALL MDITVE (RLAT,RLON,PNEW,UNEW,VNEW,
     +             RLTO,RLNO,POLD,UOLD,VOLD,
     +             RLTO,RLNO,POLD,UOLD,VOLD)
      IF (ICFELL('MDPITA',8).NE.0) RETURN
C
C Start a new series of line draws with the old point.
C
      IVSO=1
      XCRD=UOLD
      YCRD=VOLD
      L10037=    3
      GO TO 10037
10041 CONTINUE
C
C Continue the line to the new point.
C
  102 XCRD=UNEW
      YCRD=VNEW
      L10034=    3
      GO TO 10034
10042 CONTINUE
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
10037 CONTINUE
      IF (.NOT.(NCRA.GT.1)) GO TO 10043
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPITA',9).NE.0) RETURN
10043 CONTINUE
      XCRA(1)=REAL(XCRD)
      YCRA(1)=REAL(YCRD)
      NCRA=1
      GO TO (10036,10039,10041) , L10037
C
C The following internal procedure is invoked to continue a line.
C
10034 CONTINUE
      IF (.NOT.(NCRA.EQ.100)) GO TO 10044
      CALL AREDAM (IAMP,XCRA,YCRA,NCRA,IGRP,IDLT,IDRT)
      IF (ICFELL('MDPITA',10).NE.0) RETURN
      XCRA(1)=XCRA(100)
      YCRA(1)=YCRA(100)
      NCRA=1
10044 CONTINUE
      NCRA=NCRA+1
      XCRA(NCRA)=REAL(XCRD)
      YCRA(NCRA)=REAL(YCRD)
      GO TO (10033,10040,10042) , L10034
C
      END
