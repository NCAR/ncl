C
C $Id: ctplps.f,v 1.2 2004-03-19 22:51:57 kennison Exp $
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
      SUBROUTINE CTPLPS (RWRK,IPTX,IPTY,NXYC)
C
      DIMENSION RWRK(*)
C
C (RWRK(I),I=IPTX+1,IPTX+NXYC) and (RWRK(I),I=IPTY+1,IPTY+NXYC) contain
C the X and Y coordinates (in the user coordinate system) of points
C defining a portion of a contour line.  The function of the routine
C CTPLPS is to position one or more labels along that portion, using
C the "penalty scheme" of Starley Thompson and Phil Rasch.
C
C
C Declare all of the CONPACKT common blocks.
C
C
C CTCOM1 contains integer and real variables.
C
      COMMON /CTCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CTCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CTCOM1/ CLDT(256),CLEV(256),CLWA(258),CXCF
      COMMON /CTCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DMAX
      COMMON /CTCOM1/ DMIN,DOPT,DVAL,EPSI,FNCM,GRAV,GRSD,GSDM,HCHL
      COMMON /CTCOM1/ HCHS,HLSR,IAIA(258),IAIB(256),IBCF,IBHL
      COMMON /CTCOM1/ IBIL,IBLL,ICAF,ICCF,ICCL(258),ICFF,ICHI
      COMMON /CTCOM1/ ICHL,ICIL,ICLL(256),ICLO,ICLP(256),ICLS
      COMMON /CTCOM1/ ICLU(258),ICLV,ICLW,IDUF,IGCL,IGLB,IGRM
      COMMON /CTCOM1/ IGRN,IGVS,IHCF,IHLE,IIWS(2),IIWU,ILBC
      COMMON /CTCOM1/ IMPF,INCX(8),INCY(8),INHL,INIL,INIT,INLL
      COMMON /CTCOM1/ IOCF,IOHL,IOLL,IPAI,IPCF,IPIC,IPIE,IPIL,IPLL
      COMMON /CTCOM1/ IRWS(4),IRWU,ISET,ITBM,IWSO,JODP,JOMA
      COMMON /CTCOM1/ JOTZ,LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3,LINS
      COMMON /CTCOM1/ LINT(10),LINU,LIWB,LIWK,LIWM,LIWS(2),LNLG
      COMMON /CTCOM1/ LOEN,LOPN,LOTN,LRWC,LRWG,LRWK,LRWM,LRWS(4)
      COMMON /CTCOM1/ LSDD,LSDL,LSDM,LTCF,LTHI,LTIL,LTLO,MIRO
      COMMON /CTCOM1/ NCLB(256),NCLV,NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /CTCOM1/ NLBS,NLSD,NLZF,NOMF,NOVS,NPNT,NR04,NSDL
      COMMON /CTCOM1/ NSDR,NTRI,OORV,PITH,SCFS,SCFU,SEGL,T2DS
      COMMON /CTCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CTCOM1/ UWDR,UWDT,WCCF,WCHL,WCIL,WCLL,WLCF,WLHL,WLIL
      COMMON /CTCOM1/ WLLL,WOCH,WODA,WTCD,WTGR,WTNC,WTOD,WWCF,WWHL
      COMMON /CTCOM1/ WWIL,WWLL,XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /CTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CTCOM1/
C
C CTCOM2 holds character parameters.
C
      COMMON /CTCOM2/ CHEX,CLBL(256),CLDP(258),CTMA,CTMB,FRMT
      COMMON /CTCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CTCOM2/
C
C If there are fewer than three points, skip it.
C
      IF (NXYC.LT.3) RETURN
C
C Set up some needed constants.
C
      DBLS=DBLM*(XVPR-XVPL)*DBLM*(XVPR-XVPL)
C
C Save the current count of labels generated.
C
      NLBI=NLBS
C
C If it is possible to do it and if the value will be needed, estimate
C the contour interval at the current level.
C
      IF (NCLV.LE.1.OR.WTNC.LE.0.) THEN
        ESCI=0.
      ELSE
        IF (CINU.NE.0.) THEN
          ESCI=CINU
        ELSE
          IF (ICLW.EQ.1) THEN
            ESCI=ABS(CLEV(ICLP(2))-CLEV(ICLP(1)))
          ELSE IF (ICLW.EQ.NCLV) THEN
            ESCI=ABS(CLEV(ICLP(NCLV))-CLEV(ICLP(NCLV-1)))
          ELSE
            ESCI=.5*ABS(CLEV(ICLP(ICLW+1))-CLEV(ICLP(ICLW-1)))
          END IF
        END IF
      END IF
C
C Compute character-size and white-space-size variables.
C
      WCFS=CHWM*WCLL*(XVPR-XVPL)
      WWFS=CHWM*WWLL*(XVPR-XVPL)
C
      XTRA=.5*WCFS
C
C Convert all the coordinates from the user system to the fractional
C system.
C
      DO 10001 I=1,NXYC
        RWRK(IPTX+I)=CUFX(RWRK(IPTX+I))
        IF (ICFELL('CTPLPS',1).NE.0) RETURN
        RWRK(IPTY+I)=CUFY(RWRK(IPTY+I))
        IF (ICFELL('CTPLPS',2).NE.0) RETURN
10001 CONTINUE
C
C Cull points that are too close to one another.
C
      NXYT=1
C
10002 CONTINUE
        NXYT=NXYT+1
        IF (NXYT.GT.NXYC) GO TO 10003
        IF (ABS(RWRK(IPTX+NXYT)-RWRK(IPTX+NXYT-1)).LT.EPSI.AND.ABS(RWRK(
     +IPTY+NXYT)-RWRK(IPTY+NXYT-1)).LT.EPSI) THEN
          IF (NXYT.NE.NXYC) THEN
            DO 10004 I=NXYT+1,NXYC
              RWRK(IPTX+I-1)=RWRK(IPTX+I)
              RWRK(IPTY+I-1)=RWRK(IPTY+I)
10004       CONTINUE
          ELSE
            RWRK(IPTX+NXYC-1)=RWRK(IPTX+NXYC)
            RWRK(IPTY+NXYC-1)=RWRK(IPTY+NXYC)
          END IF
          NXYT=NXYT-1
          NXYC=NXYC-1
        END IF
      GO TO 10002
10003 CONTINUE
C
C If there are fewer than three points left, skip it.
C
      IF (NXYC.LT.3) RETURN
C
C Examine each point along the curve, looking for the point at which
C the penalty function exists and is minimal.  Put a label there and
C repeat until no more such points are found.
C
10005 CONTINUE
C
C IVMN will hold the index of the point at which the penalty function
C is minimized and PVMN will hold the value of the penalty function
C there.  Give them initial values which indicate nothing found so far.
C
        IVMN=0
        PVMN=0.
C
C Loop through the points on the line.
C
        DO 10006 I=1,NXYC
C
C Consider a possible label centered at the point (XCLB,YCLB).
C
          XCLB=RWRK(IPTX+I)
          YCLB=RWRK(IPTY+I)
C
C If the center point is too close to the center point of a label
C already put on this line, forget it.
C
            ILBL = NLBI+1
            GO TO 10009
10007       CONTINUE
            ILBL =ILBL +1
10009       CONTINUE
            IF (ILBL .GT.(NLBS)) GO TO 10008
            IF ((XCLB-RWRK(IR03+4*(ILBL-1)+1))**2+
     +          (YCLB-RWRK(IR03+4*(ILBL-1)+2))**2.LE.DBLS) GO TO 102
          GO TO 10007
10008     CONTINUE
C
C Call a user routine which may change the label to be used; if the
C label string is blanked by that routine, don't put a label there.
C
          XLBC=CFUX(XCLB)
          IF (ICFELL('CTPLPS',3).NE.0) RETURN
          YLBC=CFUY(YCLB)
          IF (ICFELL('CTPLPS',4).NE.0) RETURN
C
          DVAL=CLEV(ICLV)
C
          LCTM=NCLB(ICLV)
          CTMA(1:LCTM)=CLBL(ICLV)(1:LCTM)
          CTMB(1:LCTM)=CTMA(1:LCTM)
C
          CALL HLUCTCHLL (+1)
          IF (ICFELL('CTPLPS',5).NE.0) RETURN
C
C If the label string was blanked by the user, don't put a label there.
C
          IF (CTMA(1:LCTM).EQ.' ') GO TO 102
C
C Set text extent variables; how this is done depends on whether the
C user changed the label string or not.
C
          IF (CTMA(1:LCTM).EQ.CTMB(1:LCTM)) THEN
            ICHF=0
            DSTB=CLDB(ICLV)+XTRA
            DSTL=CLDL(ICLV)+XTRA
            DSTR=CLDR(ICLV)+XTRA
            DSTT=CLDT(ICLV)+XTRA
          ELSE
            ICHF=1
            CALL PCGETI ('TE',ITMP)
            IF (ICFELL('CTPLPS',6).NE.0) RETURN
            CALL PCSETI ('TE',1)
            IF (ICFELL('CTPLPS',7).NE.0) RETURN
            CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,360.,0.)
            IF (ICFELL('CTPLPS',8).NE.0) RETURN
            CALL PCGETR ('DL',DTOL)
            IF (ICFELL('CTPLPS',9).NE.0) RETURN
            CALL PCGETR ('DR',DTOR)
            IF (ICFELL('CTPLPS',10).NE.0) RETURN
            CALL PCGETR ('DB',DTOB)
            IF (ICFELL('CTPLPS',11).NE.0) RETURN
            CALL PCGETR ('DT',DTOT)
            IF (ICFELL('CTPLPS',12).NE.0) RETURN
            CALL PCSETI ('TE',ITMP)
            IF (ICFELL('CTPLPS',13).NE.0) RETURN
            DTOL=DTOL+WWFS
            DTOR=DTOR+WWFS
            DTOB=DTOB+WWFS
            DTOT=DTOT+WWFS
            DSTL=DTOL+XTRA
            DSTR=DTOR+XTRA
            DSTB=DTOB+XTRA
            DSTT=DTOT+XTRA
          END IF
C
          WLBL=DSTL+DSTR
          HLBL=DSTB+DSTT
          CRAD=MAX(DSTB,DSTL,DSTR,DSTT)
C
C Determine at what angle the label would be written and compute the
C coordinates of the left, right, bottom, and top edges of it.
C
          IF (IOLL.EQ.0) THEN
            ANLB=.017453292519943*ANLL
          ELSE
            IF (I.EQ.1) THEN
              ANLB=ATAN2(RWRK(IPTY+2)-YCLB,RWRK(IPTX+2)-XCLB)
            ELSE IF (I.EQ.NXYC) THEN
              ANLB=ATAN2(YCLB-RWRK(IPTY+NXYC-1),
     +                   XCLB-RWRK(IPTX+NXYC-1))
            ELSE
              ANLB=.5*(ATAN2(YCLB-RWRK(IPTY+I-1),XCLB-RWRK(IPTX+I-1))+
     +                 ATAN2(RWRK(IPTY+I+1)-YCLB,RWRK(IPTX+I+1)-XCLB))
            END IF
            IF (ANLB.LT.-1.57079632679490) ANLB=ANLB+3.14159265358979
            IF (ANLB.GT.+1.57079632679490) ANLB=ANLB-3.14159265358979
          END IF
C
          IF (ANLB.EQ.0.) THEN
            XLLB=XCLB-DSTL
            XRLB=XCLB+DSTR
            YBLB=YCLB-DSTB
            YTLB=YCLB+DSTT
          ELSE
            XLBL=XCLB-DSTL*COS(ANLB)+DSTB*SIN(ANLB)
            XRBL=XCLB+DSTR*COS(ANLB)+DSTB*SIN(ANLB)
            XRTL=XCLB+DSTR*COS(ANLB)-DSTT*SIN(ANLB)
            XLTL=XCLB-DSTL*COS(ANLB)-DSTT*SIN(ANLB)
            YLBL=YCLB-DSTL*SIN(ANLB)-DSTB*COS(ANLB)
            YRBL=YCLB+DSTR*SIN(ANLB)-DSTB*COS(ANLB)
            YRTL=YCLB+DSTR*SIN(ANLB)+DSTT*COS(ANLB)
            YLTL=YCLB-DSTL*SIN(ANLB)+DSTT*COS(ANLB)
            XLLB=MIN(XLBL,XRBL,XRTL,XLTL)
            XRLB=MAX(XLBL,XRBL,XRTL,XLTL)
            YBLB=MIN(YLBL,YRBL,YRTL,YLTL)
            YTLB=MAX(YLBL,YRBL,YRTL,YLTL)
          END IF
C
C If the label would extend outside the viewport, forget it.
C
          IF (XLLB.LE.XVPL.OR.XRLB.GE.XVPR.OR.
     +        YBLB.LE.YVPB.OR.YTLB.GE.YVPT) GO TO 102
C
C If the label would overlap a previous label, forget it.
C
            ILBL = 1
            GO TO 10012
10010       CONTINUE
            ILBL =ILBL +1
10012       CONTINUE
            IF (ILBL .GT.(NLBI)) GO TO 10011
C
            IF (ILBL.EQ.INIL) ETRA=.5*CHWM*WCIL*(XVPR-XVPL)
            IF (ILBL.EQ.INHL) ETRA=.5*CHWM*WCHL*(XVPR-XVPL)
            IF (ILBL.EQ.INLL) ETRA=.5*CHWM*WCLL*(XVPR-XVPL)
            XCOL=RWRK(IR03+4*(ILBL-1)+1)
            YCOL=RWRK(IR03+4*(ILBL-1)+2)
            ANOL=RWRK(IR03+4*(ILBL-1)+3)
            SAOL=SIN(ANOL)
            CAOL=COS(ANOL)
            ICOL=INT(RWRK(IR03+4*(ILBL-1)+4))
            IF (ICOL.LE.0) THEN
              ODSL=RWRK(IR04-ICOL+3)+ETRA
              ODSR=RWRK(IR04-ICOL+4)+ETRA
              ODSB=RWRK(IR04-ICOL+5)+ETRA
              ODST=RWRK(IR04-ICOL+6)+ETRA
            ELSE
              ODSL=CLDL(ICOL)+ETRA
              ODSR=CLDR(ICOL)+ETRA
              ODSB=CLDB(ICOL)+ETRA
              ODST=CLDT(ICOL)+ETRA
            END IF
C
            IF (ANOL.EQ.0.) THEN
              XLOL=XCOL-ODSL
              XROL=XCOL+ODSR
              YBOL=YCOL-ODSB
              YTOL=YCOL+ODST
            ELSE
              XLBO=XCOL-ODSL*CAOL+ODSB*SAOL
              XRBO=XCOL+ODSR*CAOL+ODSB*SAOL
              XRTO=XCOL+ODSR*CAOL-ODST*SAOL
              XLTO=XCOL-ODSL*CAOL-ODST*SAOL
              YLBO=YCOL-ODSL*SAOL-ODSB*CAOL
              YRBO=YCOL+ODSR*SAOL-ODSB*CAOL
              YRTO=YCOL+ODSR*SAOL+ODST*CAOL
              YLTO=YCOL-ODSL*SAOL+ODST*CAOL
              XLOL=MIN(XLBO,XRBO,XRTO,XLTO)
              XROL=MAX(XLBO,XRBO,XRTO,XLTO)
              YBOL=MIN(YLBO,YRBO,YRTO,YLTO)
              YTOL=MAX(YLBO,YRBO,YRTO,YLTO)
            END IF
C
            IF (XRLB.GE.XLOL.AND.XLLB.LE.XROL.AND.
     +          YTLB.GE.YBOL.AND.YBLB.LE.YTOL) GO TO 102
C
          GO TO 10010
10011     CONTINUE
C
C Compute the value of the penalty function at this point.  Initialize
C to zero.
C
          PNAL=0.
C
C If it will be needed below, estimate the gradient.  If the gradient
C does not exist (which can happen in special-value regions), skip the
C point.
C
          IF (WTGR.GT.0..OR.ESCI.NE.0.) THEN
            IGIN=MAX(1,MIN(IGRM,1+INT((XCLB-XVPL)/(XVPR-XVPL)*
     +                                                   REAL(IGRM))))
            JGIN=MAX(1,MIN(IGRN,1+INT((YCLB-YVPB)/(YVPT-YVPB)*
     +                                                   REAL(IGRN))))
            GRAD=RWRK(IR02+(JGIN-1)*IGRM+IGIN)
            IF (GRAD.LT.0.) GO TO 102
          END IF
C
C Penalize if the point is in a high-gradient region relative to the
C average gradient.  If the gradient exceeds user-specified tolerances,
C skip the point completely.
C
          IF (WTGR.GT.0.) THEN
            IF (GRAD.GT.GRAV+GSDM*GRSD) GO TO 102
            PNAL=PNAL+WTGR*GRAD/(GRAV+GSDM*GRSD)
          END IF
C
C Penalize if the number of contour lines crossing the label is too
C large.  If the number is greater than a user-specified value, skip
C the point entirely.
C
          IF (ESCI.NE.0.) THEN
            IF (I.EQ.1) THEN
              ANCL=ATAN2(RWRK(IPTY+2)-YCLB,RWRK(IPTX+2)-XCLB)
            ELSE IF (I.EQ.NXYC) THEN
              ANCL=ATAN2(YCLB-RWRK(IPTY+NXYC-1),
     +                   XCLB-RWRK(IPTX+NXYC-1))
            ELSE
              ANCL=.5*(ATAN2(YCLB-RWRK(IPTY+I-1),XCLB-RWRK(IPTX+I-1))+
     +                 ATAN2(RWRK(IPTY+I+1)-YCLB,RWRK(IPTX+I+1)-XCLB))
            END IF
            FNCL=(WLBL*ABS(SIN(ANLB-ANCL))+HLBL*ABS(COS(ANLB-ANCL)))/
     +           (ESCI/GRAD)
            IF (FNCL.GT.FNCM) GO TO 102
            PNAL=PNAL+WTNC*FNCL/FNCM
          END IF
C
C Penalize if the point is in a curvy part of the line.  Curviness is
C estimated by looking at all the points on the line within a radius
C CRAD and adding up all the changes in direction at those points.
C
          IF (WTCD.GT.0.) THEN
C
            CDIR=0.
C
            J=I
10013       CONTINUE
              K=J
              IF ((RWRK(IPTX+K)-XCLB)**2+(RWRK(IPTY+K)-YCLB)**2.GT.CRAD*
     +*2)     GO TO 10014
              IF (K.NE.1) THEN
                J=K-1
              ELSE
                IF (ABS(RWRK(IPTX+NXYC)-RWRK(IPTX+1)).GT..0001.OR.ABS(RW
     +RK(IPTY+NXYC)-RWRK(IPTY+1)).GT..0001) THEN
                  J=0
                  GO TO 10014
                END IF
                IF (I.EQ.NXYC) THEN
                  J=I
                  GO TO 10014
                END IF
                J=NXYC-1
              END IF
              IF (K.NE.NXYC) THEN
                L=K+1
              ELSE
                IF (ABS(RWRK(IPTX+NXYC)-RWRK(IPTX+1)).GT..0001.OR.
     +              ABS(RWRK(IPTY+NXYC)-RWRK(IPTY+1)).GT..0001)
     +              GO TO 101
                L=2
              END IF
              CDAP=57.2957795130823*
     +                           ABS(ATAN2(RWRK(IPTY+L)-RWRK(IPTY+K),
     +                                     RWRK(IPTX+L)-RWRK(IPTX+K))-
     +                               ATAN2(RWRK(IPTY+K)-RWRK(IPTY+J),
     +                                     RWRK(IPTX+K)-RWRK(IPTX+J)))
              IF (CDAP.GT.180.) CDAP=ABS(CDAP-360.)
              CDIR=CDIR+CDAP
  101       CONTINUE
              IF (J.EQ.I) GO TO 10014
            GO TO 10013
10014       CONTINUE
C
            IF (J.NE.I) THEN
              L=I
10015         CONTINUE
                K=L
                IF ((RWRK(IPTX+K)-XCLB)**2+(RWRK(IPTY+K)-YCLB)**2.GT.CRA
     +D**2)     GO TO 10016
                J=K-1
                IF (K.NE.NXYC) THEN
                  L=K+1
                ELSE
                  IF (ABS(RWRK(IPTX+NXYC)-RWRK(IPTX+1)).GT..0001.OR.ABS(
     +RWRK(IPTY+NXYC)-RWRK(IPTY+1)).GT..0001) GO TO 10016
                  IF ((RWRK(IPTX+1)-XCLB)**2+(RWRK(IPTY+1)-YCLB)**2.GT.C
     +RAD**2)     GO TO 10016
                  L=2
                END IF
                IF (K.NE.I) THEN
                  CDAP=57.2957795130823*
     +                           ABS(ATAN2(RWRK(IPTY+L)-RWRK(IPTY+K),
     +                                     RWRK(IPTX+L)-RWRK(IPTX+K))-
     +                               ATAN2(RWRK(IPTY+K)-RWRK(IPTY+J),
     +                                     RWRK(IPTX+K)-RWRK(IPTX+J)))
                  IF (CDAP.GT.180.) CDAP=ABS(CDAP-360.)
                  CDIR=CDIR+CDAP
                END IF
              GO TO 10015
10016         CONTINUE
            END IF
C
            IF (CDIR.GT.CDMX) GO TO 102
C
            PNAL=PNAL+WTCD*CDIR/CDMX
C
          END IF
C
C Penalize for being at other than the optimum distance from a label on
C contour lines previously considered.
C
          IF (WTOD.GT.0.) THEN
C
            POPD=1.
C
              ILBL = INLL
              GO TO 10019
10017         CONTINUE
              ILBL =ILBL +1
10019         CONTINUE
              IF (ILBL .GT.(NLBI)) GO TO 10018
              IF (INT(RWRK(IR03+4*(ILBL-1)+4)).NE.ICLV) THEN
                DIST=SQRT((XCLB-RWRK(IR03+4*(ILBL-1)+1))**2+
     +                    (YCLB-RWRK(IR03+4*(ILBL-1)+2))**2)
                POPD=MIN(POPD,1.-EXP(-((DIST-DOPT*(XVPR-XVPL))
     +                                       /(DFLD*(XVPR-XVPL)))**2))
              END IF
            GO TO 10017
10018       CONTINUE
C
            PNAL=PNAL+WTOD*POPD
C
          END IF
C
C If the value of the penalty function at this point is less than the
C previous minimum value found, update the information about the
C minimum.
C
          IF (IVMN.EQ.0.OR.PNAL.LT.PVMN) THEN
            IVMN=I
            PVMN=PNAL
            XVMN=XCLB
            YVMN=YCLB
            AVMN=ANLB
            ISCF=ICHF
            SDTL=DTOL
            SDTR=DTOR
            SDTB=DTOB
            SDTT=DTOT
          END IF
C
  102   CONTINUE
10006   CONTINUE
C
        IF (IVMN.NE.0) THEN
          NLBS=NLBS+1
          IF (4*NLBS.GT.LR03) THEN
            IS01=IR01
            CALL CTGRWS (RWRK,3,MAX(4*NLBS,LR03+100),IWSE)
            IPTX=IPTX-IS01+IR01
            IPTY=IPTY-IS01+IR01
            IF (IWSE.NE.0.OR.ICFELL('CTPLPS',14).NE.0) THEN
              NLBS=NLBS-1
              RETURN
            END IF
          END IF
          RWRK(IR03+4*(NLBS-1)+1)=XVMN
          RWRK(IR03+4*(NLBS-1)+2)=YVMN
          RWRK(IR03+4*(NLBS-1)+3)=AVMN
          IF (ISCF.EQ.0) THEN
            RWRK(IR03+4*(NLBS-1)+4)=REAL(ICLV)
          ELSE
            RWRK(IR03+4*(NLBS-1)+4)=-NR04
            NR04=NR04+6
            IF (NR04.GT.LR04) THEN
              IS01=IR01
              CALL CTGRWS (RWRK,4,MAX(NR04,LR04+100),IWSE)
              IPTX=IPTX-IS01+IR01
              IPTY=IPTY-IS01+IR01
              IF (IWSE.NE.0.OR.ICFELL('CTPLPS',15).NE.0) THEN
                NLBS=NLBS-1
                RETURN
              END IF
            END IF
            RWRK(IR04+NR04-5)=4.
            RWRK(IR04+NR04-4)=REAL(ICLV)
            RWRK(IR04+NR04-3)=SDTL
            RWRK(IR04+NR04-2)=SDTR
            RWRK(IR04+NR04-1)=SDTB
            RWRK(IR04+NR04  )=SDTT
          END IF
        END IF
C
      IF (.NOT.(IVMN.EQ.0)) GO TO 10005
C
C Done.
C
      RETURN
C
      END
