C
C $Id: ctplar.f,v 1.1 2003-05-28 15:44:33 kennison Exp $
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
      SUBROUTINE CTPLAR (RWRK,IPTX,IPTY,NXYC)
C
      DIMENSION RWRK(*)
C
C (RWRK(I),I=IPTX+1,IPTX+NXYC) and (RWRK(I),I=IPTY+1,IPTY+NXYC) contain
C the X and Y coordinates (in the user coordinate system) of points
C defining a portion of a contour line.  The function of the routine
C CTPLAR is to position one or more labels at regular intervals along
C that portion.
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
      COMMON /CTCOM1/ IRWS(4),IRWU,ISET,IWSO,JODP,JOMA,JOTZ
      COMMON /CTCOM1/ LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3,LINS
      COMMON /CTCOM1/ LINT(10),LINU,LIWK,LIWM,LIWS(2),LNLG,LOEN
      COMMON /CTCOM1/ LOPN,LOTN,LRWC,LRWG,LRWK,LRWM,LRWS(4)
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
        IF (ICFELL('CTPLAR',1).NE.0) RETURN
        RWRK(IPTY+I)=CUFY(RWRK(IPTY+I))
        IF (ICFELL('CTPLAR',2).NE.0) RETURN
10001 CONTINUE
C
C Initialize.  NLBI is the number of labels initially in the list, DATL
C is the distance along the line from the first point to the current
C point, and DANL is the desired distance to the next label.
C
      NLBI=NLBS
      DATL=0.
      DANL=DBLF+2.*(CTRANF()-.5)*DBLV
C
C Examine points along the contour line, putting labels at chosen ones.
C
      DO 10002 I=2,NXYC-1
C
C Wait until we have gone sufficiently far along the line.
C
        DATL=DATL+SQRT((RWRK(IPTX+I)-RWRK(IPTX+I-1))**2+
     +                 (RWRK(IPTY+I)-RWRK(IPTY+I-1))**2)
C
        IF (DATL.GE.DANL) THEN
C
C Consider a possible label centered at the point (XCLB,YCLB).
C
          XCLB=RWRK(IPTX+I)
          YCLB=RWRK(IPTY+I)
C
C Call a user routine which may change the label to be used; if the
C label string is blanked by that routine, don't put a label there.
C
          XLBC=CFUX(XCLB)
          IF (ICFELL('CTPLAR',3).NE.0) RETURN
          YLBC=CFUY(YCLB)
          IF (ICFELL('CTPLAR',4).NE.0) RETURN
C
          DVAL=CLEV(ICLV)
C
          LCTM=NCLB(ICLV)
          CTMA(1:LCTM)=CLBL(ICLV)(1:LCTM)
          CTMB(1:LCTM)=CTMA(1:LCTM)
C
          CALL HLUCTCHLL (+1)
          IF (ICFELL('CTPLAR',5).NE.0) RETURN
C
C If the label string was blanked by the user, don't put a label there.
C
          IF (CTMA(1:LCTM).EQ.' ') GO TO 101
C
C Set text extent variables; how this is done depends on whether the
C user changed the label string or not.
C
          IF (CTMA(1:LCTM).EQ.CTMB(1:LCTM)) THEN
            DSTL=CLDL(ICLV)+XTRA
            DSTR=CLDR(ICLV)+XTRA
            DSTB=CLDB(ICLV)+XTRA
            DSTT=CLDT(ICLV)+XTRA
          ELSE
            CALL PCGETI ('TE',ITMP)
            IF (ICFELL('CTPLAR',6).NE.0) RETURN
            CALL PCSETI ('TE',1)
            IF (ICFELL('CTPLAR',7).NE.0) RETURN
            CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,360.,0.)
            IF (ICFELL('CTPLAR',8).NE.0) RETURN
            CALL PCGETR ('DL',DTOL)
            IF (ICFELL('CTPLAR',9).NE.0) RETURN
            CALL PCGETR ('DR',DTOR)
            IF (ICFELL('CTPLAR',10).NE.0) RETURN
            CALL PCGETR ('DB',DTOB)
            IF (ICFELL('CTPLAR',11).NE.0) RETURN
            CALL PCGETR ('DT',DTOT)
            IF (ICFELL('CTPLAR',12).NE.0) RETURN
            CALL PCSETI ('TE',ITMP)
            IF (ICFELL('CTPLAR',13).NE.0) RETURN
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
     +        YBLB.LE.YVPB.OR.YTLB.GE.YVPT) GO TO 101
C
C If the label would overlap a previous label, forget it.
C
            ILBL = 1
            GO TO 10005
10003       CONTINUE
            ILBL =ILBL +1
10005       CONTINUE
            IF (ILBL .GT.(NLBS)) GO TO 10004
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
     +          YTLB.GE.YBOL.AND.YBLB.LE.YTOL) GO TO 101
C
          GO TO 10003
10004     CONTINUE
C
C No problem.  Go ahead and put a label at this point.
C
          NLBS=NLBS+1
          IF (4*NLBS.GT.LR03) THEN
            IS01=IR01
            CALL CTGRWS (RWRK,3,MAX(4*NLBS,LR03+100),IWSE)
            IPTX=IPTX-IS01+IR01
            IPTY=IPTY-IS01+IR01
            IF (IWSE.NE.0.OR.ICFELL('CTPLAR',14).NE.0) THEN
              NLBS=NLBS-1
              RETURN
            END IF
          END IF
          RWRK(IR03+4*(NLBS-1)+1)=RWRK(IPTX+I)
          RWRK(IR03+4*(NLBS-1)+2)=RWRK(IPTY+I)
          RWRK(IR03+4*(NLBS-1)+3)=ANLB
          IF (CTMA(1:LCTM).EQ.CTMB(1:LCTM)) THEN
            RWRK(IR03+4*(NLBS-1)+4)=REAL(ICLV)
          ELSE
            RWRK(IR03+4*(NLBS-1)+4)=-NR04
            NR04=NR04+6
            IF (NR04.GT.LR04) THEN
              IS01=IR01
              CALL CTGRWS (RWRK,4,MAX(NR04,LR04+100),IWSE)
              IPTX=IPTX-IS01+IR01
              IPTY=IPTY-IS01+IR01
              IF (IWSE.NE.0.OR.ICFELL('CTPLAR',15).NE.0) THEN
                NLBS=NLBS-1
                RETURN
              END IF
            END IF
            RWRK(IR04+NR04-5)=3.
            RWRK(IR04+NR04-4)=REAL(ICLV)
            RWRK(IR04+NR04-3)=DTOL
            RWRK(IR04+NR04-2)=DTOR
            RWRK(IR04+NR04-1)=DTOB
            RWRK(IR04+NR04  )=DTOT
          END IF
C
C Update the distance along the line to the next label.
C
          DANL=DBLF+REAL(NLBS-NLBI)*DBLN+2.*(CTRANF()-.5)*DBLV
C
        END IF
C
  101 CONTINUE
10002 CONTINUE
C
C Done.
C
      RETURN
C
      END
