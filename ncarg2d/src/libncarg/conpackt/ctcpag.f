C
C $Id: ctcpag.f,v 1.2 2004-03-19 22:51:54 kennison Exp $
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
      SUBROUTINE CTCPAG (RPNT,IEDG,ITRI,RWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*)
C
C Given an array of data to be contoured, the routine CTCPAG computes
C an array of IGRM*IGRN gradients to be used in positioning labels.
C
C RPNT is an array of nodes defining vertices of triangles.
C
C IEDG is an array of nodes defining edges of triangles.
C
C ITRI is an array of nodes defining triangles.
C
C RWRK is the user's real workspace array.
C
C This routine also computes values for the quantities NGRV, which is
C the number of gradient values computed, GRAV, which is the average
C gradient found, and GRSD, which is the standard deviation of the
C gradient distribution.
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
C Compute some required tolerance values.
C
      TOL1=.0001*MIN(ABS(XVPR-XVPL),ABS(YVPT-YVPB))
      TOL2=.5000*MIN(ABS(XVPR-XVPL),ABS(YVPT-YVPB))
C
C Initialize the gradient array.
C
      DO 10001 I=1,IGRM*IGRN
        RWRK(IR02+I)=-1.
10001 CONTINUE
C
C Examine each triangle of the triangular mesh in turn.
C
      DO 101 I=0,NTRI-LOTN,LOTN
C
C Use only unblocked triangles.
C
      IF (IAND(ITRI(I+4),ITBM).NE.0) GO TO 101
C
C Find the base index of point 1 (that edges 1 and 2 have in common).
C
      IF (IEDG(ITRI(I+1)+1).EQ.IEDG(ITRI(I+2)+1).OR.IEDG(ITRI(I+1)+1).EQ
     +.IEDG(ITRI(I+2)+2)) THEN
        IPP1=IEDG(ITRI(I+1)+1)
      ELSE
        IPP1=IEDG(ITRI(I+1)+2)
      END IF
C
C Project point 1; if it's invisible, skip the triangle.
C
      IF (IMPF.EQ.0) THEN
        XCD1=RPNT(IPP1+1)
        YCD1=RPNT(IPP1+2)
      ELSE
        CALL HLUCTMXYZ (IMPF,RPNT(IPP1+1),RPNT(IPP1+2),RPNT(IPP1+3),
     +                                                    XCD1,YCD1)
        IF (ICFELL('CTCPAG',1).NE.0) RETURN
        IF ((OORV.NE.0.).AND.(XCP1.EQ.OORV.OR.YCP1.EQ.OORV))
     +                                                     GO TO 101
      END IF
C
C Find the base index of point 2 (that edges 2 and 3 have in common).
C
      IF (IEDG(ITRI(I+2)+1).EQ.IEDG(ITRI(I+3)+1).OR.IEDG(ITRI(I+2)+1).EQ
     +.IEDG(ITRI(I+3)+2)) THEN
        IPP2=IEDG(ITRI(I+2)+1)
      ELSE
        IPP2=IEDG(ITRI(I+2)+2)
      END IF
C
C Project point 2; if it's invisible, skip the triangle.
C
      IF (IMPF.EQ.0) THEN
        XCD2=RPNT(IPP2+1)
        YCD2=RPNT(IPP2+2)
      ELSE
        CALL HLUCTMXYZ (IMPF,RPNT(IPP2+1),RPNT(IPP2+2),RPNT(IPP2+3),
     +                                                    XCD2,YCD2)
        IF (ICFELL('CTCPAG',2).NE.0) RETURN
        IF ((OORV.NE.0.).AND.(XCP2.EQ.OORV.OR.YCP2.EQ.OORV))
     +                                                     GO TO 101
      END IF
C
C Find the base index of point 3 (that edges 3 and 1 have in common).
C
      IF (IEDG(ITRI(I+3)+1).EQ.IEDG(ITRI(I+1)+1).OR.IEDG(ITRI(I+3)+1).EQ
     +.IEDG(ITRI(I+1)+2)) THEN
        IPP3=IEDG(ITRI(I+3)+1)
      ELSE
        IPP3=IEDG(ITRI(I+3)+2)
      END IF
C
C Project point 3; if it's invisible, skip the triangle.
C
      IF (IMPF.EQ.0) THEN
        XCD3=RPNT(IPP3+1)
        YCD3=RPNT(IPP3+2)
      ELSE
        CALL HLUCTMXYZ (IMPF,RPNT(IPP3+1),RPNT(IPP3+2),RPNT(IPP3+3),
     +                                                    XCD3,YCD3)
        IF (ICFELL('CTCPAG',3).NE.0) RETURN
        IF ((OORV.NE.0.).AND.(XCP3.EQ.OORV.OR.YCP3.EQ.OORV))
     +                                                     GO TO 101
      END IF
C
C Find the fractional coordinates of all three points.
C
      XGD1=CUFX(XCD1)
      IF (ICFELL('CTCPAG',4).NE.0) RETURN
      YGD1=CUFY(YCD1)
      IF (ICFELL('CTCPAG',5).NE.0) RETURN
C
      XGD2=CUFX(XCD2)
      IF (ICFELL('CTCPAG',6).NE.0) RETURN
      YGD2=CUFY(YCD2)
      IF (ICFELL('CTCPAG',7).NE.0) RETURN
      ZGD2=ZCD2
C
      XGD3=CUFX(XCD3)
      IF (ICFELL('CTCPAG',8).NE.0) RETURN
      YGD3=CUFY(YCD3)
      IF (ICFELL('CTCPAG',9).NE.0) RETURN
      ZGD3=ZCD3
C
C Compute X and Y coordinate differences.
C
      XD12=XGD2-XGD1
      YD12=YGD2-YGD1
      XD23=XGD3-XGD2
      YD23=YGD3-YGD2
      XD31=XGD1-XGD3
      YD31=YGD1-YGD3
C
C If two points of the triangle are too close to each other, skip it.
C
      IF (ABS(XD12).LT.TOL1.AND.ABS(YD12).LT.TOL1) GO TO 101
      IF (ABS(XD23).LT.TOL1.AND.ABS(YD23).LT.TOL1) GO TO 101
      IF (ABS(XD31).LT.TOL1.AND.ABS(YD31).LT.TOL1) GO TO 101
C
C If two points of the triangle are too far apart, skip it.
C
      IF (ABS(XD12).GT.TOL2.OR.ABS(YD12).GT.TOL2) GO TO 101
      IF (ABS(XD23).GT.TOL2.OR.ABS(YD23).GT.TOL2) GO TO 101
      IF (ABS(XD31).GT.TOL2.OR.ABS(YD31).GT.TOL2) GO TO 101
C
C Pick up the field values at the three points.
C
      ZGD1=RPNT(IPP1+4)
      ZGD2=RPNT(IPP2+4)
      ZGD3=RPNT(IPP3+4)
C
C Compute the gradient of the triangle and use that to update values
C in the gradient array.
C
      IF (ZGD1.LT.ZGD2) THEN
        IF (ZGD2.LT.ZGD3) THEN
          XGDA=XGD1
          YGDA=YGD1
          ZGDA=ZGD1
          XGDB=XGD2
          YGDB=YGD2
          ZGDB=ZGD2
          XGDC=XGD3
          YGDC=YGD3
          ZGDC=ZGD3
        ELSE
          IF (ZGD1.LT.ZGD3) THEN
            XGDA=XGD1
            YGDA=YGD1
            ZGDA=ZGD1
            XGDB=XGD3
            YGDB=YGD3
            ZGDB=ZGD3
            XGDC=XGD2
            YGDC=YGD2
            ZGDC=ZGD2
          ELSE
            XGDA=XGD3
            YGDA=YGD3
            ZGDA=ZGD3
            XGDB=XGD1
            YGDB=YGD1
            ZGDB=ZGD1
            XGDC=XGD2
            YGDC=YGD2
            ZGDC=ZGD2
          END IF
        END IF
      ELSE
        IF (ZGD1.LT.ZGD3) THEN
          XGDA=XGD2
          YGDA=YGD2
          ZGDA=ZGD2
          XGDB=XGD1
          YGDB=YGD1
          ZGDB=ZGD1
          XGDC=XGD3
          YGDC=YGD3
          ZGDC=ZGD3
        ELSE
          IF (ZGD2.LT.ZGD3) THEN
            XGDA=XGD2
            YGDA=YGD2
            ZGDA=ZGD2
            XGDB=XGD3
            YGDB=YGD3
            ZGDB=ZGD3
            XGDC=XGD1
            YGDC=YGD1
            ZGDC=ZGD1
          ELSE
            XGDA=XGD3
            YGDA=YGD3
            ZGDA=ZGD3
            XGDB=XGD2
            YGDB=YGD2
            ZGDB=ZGD2
            XGDC=XGD1
            YGDC=YGD1
            ZGDC=ZGD1
          END IF
        END IF
      END IF
      DNOM=(XGDC-XGDB)*YGDA+(XGDA-XGDC)*YGDB+(XGDB-XGDA)*YGDC
      IF (DNOM.NE.0.) THEN
        IF (ZGDC-ZGDA.NE.0.) THEN
          COFA=((YGDB-YGDC)*ZGDA+(YGDC-YGDA)*ZGDB+
     +                                    (YGDA-YGDB)*ZGDC)/DNOM
          COFB=((XGDC-XGDB)*ZGDA+(XGDA-XGDC)*ZGDB+
     +                                    (XGDB-XGDA)*ZGDC)/DNOM
          XDMX=YGDB-YGDA+(YGDA-YGDC)*(ZGDB-ZGDA)/(ZGDC-ZGDA)
          YDMX=XGDA-XGDB+(XGDC-XGDA)*(ZGDB-ZGDA)/(ZGDC-ZGDA)
          GRMX=ABS(COFA*XDMX+COFB*YDMX)/SQRT(XDMX**2+YDMX**2)
C             GANG=ATAN2(YDMX,XDMX)
        ELSE
          GRMX=0.
C             GANG=0.
        END IF
        KMIN=MAX(   1,  INT((MIN(XGD1,XGD2,XGD3)-XVPL)/
     +                                  (XVPR-XVPL)*REAL(IGRM)))
        KMAX=MIN(IGRM,1+INT((MAX(XGD1,XGD2,XGD3)-XVPL)/
     +                                  (XVPR-XVPL)*REAL(IGRM)))
        LMIN=MAX(   1,  INT((MIN(YGD1,YGD2,YGD3)-YVPB)/
     +                                  (YVPT-YVPB)*REAL(IGRN)))
        LMAX=MIN(IGRN,1+INT((MAX(YGD1,YGD2,YGD3)-YVPB)/
     +                                  (YVPT-YVPB)*REAL(IGRN)))
        DN12=SQRT(XD12*XD12+YD12*YD12)
        DN23=SQRT(XD23*XD23+YD23*YD23)
        DN31=SQRT(XD31*XD31+YD31*YD31)
        DO 10002 K=KMIN,KMAX
          XCBX=XVPL+(REAL(K)-.5)/REAL(IGRM)*(XVPR-XVPL)
          DO 10003 L=LMIN,LMAX
            YCBX=YVPB+(REAL(L)-.5)/REAL(IGRN)*(YVPT-YVPB)
            TS12=(YD12*XCBX-XD12*YCBX-YD12*XGD1+XD12*YGD1)/DN12
            TS23=(YD23*XCBX-XD23*YCBX-YD23*XGD2+XD23*YGD2)/DN23
            TS31=(YD31*XCBX-XD31*YCBX-YD31*XGD3+XD31*YGD3)/DN31
            IF ((TS12.LT.+.0001.AND.TS23.LT.+.0001.AND.TS31.LT.+.0001).O
     +R.(TS12.GT.-.0001.AND.TS23.GT.-.0001.AND.TS31.GT.-.0001)) THEN
              IF (GRMX.GT.RWRK(IR02+(L-1)*IGRM+K)) THEN
                RWRK(IR02+(L-1)*IGRM+K)=GRMX
C                   RWRK(IGRM*IGRN+IR02+(L-1)*IGRM+K)=GANG
              END IF
            END IF
10003     CONTINUE
10002   CONTINUE
      END IF
C
  101 CONTINUE
C
C Compute the average gradient and the standard deviation of the
C distribution of gradients.
C
      NGRV=0
      GRAV=0.
      GRSD=0.
C
      DO 10004 I=1,IGRM*IGRN
        IF (RWRK(IR02+I).GE.0.) THEN
          NGRV=NGRV+1
          GRAV=GRAV+RWRK(IR02+I)
C           GRSD=GRSD+RWRK(IR02+I)**2
        END IF
10004 CONTINUE
C
      IF (NGRV.NE.0) THEN
        GRAV=GRAV/NGRV
C         GRSD=SQRT(GRSD/NGRV-GRAV*GRAV)
        IF (GRAV.NE.0.) THEN
          DO 10005 I=1,IGRM*IGRN
            IF (RWRK(IR02+I).GE.0.) THEN
              GRSD=GRSD+((RWRK(IR02+I)-GRAV)/GRAV)**2
            END IF
10005     CONTINUE
          GRSD=GRAV*SQRT(GRSD/NGRV)
        END IF
      END IF
C
C Done.
C
      RETURN
C
      END
