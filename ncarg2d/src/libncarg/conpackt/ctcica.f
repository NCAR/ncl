C
C $Id: ctcica.f,v 1.4 2004-03-26 21:00:09 kennison Exp $
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
      SUBROUTINE CTCICA (RPNT,IEDG,ITRI,RWRK,IWRK,ICRA,ICA1,ICAM,ICAN,
     +                                            XFCP,YFCP,XFCQ,YFCQ)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*),ICRA(ICA1,*)
C
C This routine adds color indices to a user's cell array.
C
C RPNT is an array of nodes defining vertices of triangles.
C
C IEDG is an array of nodes defining edges of triangles.
C
C ITRI is an array of nodes defining triangles.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C ICRA is the user array in which the cell array is stored.
C
C ICA1 is the first dimension of the FORTRAN array ICRA.
C
C ICAM is the first dimension of the cell array.
C
C ICAN is the second dimension of the cell array.
C
C (XFCP,YFCP) is the point at that corner of the rectangular area
C into which the cell array maps that corresponds to the cell (1,1).
C The coordinates are given in the fractional coordinate system (unlike
C what is required in a call to GCA, in which the coordinates of the
C point P are in the world coordinate system).
C
C (XFCQ,YFCQ) is the point at that corner of the rectangular area into
C which the cell array maps that corresponds to the cell (ICAM,ICAN).
C The coordinates are given in the fractional coordinate system (unlike
C what is required in a call to GCA, in which the coordinates of the
C point Q are in the world coordinate system).
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
C
C HERO(A,B,C) is the area of a triangle having sides of length A, B,
C and C (formula of Hero, or Heron), times 4.  (We are using ratios of
C the areas of triangles, so we don't worry about the factor of 4.)
C
      HERO(A,B,C)=SQRT(MAX(0.,(A+B+C)*(B+C-A)*(A+C-B)*(A+B-C)))
C
C IXOR(IONE,ITWO) is the exclusive OR of the 12-bit masks IONE and ITWO.
C
      IXOR(IONE,ITWO)=IAND(IOR(IONE,ITWO),4095-IAND(IONE,ITWO))
C
C ITBF(IARG) is non-zero if and only if a triangle is blocked.
C
      ITBF(IARG)=IAND(IXOR(IARG,ITBX),ITBA)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTCICA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTCICA - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Check for errors in the arguments.
C
      IF (ICAM.LE.0.OR.ICAN.LE.0.OR.ICAM.GT.ICA1) THEN
        CALL SETER ('CTCICA - DIMENSIONS OF CELL ARRAY ARE WRONG',3,1)
        RETURN
      END IF
C
      IF (XFCP.LT.0..OR.XFCP.GT.1..OR.YFCP.LT.0..OR.YFCP.GT.1..OR.XFCQ.L
     +T.0..OR.XFCQ.GT.1..OR.YFCQ.LT.0..OR.YFCQ.GT.1.) THEN
        CALL SETER ('CTCICA - CORNER POINTS ARE INCORRECT',4,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTCICA',5).NE.0) RETURN
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
        IF (ICFELL('CTCICA',6).NE.0) RETURN
      END IF
C
C If no levels are defined now, do nothing.
C
      IF (NCLV.LE.0) RETURN
C
C Get indices for the contour levels in ascending order.
C
      CALL CTSORT (CLEV,NCLV,ICLP)
C
C Compute some required tolerance values.
C
      TOL1=.00001*MIN(ABS(XVPR-XVPL),ABS(YVPT-YVPB))
      TOL2=.50000*MIN(ABS(XVPR-XVPL),ABS(YVPT-YVPB))
C
C Extract the values of ITBX and ITBA.
C
      ITBX=IAND(ISHIFT(ITBM,-12),4095)
      ITBA=IAND(       ITBM     ,4095)
C
C Initialize the cell array to contain the off-grid value of IAID.
C
      DO 10001 I=1,ICAM
        DO 10002 J=1,ICAN
          ICRA(I,J)=IAIA(257)
10002   CONTINUE
10001 CONTINUE
C
C If mapping is activated and an out-of-range value is defined, check
C for out-of-range cells in the cell array and redefine them to contain
C the out-of-range value of IAID.
C
      IF (IMPF.NE.0.AND.OORV.NE.0.) THEN
C
        DO 10003 I=1,ICAM
C
          XFCC=XFCP+(REAL(I)-.5)*((XFCQ-XFCP)/REAL(ICAM))
          XUCC=CFUX(XFCC)
          IF (ICFELL('CTCICA',7).NE.0) RETURN
C
          DO 10004 J=1,ICAN
C
            YFCC=YFCP+(REAL(J)-.5)*((YFCQ-YFCP)/REAL(ICAN))
            YUCC=CFUY(YFCC)
            IF (ICFELL('CTCICA',8).NE.0) RETURN
C
            CALL HLUCTMXYZ (-IMPF,XUCC,YUCC,0.,XTMP,YTMP)
            IF (ICFELL('CTCICA',9).NE.0) RETURN
C
            IF (XTMP.EQ.OORV) THEN
              ICRA(I,J)=IAIA(258)
            END IF
C
10004     CONTINUE
C
10003   CONTINUE
C
      END IF
C
C Examine each triangle of the triangular mesh in turn.
C
      DO 101 IIII=0,NTRI-LOTN,LOTN
C
C Use only unblocked triangles.
C
      IF (ITBF(ITRI(IIII+4)).NE.0) GO TO 101
C
C Find the base index of point 1 (that edges 1 and 2 have in common).
C
      IF (IEDG(ITRI(IIII+1)+1).EQ.IEDG(ITRI(IIII+2)+1).OR.IEDG(ITRI(IIII
     ++1)+1).EQ.IEDG(ITRI(IIII+2)+2)) THEN
        IPP1=IEDG(ITRI(IIII+1)+1)
      ELSE
        IPP1=IEDG(ITRI(IIII+1)+2)
      END IF
C
C Project point 1; if it's invisible, skip the triangle.
C
      IF (IMPF.EQ.0) THEN
        XCU1=RPNT(IPP1+1)
        YCU1=RPNT(IPP1+2)
      ELSE
        CALL HLUCTMXYZ (IMPF,RPNT(IPP1+1),RPNT(IPP1+2),RPNT(IPP1+3),
     +                                                    XCU1,YCU1)
        IF (ICFELL('CTCICA',10).NE.0) RETURN
        IF ((OORV.NE.0.).AND.(XCP1.EQ.OORV.OR.YCP1.EQ.OORV))
     +                                                     GO TO 101
      END IF
C
C Find the base index of point 2 (that edges 2 and 3 have in common).
C
      IF (IEDG(ITRI(IIII+2)+1).EQ.IEDG(ITRI(IIII+3)+1).OR.IEDG(ITRI(IIII
     ++2)+1).EQ.IEDG(ITRI(IIII+3)+2)) THEN
        IPP2=IEDG(ITRI(IIII+2)+1)
      ELSE
        IPP2=IEDG(ITRI(IIII+2)+2)
      END IF
C
C Project point 2; if it's invisible, skip the triangle.
C
      IF (IMPF.EQ.0) THEN
        XCU2=RPNT(IPP2+1)
        YCU2=RPNT(IPP2+2)
      ELSE
        CALL HLUCTMXYZ (IMPF,RPNT(IPP2+1),RPNT(IPP2+2),RPNT(IPP2+3),
     +                                                    XCU2,YCU2)
        IF (ICFELL('CTCICA',11).NE.0) RETURN
        IF ((OORV.NE.0.).AND.(XCP2.EQ.OORV.OR.YCP2.EQ.OORV))
     +                                                     GO TO 101
      END IF
C
C Find the base index of point 3 (that edges 3 and 1 have in common).
C
      IF (IEDG(ITRI(IIII+3)+1).EQ.IEDG(ITRI(IIII+1)+1).OR.IEDG(ITRI(IIII
     ++3)+1).EQ.IEDG(ITRI(IIII+1)+2)) THEN
        IPP3=IEDG(ITRI(IIII+3)+1)
      ELSE
        IPP3=IEDG(ITRI(IIII+3)+2)
      END IF
C
C Project point 3; if it's invisible, skip the triangle.
C
      IF (IMPF.EQ.0) THEN
        XCU3=RPNT(IPP3+1)
        YCU3=RPNT(IPP3+2)
      ELSE
        CALL HLUCTMXYZ (IMPF,RPNT(IPP3+1),RPNT(IPP3+2),RPNT(IPP3+3),
     +                                                    XCU3,YCU3)
        IF (ICFELL('CTCICA',12).NE.0) RETURN
        IF ((OORV.NE.0.).AND.(XCP3.EQ.OORV.OR.YCP3.EQ.OORV))
     +                                                     GO TO 101
      END IF
C
C Find the fractional coordinates of all three points.
C
      XCF1=CUFX(XCU1)
      IF (ICFELL('CTCICA',13).NE.0) RETURN
      YCF1=CUFY(YCU1)
      IF (ICFELL('CTCICA',14).NE.0) RETURN
C
      XCF2=CUFX(XCU2)
      IF (ICFELL('CTCICA',15).NE.0) RETURN
      YCF2=CUFY(YCU2)
      IF (ICFELL('CTCICA',16).NE.0) RETURN
C
      XCF3=CUFX(XCU3)
      IF (ICFELL('CTCICA',17).NE.0) RETURN
      YCF3=CUFY(YCU3)
      IF (ICFELL('CTCICA',18).NE.0) RETURN
C
C Compute X and Y coordinate differences.
C
      XD12=XCF2-XCF1
      YD12=YCF2-YCF1
      XD23=XCF3-XCF2
      YD23=YCF3-YCF2
      XD31=XCF1-XCF3
      YD31=YCF1-YCF3
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
      FVA1=RPNT(IPP1+4)
      FVA2=RPNT(IPP2+4)
      FVA3=RPNT(IPP3+4)
C
C Compute the lengths of the sides of the triangle.
C
      DN12=SQRT(XD12**2+YD12**2)
      DN23=SQRT(XD23**2+YD23**2)
      DN31=SQRT(XD31**2+YD31**2)
C
C Set loop limits so as to examine the center points of all cells of
C the cell array that overlap the bounding box of the triangle.
C
      ITM1=MAX(1,MIN(ICAM,INT((MIN(XCF1,XCF2,XCF3)-XFCP)/
     +                        (XFCQ-XFCP)*REAL(ICAM))+1))
      ITM2=MAX(1,MIN(ICAM,INT((MAX(XCF1,XCF2,XCF3)-XFCP)/
     +                        (XFCQ-XFCP)*REAL(ICAM))+1))
C
      IBEG=MIN(ITM1,ITM2)
      IEND=MAX(ITM1,ITM2)
C
      JTM1=MAX(1,MIN(ICAN,INT((MIN(YCF1,YCF2,YCF3)-YFCP)/
     +                        (YFCQ-YFCP)*REAL(ICAN))+1))
      JTM2=MAX(1,MIN(ICAN,INT((MAX(YCF1,YCF2,YCF3)-YFCP)/
     +                        (YFCQ-YFCP)*REAL(ICAN))+1))
C
      JBEG=MIN(JTM1,JTM2)
      JEND=MAX(JTM1,JTM2)
C
C Find each cell of the cell array whose center point lies within
C the triangle and set its color index appropriately.
C
      DO 10005 I=IBEG,IEND
        XFCC=XFCP+((REAL(I)-.5)/REAL(ICAM))*(XFCQ-XFCP)
        DO 10006 J=JBEG,JEND
          YFCC=YFCP+((REAL(J)-.5)/REAL(ICAN))*(YFCQ-YFCP)
          TS12=(YD12*XFCC-XD12*YFCC-YD12*XCF1+XD12*YCF1)/DN12
          TS23=(YD23*XFCC-XD23*YFCC-YD23*XCF2+XD23*YCF2)/DN23
          TS31=(YD31*XFCC-XD31*YFCC-YD31*XCF3+XD31*YCF3)/DN31
          IF ((TS12.LT.+.00001.AND.TS23.LT.+.00001.AND.TS31.LT.+.00001).
     +OR.(TS12.GT.-.00001.AND.TS23.GT.-.00001.AND.TS31.GT.-.00001)) THEN
            DNC1=SQRT((XFCC-XCF1)**2+(YFCC-YCF1)**2)
            DNC2=SQRT((XFCC-XCF2)**2+(YFCC-YCF2)**2)
            DNC3=SQRT((XFCC-XCF3)**2+(YFCC-YCF3)**2)
            ATR1=HERO(DN23,DNC2,DNC3)
            ATR2=HERO(DN31,DNC3,DNC1)
            ATR3=HERO(DN12,DNC1,DNC2)
            ATOT=ATR1+ATR2+ATR3
            IF (ATOT.NE.0.) THEN
              CALL CTGVAI ((ATR1*FVA1+ATR2*FVA2+ATR3*FVA3)/ATOT,
     +                                                ICRA(I,J))
            END IF
          END IF
10006   CONTINUE
10005 CONTINUE
C
  101 CONTINUE
C
C Adjust the values in the cell array per the user-set flag ICAF.
C
      IF (ICAF.GT.0) THEN
        DO 10007 I=1,ICAM
          DO 10008 J=1,ICAN
            ICRA(I,J)=ICAF+ICRA(I,J)
10008     CONTINUE
10007   CONTINUE
      ELSE IF (ICAF.LT.0) THEN
        DO 10009 I=1,ICAM
          DO 10010 J=1,ICAN
            CALL HLUCTSCAE (ICRA,ICA1,ICAM,ICAN,
     +                      XFCP,YFCP,XFCQ,YFCQ,I,J,ICAF,ICRA(I,J))
            IF (ICFELL('CTCICA',19).NE.0) RETURN
10010     CONTINUE
10009   CONTINUE
      END IF
C
C Make sure there are no negative values in ICAF.
C
      DO 10011 I=1,ICAM
        DO 10012 J=1,ICAN
          IF (ICRA(I,J).LT.0) ICRA(I,J)=0
10012   CONTINUE
10011 CONTINUE
C
C Done.
C
      RETURN
C
      END
