C
C $Id: cttdca.f,v 1.1 2004-03-19 22:51:58 kennison Exp $
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
      SUBROUTINE CTTDCA (RPNT,IEDG,ITRI,RWRK,IWRK,ICRA,ICA1,ICAM,ICAN,
     +                                            XFCP,YFCP,XFCQ,YFCQ)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*),ICRA(ICA1,*)
C
C This routine is a version of CTCICA that assumes TDPACK routines are
C being used to map the triangular mesh from 3-space into 2-space.
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
C ICRA is the user array in which a cell array is to be returned.
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
C The variables in the following common block define TDPACK's mapping
C from 3-space to 2-space.
C
      COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
      COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
      COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
      SAVE   /TDCOM1/
C
C SIDE(X1,Y1,X2,Y2,X3,Y3) is negative if the three vertices of a given
C triangle in the plane are in clockwise order, positive if they are in
C counterclockwise order.  A zero value means that the three points are
C collinear.
C
      SIDE(X1,Y1,X2,Y2,X3,Y3)=(X1-X3)*(Y2-Y3)-(Y1-Y3)*(X2-X3)
C
C HERO(A,B,C) is the area of a triangle having sides of length A, B,
C and C (formula of Hero, or Heron), times 4.  (We are using ratios of
C the areas of triangles, so we don't worry about the factor of 4.)
C
      HERO(A,B,C)=SQRT(MAX(0.,(A+B+C)*(B+C-A)*(A+C-B)*(A+B-C)))
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTTDCA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTTDCA - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Check for errors in the arguments.
C
      IF (ICAM.LE.0.OR.ICAN.LE.0.OR.ICAM.GT.ICA1) THEN
        CALL SETER ('CTTDCA - DIMENSIONS OF CELL ARRAY ARE WRONG',3,1)
        RETURN
      END IF
C
      IF (XFCP.LT.0..OR.XFCP.GT.1..OR.YFCP.LT.0..OR.YFCP.GT.1..OR.XFCQ.L
     +T.0..OR.XFCQ.GT.1..OR.YFCQ.LT.0..OR.YFCQ.GT.1.) THEN
        CALL SETER ('CTTDCA - CORNER POINTS ARE INCORRECT',4,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTTDCA',5).NE.0) RETURN
C
C Compute some required tolerance values.
C
      TOLR=.00001*MIN(ABS(XWDR-XWDL),ABS(YWDT-YWDB))
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
        IF (ICFELL('CTTDCA',6).NE.0) RETURN
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
C Initially, we want to generate values in the cell array as follows:
C
C   The value 0 implies that the center point of the cell is not along
C   the line of sight to any part of the mesh.
C
C   A non-zero value, call it I, says that ABS(I)-1 is the base index,
C   in ITRI, of that triangle of the mesh which is nearest the eye along
C   the line of sight to the center point of the cell.  If I is greater
C   than zero, the "front" of the triangle is visible (its projection is
C   traversed in a counterclockwise direction); otherwise, the "back" of
C   the triangle is visible (its projection is traversed in a clockwise
C   direction).
C
C First, initialize the cell array to contain zeroes.
C
      DO 10001 I=1,ICAM
        DO 10002 J=1,ICAN
          ICRA(I,J)=0
10002   CONTINUE
10001 CONTINUE
C
C Examine each triangle of the triangular mesh in turn.
C
      DO 101 IPTA=0,NTRI-LOTN,LOTN
C
C Examine only unblocked triangles.
C
      IF (IAND(ITRI(IPTA+4),ITBM).NE.0) GO TO 101
C
C Find the base index of point 1 (that edges 1 and 2 have in common).
C
      IF (IEDG(ITRI(IPTA+1)+1).EQ.IEDG(ITRI(IPTA+2)+1).OR.IEDG(ITRI(IPTA
     ++1)+1).EQ.IEDG(ITRI(IPTA+2)+2)) THEN
        IPA1=IEDG(ITRI(IPTA+1)+1)
      ELSE
        IPA1=IEDG(ITRI(IPTA+1)+2)
      END IF
C
C Find the base index of point 2 (that edges 2 and 3 have in common).
C
      IF (IEDG(ITRI(IPTA+2)+1).EQ.IEDG(ITRI(IPTA+3)+1).OR.IEDG(ITRI(IPTA
     ++2)+1).EQ.IEDG(ITRI(IPTA+3)+2)) THEN
        IPA2=IEDG(ITRI(IPTA+2)+1)
      ELSE
        IPA2=IEDG(ITRI(IPTA+2)+2)
      END IF
C
C Find the base index of point 3 (that edges 3 and 1 have in common).
C
      IF (IEDG(ITRI(IPTA+3)+1).EQ.IEDG(ITRI(IPTA+1)+1).OR.IEDG(ITRI(IPTA
     ++3)+1).EQ.IEDG(ITRI(IPTA+1)+2)) THEN
        IPA3=IEDG(ITRI(IPTA+3)+1)
      ELSE
        IPA3=IEDG(ITRI(IPTA+3)+2)
      END IF
C
C Project all three points.
C
      CALL TDPRPT (RPNT(IPA1+1),RPNT(IPA1+2),RPNT(IPA1+3),XPA1,YPA1)
      CALL TDPRPT (RPNT(IPA2+1),RPNT(IPA2+2),RPNT(IPA2+3),XPA2,YPA2)
      CALL TDPRPT (RPNT(IPA3+1),RPNT(IPA3+2),RPNT(IPA3+3),XPA3,YPA3)
C
C If any two points are too close together, skip the triangle.
C
      IF (ABS(XPA2-XPA1).LT.TOLR.AND.
     +    ABS(YPA2-YPA1).LT.TOLR) GO TO 101
      IF (ABS(XPA3-XPA2).LT.TOLR.AND.
     +    ABS(YPA3-YPA2).LT.TOLR) GO TO 101
      IF (ABS(XPA1-XPA3).LT.TOLR.AND.
     +    ABS(YPA1-YPA3).LT.TOLR) GO TO 101
C
C Find the fractional coordinates of all three points.
C
      XFA1=CUFX(XPA1)
      IF (ICFELL('CTTDCA',7).NE.0) RETURN
      YFA1=CUFY(YPA1)
      IF (ICFELL('CTTDCA',8).NE.0) RETURN
C
      XFA2=CUFX(XPA2)
      IF (ICFELL('CTTDCA',9).NE.0) RETURN
      YFA2=CUFY(YPA2)
      IF (ICFELL('CTTDCA',10).NE.0) RETURN
C
      XFA3=CUFX(XPA3)
      IF (ICFELL('CTTDCA',11).NE.0) RETURN
      YFA3=CUFY(YPA3)
      IF (ICFELL('CTTDCA',12).NE.0) RETURN
C
C Set a flag that says whether the projected triangle is given in
C clockwise or counterclockwise order.
C
      IF (SIDE(XFA1,YFA1,XFA2,YFA2,XFA3,YFA3).LT.0.) THEN
        ICCW=-1
      ELSE
        ICCW=+1
      END IF
C
C Compute the coordinates of the center point of the triangle and the
C square of the distance from the eye to that point.
C
C THINK ABOUT THIS: SHOULD I USE THE DISTANCES TO THE CENTERS OF THE
C TRIANGLES OR THE DISTANCES ALONG THE RAY THROUGH THE CENTER POINT OF
C THE CELL?  I'M PRETTY SURE THE 1ST IS FASTER.  IS THE 2ND BETTER?
C
      XCTA=(RPNT(IPA1+1)+RPNT(IPA2+1)+RPNT(IPA3+1))/3.
      YCTA=(RPNT(IPA1+2)+RPNT(IPA2+2)+RPNT(IPA3+2))/3.
      ZCTA=(RPNT(IPA1+3)+RPNT(IPA2+3)+RPNT(IPA3+3))/3.
C
      SDTA=(XCTA-XE)**2+(YCTA-YE)**2+(ZCTA-ZE)**2
C
C Compute X and Y coordinate differences.
C
      XD12=XFA2-XFA1
      YD12=YFA2-YFA1
      XD23=XFA3-XFA2
      YD23=YFA3-YFA2
      XD31=XFA1-XFA3
      YD31=YFA1-YFA3
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
      ITM1=MAX(1,MIN(ICAM,INT((MIN(XFA1,XFA2,XFA3)-XFCP)/
     +                        (XFCQ-XFCP)*REAL(ICAM))+1))
      ITM2=MAX(1,MIN(ICAM,INT((MAX(XFA1,XFA2,XFA3)-XFCP)/
     +                        (XFCQ-XFCP)*REAL(ICAM))+1))
C
      IBEG=MIN(ITM1,ITM2)
      IEND=MAX(ITM1,ITM2)
C
      JTM1=MAX(1,MIN(ICAN,INT((MIN(YFA1,YFA2,YFA3)-YFCP)/
     +                        (YFCQ-YFCP)*REAL(ICAN))+1))
      JTM2=MAX(1,MIN(ICAN,INT((MAX(YFA1,YFA2,YFA3)-YFCP)/
     +                        (YFCQ-YFCP)*REAL(ICAN))+1))
C
      JBEG=MIN(JTM1,JTM2)
      JEND=MAX(JTM1,JTM2)
C
C Find each cell of the cell array whose center point lies within the
C triangle and set the cell array element to point to the triangle; if
C it already points to some other triangle, make it point to the one
C nearest the eye.
C
      IOLD=-1
C
      DO 10003 I=IBEG,IEND
        XFCC=XFCP+((REAL(I)-.5)/REAL(ICAM))*(XFCQ-XFCP)
        DO 10004 J=JBEG,JEND
          YFCC=YFCP+((REAL(J)-.5)/REAL(ICAN))*(YFCQ-YFCP)
          TS12=(YD12*XFCC-XD12*YFCC-YD12*XFA1+XD12*YFA1)/DN12
          TS23=(YD23*XFCC-XD23*YFCC-YD23*XFA2+XD23*YFA2)/DN23
          TS31=(YD31*XFCC-XD31*YFCC-YD31*XFA3+XD31*YFA3)/DN31
          IF ((TS12.LT.+.00001.AND.TS23.LT.+.00001.AND.TS31.LT.+.00001).
     +OR.(TS12.GT.-.00001.AND.TS23.GT.-.00001.AND.TS31.GT.-.00001)) THEN
            IF (ICRA(I,J).EQ.0) THEN
              ICRA(I,J)=ICCW*(IPTA+1)
            ELSE
              IF (IOLD.NE.ABS(ICRA(I,J)-1)) THEN
                IOLD=ABS(ICRA(I,J)-1)
                XOLD=(RPNT(IEDG(ITRI(IOLD+1)+1)+1)+
     +                RPNT(IEDG(ITRI(IOLD+1)+2)+1)+
     +                RPNT(IEDG(ITRI(IOLD+2)+1)+1)+
     +                RPNT(IEDG(ITRI(IOLD+2)+2)+1)+
     +                RPNT(IEDG(ITRI(IOLD+3)+1)+1)+
     +                RPNT(IEDG(ITRI(IOLD+3)+2)+1))/6.
                YOLD=(RPNT(IEDG(ITRI(IOLD+1)+1)+2)+
     +                RPNT(IEDG(ITRI(IOLD+1)+2)+2)+
     +                RPNT(IEDG(ITRI(IOLD+2)+1)+2)+
     +                RPNT(IEDG(ITRI(IOLD+2)+2)+2)+
     +                RPNT(IEDG(ITRI(IOLD+3)+1)+2)+
     +                RPNT(IEDG(ITRI(IOLD+3)+2)+2))/6.
                ZOLD=(RPNT(IEDG(ITRI(IOLD+1)+1)+3)+
     +                RPNT(IEDG(ITRI(IOLD+1)+2)+3)+
     +                RPNT(IEDG(ITRI(IOLD+2)+1)+3)+
     +                RPNT(IEDG(ITRI(IOLD+2)+2)+3)+
     +                RPNT(IEDG(ITRI(IOLD+3)+1)+3)+
     +                RPNT(IEDG(ITRI(IOLD+3)+2)+3))/6.
                SOLD=(XOLD-XE)**2+(YOLD-YE)**2+(ZOLD-ZE)**2
              END IF
              IF (SDTA.LT.SOLD) ICRA(I,J)=ICCW*(IPTA+1)
            END IF
          END IF
10004   CONTINUE
10003 CONTINUE
C
  101 CONTINUE
C
C Replace the triangle indices in the cell array with area identifiers.
C
      DO 10005 I=1,ICAM
        XFCC=XFCP+((REAL(I)-.5)/REAL(ICAM))*(XFCQ-XFCP)
        DO 10006 J=1,ICAN
          YFCC=YFCP+((REAL(J)-.5)/REAL(ICAN))*(YFCQ-YFCP)
          IF (ICRA(I,J).LE.0) THEN
            ICRA(I,J)=IAIA(257)
          ELSE
            IPTA=ICRA(I,J)-1
            IF (IEDG(ITRI(IPTA+1)+1).EQ.IEDG(ITRI(IPTA+2)+1).OR.IEDG(ITR
     +I(IPTA+1)+1).EQ.IEDG(ITRI(IPTA+2)+2)) THEN
              IPA1=IEDG(ITRI(IPTA+1)+1)
            ELSE
              IPA1=IEDG(ITRI(IPTA+1)+2)
            END IF
            IF (IEDG(ITRI(IPTA+2)+1).EQ.IEDG(ITRI(IPTA+3)+1).OR.IEDG(ITR
     +I(IPTA+2)+1).EQ.IEDG(ITRI(IPTA+3)+2)) THEN
              IPA2=IEDG(ITRI(IPTA+2)+1)
            ELSE
              IPA2=IEDG(ITRI(IPTA+2)+2)
            END IF
            IF (IEDG(ITRI(IPTA+3)+1).EQ.IEDG(ITRI(IPTA+1)+1).OR.IEDG(ITR
     +I(IPTA+3)+1).EQ.IEDG(ITRI(IPTA+1)+2)) THEN
              IPA3=IEDG(ITRI(IPTA+3)+1)
            ELSE
              IPA3=IEDG(ITRI(IPTA+3)+2)
            END IF
            CALL TDPRPT (RPNT(IPA1+1),RPNT(IPA1+2),RPNT(IPA1+3),
     +                                                      XPA1,YPA1)
            CALL TDPRPT (RPNT(IPA2+1),RPNT(IPA2+2),RPNT(IPA2+3),
     +                                                      XPA2,YPA2)
            CALL TDPRPT (RPNT(IPA3+1),RPNT(IPA3+2),RPNT(IPA3+3),
     +                                                      XPA3,YPA3)
            XFA1=CUFX(XPA1)
            IF (ICFELL('CTTDCA',13).NE.0) RETURN
            YFA1=CUFY(YPA1)
            IF (ICFELL('CTTDCA',14).NE.0) RETURN
            XFA2=CUFX(XPA2)
            IF (ICFELL('CTTDCA',15).NE.0) RETURN
            YFA2=CUFY(YPA2)
            IF (ICFELL('CTTDCA',16).NE.0) RETURN
            XFA3=CUFX(XPA3)
            IF (ICFELL('CTTDCA',17).NE.0) RETURN
            YFA3=CUFY(YPA3)
            IF (ICFELL('CTTDCA',18).NE.0) RETURN
            XD12=XFA2-XFA1
            YD12=YFA2-YFA1
            XD23=XFA3-XFA2
            YD23=YFA3-YFA2
            XD31=XFA1-XFA3
            YD31=YFA1-YFA3
            FVA1=RPNT(IPA1+4)
            FVA2=RPNT(IPA2+4)
            FVA3=RPNT(IPA3+4)
            DN12=SQRT(XD12**2+YD12**2)
            DN23=SQRT(XD23**2+YD23**2)
            DN31=SQRT(XD31**2+YD31**2)
            DNC1=SQRT((XFCC-XFA1)**2+(YFCC-YFA1)**2)
            DNC2=SQRT((XFCC-XFA2)**2+(YFCC-YFA2)**2)
            DNC3=SQRT((XFCC-XFA3)**2+(YFCC-YFA3)**2)
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
C Replace the area identifiers in the cell array with color indices, as
C directed by the value of the user-set flag ICAF.
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
            IF (ICFELL('CTTDCA',19).NE.0) RETURN
10010     CONTINUE
10009   CONTINUE
      END IF
C
C Make sure there are no negative values in the cell array.
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
