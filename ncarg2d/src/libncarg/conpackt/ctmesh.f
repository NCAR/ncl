C
C $Id: ctmesh.f,v 1.1 2003-05-28 15:44:31 kennison Exp $
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
      SUBROUTINE CTMESH (RPNT,KPNT,KOPN,
     +                   IEDG,KEDG,KOEN,
     +                   ITRI,KTRI,KOTN,
     +                   RWRK,KRWK,
     +                   IWRK,KIWK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C The routine CTMESH is called to start the process of drawing a
C contour plot, given data on a triangular mesh.
C
C RPNT is a one-dimensional array containing information about the
C points of the triangular mesh.
C
C KPNT is the index of the last element of RPNT containing data.
C
C KOPN is the length of a point node in RPNT.
C
C IEDG is a one-dimensional array containing information about the
C edges of the triangular mesh.
C
C KEDG is the index of the last element of IEDG.
C
C KOEN is the length of an edge node in IEDG.
C
C ITRI is a one-dimensional array containing information about the
C triangles of the triangular mesh.
C
C KTRI is the index of the last element of ITRI.
C
C KOTN is the length of a triangle node in ITRI.
C
C RWRK is a singly-subscripted real work array of length KRWK.
C
C KRWK is the dimension of RWRK.
C
C IWRK is a singly-subscripted integer work array of length KIWK.
C
C KIWK is the dimension of IWRK.
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
C Define a variable which will hold a single character.
C
      CHARACTER*1 SCHR
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTMESH - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If no CONPACKT routine has been called before, initialize required
C constants.
C
      IF (INIT.EQ.0) THEN
        CALL CTINRC
        IF (ICFELL('CTMESH',2).NE.0) RETURN
      END IF
C
C Transfer the array dimensions and node lengths to variables in COMMON.
C
      NPNT=KPNT
      LOPN=KOPN
C
      NEDG=KEDG
      LOEN=KOEN
C
      NTRI=KTRI
      LOTN=KOTN
C
      LRWK=KRWK
C
      LIWK=KIWK
C
C Clear all the workspace block lengths.
C
      DO 10001 I=1,4
        LRWS(I)=0
10001 CONTINUE
C
      DO 10002 I=1,2
        LIWS(I)=0
10002 CONTINUE
C
C Zero the internal parameters which keep track of workspace usage.
C
      IIWU=0
      IRWU=0
C
C Swap the end points of each edge of the mesh so that the field value
C at the first point is less than or equal to the field value at the
C second point.  Because this changes the definitions of "left" and
C "right" for the edge, the pointers to the triangles to the left and
C right of the edge must also be swapped.
C
      DO 10003 IPTE=0,NEDG-LOEN,LOEN
        IF (RPNT(IEDG(IPTE+1)+4).GT.RPNT(IEDG(IPTE+2)+4)) THEN
          ITMP=IEDG(IPTE+1)
          IEDG(IPTE+1)=IEDG(IPTE+2)
          IEDG(IPTE+2)=ITMP
          ITMP=IEDG(IPTE+3)
          IEDG(IPTE+3)=IEDG(IPTE+4)
          IEDG(IPTE+4)=ITMP
        END IF
10003 CONTINUE
C
C Find the ranges of the X, Y, and Z coordinates, the field values,
C and the values of the 2D coordinates in the projection plane, over
C the unblocked portion of the mesh.
C
      ITM1=0
C
      XMIN=0.
      XMAX=0.
      YMIN=0.
      YMAX=0.
      ZMIN=0.
      ZMAX=0.
      DMIN=0.
      DMAX=0.
C
      ITM2=0
C
      UMIN=0.
      UMAX=0.
      VMIN=0.
      VMAX=0.
C
      DO 10004 I=0,NTRI-LOTN,LOTN
        IF (ITRI(I+4).EQ.0) THEN
          DO 10005 J=1,3
            DO 10006 K=1,2
              L=IEDG(ITRI(I+J)+K)
              IF (ITM1.EQ.0) THEN
                ITM1=1
                XMIN=RPNT(L+1)
                XMAX=RPNT(L+1)
                YMIN=RPNT(L+2)
                YMAX=RPNT(L+2)
                ZMIN=RPNT(L+3)
                ZMAX=RPNT(L+3)
                DMIN=RPNT(L+4)
                DMAX=RPNT(L+4)
              ELSE
                XMIN=MIN(XMIN,RPNT(L+1))
                XMAX=MAX(XMAX,RPNT(L+1))
                YMIN=MIN(YMIN,RPNT(L+2))
                YMAX=MAX(YMAX,RPNT(L+2))
                ZMIN=MIN(ZMIN,RPNT(L+3))
                ZMAX=MAX(ZMAX,RPNT(L+3))
                DMIN=MIN(DMIN,RPNT(L+4))
                DMAX=MAX(DMAX,RPNT(L+4))
              END IF
              IF (IMPF.EQ.0) THEN
                UTMP=RPNT(L+1)
                VTMP=RPNT(L+2)
              ELSE
                CALL HLUCTMXYZ (IMPF,RPNT(L+1),RPNT(L+2),RPNT(L+3),
     +                                                   UTMP,VTMP)
                IF (ICFELL('CTMESH',3).NE.0) RETURN
                IF (OORV.NE.0..AND.(UTMP.EQ.OORV.OR.VTMP.EQ.OORV))
     +                                                       GO TO 101
              END IF
              IF (ITM2.EQ.0) THEN
                ITM2=1
                UMIN=UTMP
                UMAX=UTMP
                VMIN=VTMP
                VMAX=VTMP
              ELSE
                UMIN=MIN(UMIN,UTMP)
                UMAX=MAX(UMAX,UTMP)
                VMIN=MIN(VMIN,VTMP)
                VMAX=MAX(VMAX,VTMP)
              END IF
  101       CONTINUE
10006       CONTINUE
10005     CONTINUE
        END IF
10004 CONTINUE
C
C If the user has done a SET call, retrieve the arguments; if he hasn't
C done a SET call, do it for him.
C
      IF (ISET.EQ.0) THEN
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('CTMESH',4).NE.0) RETURN
C
      ELSE
C
        LNLG=1
C
        IF (UWDL.EQ.UWDR) THEN
          XWDL=UMIN
          XWDR=UMAX
        ELSE
          XWDL=UWDL
          XWDR=UWDR
        END IF
C
        IF (UWDB.EQ.UWDT) THEN
          YWDB=VMIN
          YWDT=VMAX
        ELSE
          YWDB=UWDB
          YWDT=UWDT
        END IF
C
        IF (UVPS.LT.0.) THEN
          RWTH=ABS(UVPS)
        ELSE IF (UVPS.EQ.0.) THEN
          RWTH=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE IF (UVPS.LE.1.) THEN
          RWTH=ABS((XWDR-XWDL)/(YWDT-YWDB))
          IF (MIN(RWTH,1./RWTH).LT.UVPS) RWTH=(UVPR-UVPL)/(UVPT-UVPB)
        ELSE
          RWTH=ABS((XWDR-XWDL)/(YWDT-YWDB))
          IF (MAX(RWTH,1./RWTH).GT.UVPS) RWTH=1.
        END IF
C
        IF (RWTH.LT.(UVPR-UVPL)/(UVPT-UVPB)) THEN
          XVPL=.5*(UVPL+UVPR)-.5*(UVPT-UVPB)*RWTH
          XVPR=.5*(UVPL+UVPR)+.5*(UVPT-UVPB)*RWTH
          YVPB=UVPB
          YVPT=UVPT
        ELSE
          XVPL=UVPL
          XVPR=UVPR
          YVPB=.5*(UVPB+UVPT)-.5*(UVPR-UVPL)/RWTH
          YVPT=.5*(UVPB+UVPT)+.5*(UVPR-UVPL)/RWTH
        END IF
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        IF (ICFELL('CTMESH',5).NE.0) RETURN
C
      END IF
C
C Set the flag MIRO, which indicates whether or not the transformations
C in effect cause mirror imaging.  To do this, we look for an unblocked
C triangle in the mesh, all of whose vertices are visible under the
C current mapping, and check to see if its vertices, after mapping, are
C still in counterclockwise order (in which case we set MIRO=0) or not
C (in which case we set MIRO=1).
C
      MIRO=0
C
      IF (IMPF.NE.0) THEN
C
        DO 10007 I=0,NTRI-LOTN,LOTN
C
C Use only unblocked triangles.
C
          IF (ITRI(I+4).EQ.0) THEN
C
C Find the base index of the point that edges 1 and 2 have in common.
C
            IF (IEDG(ITRI(I+1)+1).EQ.IEDG(ITRI(I+2)+1).OR.IEDG(ITRI(I+1)
     ++1).EQ.IEDG(ITRI(I+2)+2)) THEN
              IPP1=IEDG(ITRI(I+1)+1)
            ELSE
              IPP1=IEDG(ITRI(I+1)+2)
            END IF
C
C Find the base index of the point that edges 2 and 3 have in common.
C
            IF (IEDG(ITRI(I+2)+1).EQ.IEDG(ITRI(I+3)+1).OR.IEDG(ITRI(I+2)
     ++1).EQ.IEDG(ITRI(I+3)+2)) THEN
              IPP2=IEDG(ITRI(I+2)+1)
            ELSE
              IPP2=IEDG(ITRI(I+2)+2)
            END IF
C
C Find the base index of the point that edges 3 and 1 have in common.
C
            IF (IEDG(ITRI(I+3)+1).EQ.IEDG(ITRI(I+1)+1).OR.IEDG(ITRI(I+3)
     ++1).EQ.IEDG(ITRI(I+1)+2)) THEN
              IPP3=IEDG(ITRI(I+3)+1)
            ELSE
              IPP3=IEDG(ITRI(I+3)+2)
            END IF
C
C Project point 1; if it's invisible, skip the triangle.
C
            CALL HLUCTMXYZ (IMPF,
     +                      RPNT(IPP1+1),RPNT(IPP1+2),RPNT(IPP1+3),
     +                                                      XCP1,YCP1)
            IF (ICFELL('CTMESH',6).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCP1.EQ.OORV.OR.YCP1.EQ.OORV))
     +                                                       GO TO 102
C
C Project point 2; if it's invisible, skip the triangle.
C
            CALL HLUCTMXYZ (IMPF,
     +                      RPNT(IPP2+1),RPNT(IPP2+2),RPNT(IPP2+3),
     +                                                      XCP2,YCP2)
            IF (ICFELL('CTMESH',7).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCP2.EQ.OORV.OR.YCP2.EQ.OORV))
     +                                                       GO TO 102
C
C Project point 3; if it's invisible, skip the triangle.
C
            CALL HLUCTMXYZ (IMPF,
     +                      RPNT(IPP3+1),RPNT(IPP3+2),RPNT(IPP3+3),
     +                                                      XCP3,YCP3)
            IF (ICFELL('CTMESH',8).NE.0) RETURN
            IF ((OORV.NE.0.).AND.(XCP3.EQ.OORV.OR.YCP3.EQ.OORV))
     +                                                       GO TO 102
C
C If two points of the triangle are too close to each other, skip it.
C
            IF (ABS(XCP1-XCP2).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCP1-YCP2).LT..0001*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP2-XCP3).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCP2-YCP3).LT..0001*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP3-XCP1).LT..0001*ABS(XWDR-XWDL).AND.
     +          ABS(YCP3-YCP1).LT..0001*ABS(YWDT-YWDB)) GO TO 102
C
C If two points of the triangle are too far apart, skip it.
C
            IF (ABS(XCP1-XCP2).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCP1-YCP2).GT..5*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP2-XCP3).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCP2-YCP3).GT..5*ABS(YWDT-YWDB)) GO TO 102
            IF (ABS(XCP3-XCP1).GT..5*ABS(XWDR-XWDL).OR.
     +          ABS(YCP3-YCP1).GT..5*ABS(YWDT-YWDB)) GO TO 102
C
C Use this triangle to make the decision.  If point 1 is to the right
C of the vector from point 3 to point 2, then the mapping does not
C cause mirror imaging; otherwise, it does.
C
            IF (ABS(XCP2-XCP3).LT.ABS(YCP2-YCP3)) THEN
              IF (XCP1.LT.XCP3+((XCP2-XCP3)/(YCP2-YCP3))*(YCP1-YCP3)) TH
     +EN
                IF (YCP3.LT.YCP2) MIRO=1
                GO TO 103
              ELSE
                IF (YCP3.GT.YCP2) MIRO=1
                GO TO 103
              END IF
            ELSE
              IF (YCP1.LT.YCP3+((YCP2-YCP3)/(XCP2-XCP3))*(XCP1-XCP3)) TH
     +EN
                IF (XCP3.GT.XCP2) MIRO=1
                GO TO 103
              ELSE
                IF (XCP3.LT.XCP2) MIRO=1
                GO TO 103
              END IF
            END IF
C
          END IF
C
C End of loop through triangles.
C
  102   CONTINUE
10007   CONTINUE
C
      END IF
C
C Zero the count of label positions selected, the count of words used
C in real workspace number 4 (for informational and high/low label
C data), and the three indices which indicate where the different kinds
C of labels are stored.
C
  103 NLBS=0
      NR04=0
      INIL=0
      INHL=0
      INLL=0
C
C Initialize the value of the scale factor used.
C
      IF (SCFS.LE.0.) THEN
        SCFU=1.
      ELSE
        SCFU=SCFS
      END IF
C
C If contour levels are being chosen by CONPACKT, zero the number of
C levels and the values of the contour interval and label interval
C used.  If new levels are not being chosen, force recomputation of
C the text-extent parameter elements for all existing contour levels,
C in case the user changes the character-quality parameter of PLOTCHAR.
C
      IF (ICLS.NE.0) THEN
        NCLV=0
        CINU=0.
        LINU=0
      ELSE
        DO 10008 I=1,NCLV
          NCLB(I)=-ABS(NCLB(I))
10008   CONTINUE
      END IF
C
C If the field is (effectively) constant, set a flag to indicate that
C and force the scale factor back to 1.  Otherwise, clear the flag.
C
      IF (DMAX-DMIN.LE.10.*EPSI*ABS((DMIN+DMAX)/2.)) THEN
        ICFF=1
        SCFU=1.
      ELSE
        ICFF=0
      END IF
C
C Find the positions of the leftmost significant digits in the largest
C absolute value in the field and in the difference between the minimum
C and the maximum values in the field.  If the field is effectively
C constant, the latter value is set equal to the former.
C
      CALL CTNUMB (MAX(ABS(DMIN/SCFU),ABS(DMAX/SCFU)),1,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
      LSDM=IEVA-1
C
      IF (ICFF.EQ.0) THEN
        CALL CTNUMB ((DMAX-DMIN)/SCFU,1,-10000,-1,-1,
     +                    ' ',' ',' ',0,0,0,0,0,0,SCHR,NCHS,NDGS,IEVA)
        LSDD=IEVA-1
      ELSE
        LSDD=LSDM
      END IF
C
C Retrieve the current PLOTCHAR function code signal character.
C
      CALL PCGETC ('FC',SCHR)
      IF (ICFELL('CTMESH',9).NE.0) RETURN
C
C If highs and lows are to be labelled, attempt to make sure that the
C string will be treated properly by PLOTCHAR.
C
      IF (LTHI.GE.4) THEN
        IF (TXHI(1:1).EQ.'H'.AND.TXHI(3:3).EQ.'B') THEN
          DO 10009 I=4,LTHI
            IF (TXHI(I:I).EQ.TXHI(2:2)) TXHI(I:I)=SCHR
10009     CONTINUE
          TXHI(2:2)=SCHR
        END IF
      END IF
C
      IF (LTLO.GE.4) THEN
        IF (TXLO(1:1).EQ.'L'.AND.TXLO(3:3).EQ.'B') THEN
          DO 10010 I=4,LTLO
            IF (TXLO(I:I).EQ.TXLO(2:2)) TXLO(I:I)=SCHR
10010     CONTINUE
          TXLO(2:2)=SCHR
        END IF
      END IF
C
C Set up the parameters used in generating numeric labels.  Set the
C number of significant digits to be used ...
C
      IF (NSDL.LT.0) THEN
        NDGL=ABS(NSDL)
      ELSE
        NDGL=MAX(0,LSDM-LSDD)+NSDL
      END IF
C
C ... the leftmost-significant digit flag ...
C
      IF (NLSD.EQ.0) THEN
        LSDL=-10000
      ELSE
        LSDL=LSDM
      END IF
C
C ... the numeric exponent type ...
C
      IF (NEXT.LE.0) THEN
        CHEX=' E '
        LEA1=1
        LEA2=1
        LEA3=1
        LEE1=0
        LEE2=1
        LEE3=0
      ELSE IF (NEXT.EQ.1) THEN
        CHEX=':L1:410:S::N:'
        IF (SCHR.NE.':') THEN
          CHEX( 1: 1)=SCHR
          CHEX( 4: 4)=SCHR
          CHEX( 8: 8)=SCHR
          CHEX(10:10)=SCHR
          CHEX(11:11)=SCHR
          CHEX(13:13)=SCHR
        END IF
        LEA1=5
        LEA2=5
        LEA3=3
        LEE1=1
        LEE2=2
        LEE3=0
      ELSE
        CHEX='x10** '
        LEA1=1
        LEA2=4
        LEA3=1
        LEE1=1
        LEE2=4
        LEE3=0
      END IF
C
C ... and the omission flags.
C
      JOMA=MOD(MAX(0,MIN(7,NOMF))/4,2)
      JODP=MOD(MAX(0,MIN(7,NOMF))/2,2)
      JOTZ=MOD(MAX(0,MIN(7,NOMF))  ,2)
C
C If the field is not constant and the scale factor is to be chosen
C here, do it now.  The parameter which specifies where the leftmost
C significant digit is assumed to be also must be updated.
C
      IF (ICFF.EQ.0.AND.SCFS.LE.0..AND.SCFS.GE.-3.) THEN
        ITMP=0
        IF (SCFS.EQ.0..OR.(SCFS.EQ.-3..AND.LSDM.LT.-1)) ITMP=LSDM+1
        IF (SCFS.EQ.-1.) ITMP=LSDM
        IF (SCFS.EQ.-2..OR.(SCFS.EQ.-3..AND.LSDM-NDGL.GE.0))
     +                                                ITMP=LSDM-NDGL+1
        SCFU=10.**ITMP
        IF (LSDL.NE.-10000) LSDL=LSDL-ITMP
      END IF
C
C Done.
C
      RETURN
C
      END
