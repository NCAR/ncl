C
C $Id: ctpklp.f,v 1.1 2003-05-28 15:44:33 kennison Exp $
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
      SUBROUTINE CTPKLP (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C The routine CTPKLP is called to pick the label positions.
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
C Check for an uncleared prior error.
C
      IF (ICFELL('CTPKLP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTPKLP - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTPKLP',3).NE.0) RETURN
C
C If the constant-field flag is set, do nothing.
C
      IF (ICFF.NE.0) RETURN
C
C If labels have already been positioned, don't do it again.
C
      IF (NLBS.NE.0) RETURN
C
C Make sure contour labels are completely defined.
C
      CALL CTPKLB (RPNT,IEDG,ITRI,RWRK,IWRK)
      IF (ICFELL('CTPKLP',4).NE.0) RETURN
      CALL CTSTLS (RPNT,IEDG,ITRI,RWRK,IWRK)
      IF (ICFELL('CTPKLP',5).NE.0) RETURN
C
C Save the index of the informational label.
C
      INIL=NLBS+1
C
C Add the informational label, if any, to the list.
C
      CALL CTINLB (RPNT,IEDG,ITRI,RWRK,IWRK)
      IF (ICFELL('CTPKLP',6).NE.0) RETURN
C
C Save the index of the high/low labels.
C
      INHL=NLBS+1
C
C Add the high/low labels, if any, to the list.
C
      CALL CTHLLB (RPNT,IEDG,ITRI,RWRK,IWRK)
      IF (ICFELL('CTPKLP',7).NE.0) RETURN
C
C Save the index of the contour-line labels.
C
      INLL=NLBS+1
C
C If labels are not being positioned along the contour lines using the
C regular scheme or the penalty scheme, quit now.
C
      IF (ABS(IPLL).NE.2.AND.ABS(IPLL).NE.3) RETURN
C
C If it will be needed, compute the array of gradients.
C
      IF (ABS(IPLL).EQ.3.AND.(WTGR.GT.0..OR.WTNC.GT.0.)) THEN
        RWTH=(XVPR-XVPL)/(YVPT-YVPB)
        IGRM=MAX(10,INT(SQRT(RWTH*REAL(LRWG))))
        IGRN=MAX(10,LRWG/IGRM)
        CALL CTGRWS (RWRK,2,IGRM*IGRN,IWSE)
        IF (IWSE.NE.0.OR.ICFELL('CTPKLP',8).NE.0) RETURN
        CALL CTCPAG (RPNT,IEDG,ITRI,RWRK)
        IF (ICFELL('CTPKLP',9).NE.0) RETURN
      END IF
C
C If the label-positioning flag is positive, force 2D smoothing off
C temporarily.
C
      IF (IPLL.GT.0) THEN
        S2DS=T2DS
        T2DS=0.
      END IF
C
C Trace all the contour lines, positioning labels along each.
C
        ICLW = 1
        GO TO 10003
10001   CONTINUE
        ICLW =ICLW +1
10003   CONTINUE
        IF (ICLW .GT.(NCLV)) GO TO 10002
        IF (CLEV(ICLW).GT.DMIN.AND.CLEV(ICLW).LT.DMAX) THEN
          ICLV=ICLP(ICLW)
          IF (MOD(ICLU(ICLV)/2,2).NE.0) THEN
            IJMP=0
10004       CONTINUE
              CALL CTTRCL (RPNT,IEDG,ITRI,RWRK,IWRK,CLEV(ICLV),IJMP,
     +                                               IRW1,IRW2,NRWK)
              IF (ICFELL('CTPKLP',10).NE.0) RETURN
              IF (IJMP.EQ.0) GO TO 10005
              IF (ABS(IPLL).EQ.2) THEN
                CALL CTPLAR (RWRK,IRW1,IRW2,NRWK)
                IF (ICFELL('CTPKLP',11).NE.0) RETURN
              ELSE
                CALL CTPLPS (RWRK,IRW1,IRW2,NRWK)
                IF (ICFELL('CTPKLP',12).NE.0) RETURN
              END IF
            GO TO 10004
10005       CONTINUE
          END IF
        END IF
      GO TO 10001
10002 CONTINUE
C
C If the label-positioning flag is positive, restore 2D smoothing to
C its original state.
C
      IF (IPLL.GT.0) THEN
        T2DS=S2DS
      END IF
C
C Release the space used for the gradient array, if any.
C
      LR02=0
C
C Done.
C
      RETURN
C
      END
