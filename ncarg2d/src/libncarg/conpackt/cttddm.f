C
C $Id: cttddm.f,v 1.1 2004-03-26 21:00:10 kennison Exp $
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
      SUBROUTINE CTTDDM (RPNT,IEDG,ITRI,RWRK,IWRK,IDIA)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C This routine assumes that TDPACK routines are being used to map the
C triangular mesh from 3-space into 2-space and calls the routine
C TDLINE to draw it.
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
C IDIA is a flag that can be used to activate or deactivate the drawing
C of line segments.  If IDIA is non-zero, then element IDIA of each edge
C node must be a flag that says whether or not that edge is to be drawn
C (element value zero) or not (element value non-zero).
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
C IXOR(IONE,ITWO) is the exclusive OR of the 12-bit masks IONE and ITWO.
C
      IXOR(IONE,ITWO)=IAND(IOR(IONE,ITWO),4095-IAND(IONE,ITWO))
C
C ITBF(IARG) is non-zero if and only if a triangle is blocked.
C
      ITBF(IARG)=IAND(IXOR(IARG,ITBX),ITBA)
C
C Extract the values of ITBX and ITBA.
C
      ITBX=IAND(ISHIFT(ITBM,-12),4095)
      ITBA=IAND(       ITBM     ,4095)
C
C Draw the mesh.
C
      DO 101 IPTE=0,NEDG-LOEN,LOEN
C
      IF (IDIA.NE.0.AND.IEDG(IPTE+IDIA).NE.0) GO TO 101
C
      IFLL=0
C
      IF (IEDG(IPTE+3).GE.0) THEN
        IF (ITBF(ITRI(LOTN*((IEDG(IPTE+3)-1)/LOTN)+4)).EQ.0) IFLL=1
      END IF
C
      IFLR=0
C
      IF (IEDG(IPTE+4).GE.0) THEN
        IF (ITBF(ITRI(LOTN*((IEDG(IPTE+4)-1)/LOTN)+4)).EQ.0) IFLR=1
      END IF
C
      IF (IFLL.NE.0.OR.IFLR.NE.0) THEN
C
        CALL TDLINE (RPNT(IEDG(IPTE+1)+1),
     +               RPNT(IEDG(IPTE+1)+2),
     +               RPNT(IEDG(IPTE+1)+3),
     +               RPNT(IEDG(IPTE+2)+1),
     +               RPNT(IEDG(IPTE+2)+2),
     +               RPNT(IEDG(IPTE+2)+3))
C
      END IF
C
  101 CONTINUE
C
C Done.
C
      RETURN
C
      END
