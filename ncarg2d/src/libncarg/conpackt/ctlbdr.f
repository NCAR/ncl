C
C $Id: ctlbdr.f,v 1.2 2004-03-19 22:51:56 kennison Exp $
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
      SUBROUTINE CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C The function of the routine CTLBDR is to draw all of the labels.
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
C Declare local arrays to hold coordinates for area fill of boxes.
C
      DIMENSION BFXC(4),BFYC(4)
C
C Define a local array to receive some information we don't care about
C from the GKS routine GQCLIP.
C
      DIMENSION DUMI(4)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTLBDR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTLBDR - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTLBDR',3).NE.0) RETURN
C
C If the constant-field flag is set, write the constant-field message
C and quit.
C
      IF (ICFF.NE.0) THEN
        CALL CTCFLB (1,RWRK,IWRK)
        IF (ICFELL('CTLBDR',4).NE.0) RETURN
        RETURN
      END IF
C
C Make sure label positions have been chosen.
C
      CALL CTPKLP (RPNT,IEDG,ITRI,RWRK,IWRK)
      IF (ICFELL('CTLBDR',5).NE.0) THEN
        NLBS=0
        NR04=0
        INIL=0
        INHL=0
        INLL=0
        RETURN
      END IF
C
C If there are no labels in the label list, quit.
C
      IF (NLBS.LE.0) RETURN
C
C Redo the SET call so that we can use fractional-system coordinates.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XVPL,XVPR,YVPB,YVPT,1)
      IF (ICFELL('CTLBDR',6).NE.0) RETURN
C
C Set up color-index controls.
C
      CALL GQPLCI (IGER,ISLC)
      IF (IGER.NE.0) THEN
        CALL SETER ('CTLBDR - ERROR EXIT FROM GQPLCI',7,1)
        RETURN
      END IF
      CALL GQTXCI (IGER,ISTC)
      IF (IGER.NE.0) THEN
        CALL SETER ('CTLBDR - ERROR EXIT FROM GQTXCI',8,1)
        RETURN
      END IF
      CALL GQFACI (IGER,ISFC)
      IF (IGER.NE.0) THEN
        CALL SETER ('CTLBDR - ERROR EXIT FROM GQFACI',9,1)
        RETURN
      END IF
C
      IF (ICIL.GE.0) THEN
        JCIL=ICIL
      ELSE
        JCIL=ISTC
      END IF
C
      IF (ICHI.GE.0) THEN
        JCHI=ICHI
      ELSE IF (ICHL.GE.0) THEN
        JCHI=ICHL
      ELSE
        JCHI=ISTC
      END IF
C
      IF (ICLO.GE.0) THEN
        JCLO=ICLO
      ELSE IF (ICHL.GE.0) THEN
        JCLO=ICHL
      ELSE
        JCLO=ISTC
      END IF
C
      IF (ILBC.GE.0) THEN
        JLBC=ILBC
      ELSE
        JLBC=ISFC
      END IF
C
      JSLC=ISLC
      JSTC=ISTC
      JSFC=ISFC
C
C Draw all the labels.
C
      DO 10001 I=1,NLBS
C
        XCLB=RWRK(IR03+4*(I-1)+1)
        YCLB=RWRK(IR03+4*(I-1)+2)
        XLBC=XCLB
        YLBC=YCLB
        ANLB=RWRK(IR03+4*(I-1)+3)
        SALB=SIN(ANLB)
        CALB=COS(ANLB)
        ANGD=57.2957795130823*ANLB
        ICLB=INT(RWRK(IR03+4*(I-1)+4))
C
        IF (ICLB.LE.0) THEN
C
          ICLX=ICLB
C
          IF (RWRK(IR04-ICLB+1).EQ.0.) THEN
            ITYP=1
            DVAL=0.
            CALL CTSBST (TXIL(1:LTIL),CTMA,LCTM)
            WCHR=(XVPR-XVPL)*CHWM*WCIL
            IBOX=IBIL
            JCOL=JCIL
            WDTH=WLIL
          ELSE IF (RWRK(IR04-ICLB+1).EQ.1.) THEN
            ITYP=2
            DVAL=RWRK(IR04-ICLB+2)
            CALL CTSBST (TXHI(1:LTHI),CTMA,LCTM)
            WCHR=(XVPR-XVPL)*CHWM*WCHL
            IBOX=IBHL
            JCOL=JCHI
            WDTH=WLHL
          ELSE IF (RWRK(IR04-ICLB+1).EQ.2.) THEN
            ITYP=3
            DVAL=RWRK(IR04-ICLB+2)
            CALL CTSBST (TXLO(1:LTLO),CTMA,LCTM)
            WCHR=(XVPR-XVPL)*CHWM*WCHL
            IBOX=IBHL
            JCOL=JCLO
            WDTH=WLHL
          ELSE
            ICLB=INT(RWRK(IR04-ICLB+2))
            ITYP=4
            DVAL=CLEV(ICLB)
            LCTM=NCLB(ICLB)
            CTMA=CLBL(ICLB)(1:LCTM)
            WCHR=(XVPR-XVPL)*CHWM*WCLL
            IBOX=IBLL
            JCOL=ISTC
            IF (ICLL(ICLB).GE.0) JCOL=ICLL(ICLB)
            WDTH=WLLL
          END IF
C
          IF (IBOX.NE.0) THEN
            DLLB=RWRK(IR04-ICLX+3)
            DRLB=RWRK(IR04-ICLX+4)
            DBLB=RWRK(IR04-ICLX+5)
            DTLB=RWRK(IR04-ICLX+6)
          END IF
C
        ELSE
C
          ITYP=4
          DVAL=CLEV(ICLB)
          LCTM=NCLB(ICLB)
          CTMA=CLBL(ICLB)(1:LCTM)
          WCHR=(XVPR-XVPL)*CHWM*WCLL
          IBOX=IBLL
          JCOL=ISTC
          IF (ICLL(ICLB).GE.0) JCOL=ICLL(ICLB)
          WDTH=WLLL
C
          IF (IBOX.NE.0) THEN
            DLLB=CLDL(ICLB)
            DRLB=CLDR(ICLB)
            DBLB=CLDB(ICLB)
            DTLB=CLDT(ICLB)
          END IF
C
        END IF
C
        IF (ITYP.EQ.1) THEN
          CALL GQCLIP (IGER,IGCF,DUMI)
          IF (IGER.NE.0) THEN
            CALL SETER ('CTLBDR - ERROR EXIT FROM GQCLIP',10,1)
            RETURN
          END IF
          IF (IGCF.NE.0) THEN
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('CTLBDR',11).NE.0) RETURN
            CALL GSCLIP (0)
          END IF
        END IF
C
        IF (MOD(IBOX/2,2).NE.0) THEN
          IF (JSFC.NE.JLBC) THEN
            CALL GSFACI (JLBC)
            JSFC=JLBC
          END IF
          IF (ITYP.EQ.1) THEN
            CALL HLUCTCHIL (+2)
            IF (ICFELL('CTLBDR',12).NE.0) RETURN
          ELSE IF (ITYP.EQ.2) THEN
            CALL HLUCTCHHL (+2)
            IF (ICFELL('CTLBDR',13).NE.0) RETURN
          ELSE IF (ITYP.EQ.3) THEN
            CALL HLUCTCHHL (+6)
            IF (ICFELL('CTLBDR',14).NE.0) RETURN
          ELSE
            IPAI=ICLB
            IF (IPAI.GT.256) IPAI=256-IPAI
            CALL HLUCTCHLL (+2)
            IF (ICFELL('CTLBDR',15).NE.0) RETURN
          END IF
          BFXC(1)=XCLB-DLLB*CALB+DBLB*SALB
          BFYC(1)=YCLB-DLLB*SALB-DBLB*CALB
          BFXC(2)=XCLB+DRLB*CALB+DBLB*SALB
          BFYC(2)=YCLB+DRLB*SALB-DBLB*CALB
          BFXC(3)=XCLB+DRLB*CALB-DTLB*SALB
          BFYC(3)=YCLB+DRLB*SALB+DTLB*CALB
          BFXC(4)=XCLB-DLLB*CALB-DTLB*SALB
          BFYC(4)=YCLB-DLLB*SALB+DTLB*CALB
          CALL GFA (4,BFXC,BFYC)
          IF (ITYP.EQ.1) THEN
            CALL HLUCTCHIL (-2)
            IF (ICFELL('CTLBDR',16).NE.0) RETURN
          ELSE IF (ITYP.EQ.2) THEN
            CALL HLUCTCHHL (-2)
            IF (ICFELL('CTLBDR',17).NE.0) RETURN
          ELSE IF (ITYP.EQ.3) THEN
            CALL HLUCTCHHL (-6)
            IF (ICFELL('CTLBDR',18).NE.0) RETURN
          ELSE
            IPAI=ICLB
            IF (IPAI.GT.256) IPAI=256-IPAI
            CALL HLUCTCHLL (-2)
            IF (ICFELL('CTLBDR',19).NE.0) RETURN
          END IF
        END IF
C
        IF (JSLC.NE.JCOL) THEN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CTLBDR',20).NE.0) RETURN
          CALL GSPLCI (JCOL)
          JSLC=JCOL
        END IF
C
        IF (JSTC.NE.JCOL) THEN
          CALL GSTXCI (JCOL)
          JSTC=JCOL
        END IF
C
        IF (ITYP.EQ.1) THEN
          CALL HLUCTCHIL (+3)
          IF (ICFELL('CTLBDR',21).NE.0) RETURN
        ELSE IF (ITYP.EQ.2) THEN
          CALL HLUCTCHHL (+3)
          IF (ICFELL('CTLBDR',22).NE.0) RETURN
        ELSE IF (ITYP.EQ.3) THEN
          CALL HLUCTCHHL (+7)
          IF (ICFELL('CTLBDR',23).NE.0) RETURN
        ELSE
          IPAI=ICLB
          IF (IPAI.GT.256) IPAI=256-IPAI
          CALL HLUCTCHLL (+3)
          IF (ICFELL('CTLBDR',24).NE.0) RETURN
        END IF
        CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCHR,ANGD,0.)
        IF (ICFELL('CTLBDR',25).NE.0) RETURN
        IF (ITYP.EQ.1) THEN
          CALL HLUCTCHIL (-3)
          IF (ICFELL('CTLBDR',26).NE.0) RETURN
        ELSE IF (ITYP.EQ.2) THEN
          CALL HLUCTCHHL (-3)
          IF (ICFELL('CTLBDR',27).NE.0) RETURN
        ELSE IF (ITYP.EQ.3) THEN
          CALL HLUCTCHHL (-7)
          IF (ICFELL('CTLBDR',28).NE.0) RETURN
        ELSE
          IPAI=ICLB
          IF (IPAI.GT.256) IPAI=256-IPAI
          CALL HLUCTCHLL (-3)
          IF (ICFELL('CTLBDR',29).NE.0) RETURN
        END IF
C
        IF (MOD(IBOX,2).NE.0) THEN
          IF (WDTH.GT.0.) THEN
            CALL GQLWSC (IGER,SFLW)
            IF (IGER.NE.0) THEN
              CALL SETER ('CTLBDR - ERROR EXIT FROM GQLWSC',30,1)
              RETURN
            END IF
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('CTLBDR',31).NE.0) RETURN
            CALL GSLWSC (WDTH)
          END IF
          IF (ITYP.EQ.1) THEN
            CALL HLUCTCHIL (+4)
            IF (ICFELL('CTLBDR',32).NE.0) RETURN
          ELSE IF (ITYP.EQ.2) THEN
            CALL HLUCTCHHL (+4)
            IF (ICFELL('CTLBDR',33).NE.0) RETURN
          ELSE IF (ITYP.EQ.3) THEN
            CALL HLUCTCHHL (+8)
            IF (ICFELL('CTLBDR',34).NE.0) RETURN
          ELSE
            IPAI=ICLB
            IF (IPAI.GT.256) IPAI=256-IPAI
            CALL HLUCTCHLL (+4)
            IF (ICFELL('CTLBDR',35).NE.0) RETURN
          END IF
          CALL PLOTIF (XCLB-DLLB*CALB+DBLB*SALB,
     +                 YCLB-DLLB*SALB-DBLB*CALB,0)
          IF (ICFELL('CTLBDR',36).NE.0) RETURN
          CALL PLOTIF (XCLB+DRLB*CALB+DBLB*SALB,
     +                 YCLB+DRLB*SALB-DBLB*CALB,1)
          IF (ICFELL('CTLBDR',37).NE.0) RETURN
          CALL PLOTIF (XCLB+DRLB*CALB-DTLB*SALB,
     +                 YCLB+DRLB*SALB+DTLB*CALB,1)
          IF (ICFELL('CTLBDR',38).NE.0) RETURN
          CALL PLOTIF (XCLB-DLLB*CALB-DTLB*SALB,
     +                 YCLB-DLLB*SALB+DTLB*CALB,1)
          IF (ICFELL('CTLBDR',39).NE.0) RETURN
          CALL PLOTIF (XCLB-DLLB*CALB+DBLB*SALB,
     +                 YCLB-DLLB*SALB-DBLB*CALB,1)
          IF (ICFELL('CTLBDR',40).NE.0) RETURN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CTLBDR',41).NE.0) RETURN
          IF (ITYP.EQ.1) THEN
            CALL HLUCTCHIL (-4)
            IF (ICFELL('CTLBDR',42).NE.0) RETURN
          ELSE IF (ITYP.EQ.2) THEN
            CALL HLUCTCHHL (-4)
            IF (ICFELL('CTLBDR',43).NE.0) RETURN
          ELSE IF (ITYP.EQ.3) THEN
            CALL HLUCTCHHL (-8)
            IF (ICFELL('CTLBDR',44).NE.0) RETURN
          ELSE
            IPAI=ICLB
            IF (IPAI.GT.256) IPAI=256-IPAI
            CALL HLUCTCHLL (-4)
            IF (ICFELL('CTLBDR',45).NE.0) RETURN
          END IF
          IF (WDTH.GT.0.) THEN
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('CTLBDR',46).NE.0) RETURN
            CALL GSLWSC (SFLW)
          END IF
        END IF
C
        IF (ITYP.EQ.1) THEN
          IF (IGCF.NE.0) THEN
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('CTLBDR',47).NE.0) RETURN
            CALL GSCLIP (IGCF)
          END IF
        END IF
C
10001 CONTINUE
C
C Return the color indices to their original values.
C
      IF (JSLC.NE.ISLC) THEN
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CTLBDR',48).NE.0) RETURN
        CALL GSPLCI (ISLC)
      END IF
      IF (JSTC.NE.ISTC) CALL GSTXCI (ISTC)
      IF (JSFC.NE.ISFC) CALL GSFACI (ISFC)
C
C Restore the original SET parameters.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTLBDR',49).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
