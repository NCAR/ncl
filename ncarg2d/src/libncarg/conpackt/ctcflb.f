C
C $Id: ctcflb.f,v 1.1 2003-05-28 15:44:27 kennison Exp $
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
      SUBROUTINE CTCFLB (IACT,RWRK,IAMA)
C
      DIMENSION RWRK(*),IAMA(*)
C
C CTCFLB generates the constant-field label.  If IACT = 1, the label is
C plotted.  If IACT = 2, the label box is added to the area map in IAMA.
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
C Declare local arrays to hold coordinates for area fill of boxes.
C
      DIMENSION BFXC(4),BFYC(4)
C
C Define some local arrays in which to retrieve information from GKS.
C
      DIMENSION DUMI(4),VPRT(4),WIND(4)
C
C Define some arithmetic statement functions to get from the fractional
C system to the world system.
C
      CFWX(X)=WIND(1)+(WIND(2)-WIND(1))*(X-VPRT(1))/(VPRT(2)-VPRT(1))
      CFWY(Y)=WIND(3)+(WIND(4)-WIND(3))*(Y-VPRT(3))/(VPRT(4)-VPRT(3))
C
C Retrieve the definitions of the current GKS window and viewport.
C
      CALL GQCNTN (IGER,NCNT)
C
      IF (IGER.NE.0) THEN
        CALL SETER ('CTCFLB - ERROR EXIT FROM GQCNTN',1,1)
        RETURN
      END IF
C
      CALL GQNT (NCNT,IGER,WIND,VPRT)
C
      IF (IGER.NE.0) THEN
        CALL SETER ('CTCFLB - ERROR EXIT FROM GQNT',2,1)
        RETURN
      END IF
C
C If the text string for the constant-field label is blank, do nothing.
C
      IF (TXCF(1:LTCF).EQ.' ') RETURN
C
C Otherwise, form the constant-field label ...
C
      DVAL=DMIN
      CALL CTSBST (TXCF(1:LTCF),CTMA,LCTM)
C
C ... get sizing information for the label ...
C
      XPFS=XVPL+CXCF*(XVPR-XVPL)
      YPFS=YVPB+CYCF*(YVPT-YVPB)
      XLBC=CFUX(XPFS)
      IF (ICFELL('CTCFLB',3).NE.0) RETURN
      YLBC=CFUY(YPFS)
      IF (ICFELL('CTCFLB',4).NE.0) RETURN
      WCFS=CHWM*WCCF*(XVPR-XVPL)
      WWFS=CHWM*WWCF*(XVPR-XVPL)
C
      CALL PCGETI ('TE',ITMP)
      IF (ICFELL('CTCFLB',5).NE.0) RETURN
      CALL PCSETI ('TE',1)
      IF (ICFELL('CTCFLB',6).NE.0) RETURN
      CALL HLUCTCHCF (+1)
      IF (ICFELL('CTCFLB',7).NE.0) RETURN
      CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,360.,0.)
      IF (ICFELL('CTCFLB',8).NE.0) RETURN
      CALL HLUCTCHCF (-1)
      IF (ICFELL('CTCFLB',9).NE.0) RETURN
      CALL PCGETR ('DL',DSTL)
      IF (ICFELL('CTCFLB',10).NE.0) RETURN
      CALL PCGETR ('DR',DSTR)
      IF (ICFELL('CTCFLB',11).NE.0) RETURN
      CALL PCGETR ('DB',DSTB)
      IF (ICFELL('CTCFLB',12).NE.0) RETURN
      CALL PCGETR ('DT',DSTT)
      IF (ICFELL('CTCFLB',13).NE.0) RETURN
      CALL PCSETI ('TE',ITMP)
      IF (ICFELL('CTCFLB',14).NE.0) RETURN
      DSTL=DSTL+WWFS
      DSTR=DSTR+WWFS
      DSTB=DSTB+WWFS
      DSTT=DSTT+WWFS
C
C ... and then take the desired action, either plotting the label or
C putting a box around it into the area map.
C
      SINA=SIN(.017453292519943*ANCF)
      COSA=COS(.017453292519943*ANCF)
C
      IXPO=MOD(IPCF+4,3)-1
C
      IF (IXPO.LT.0) THEN
        XPFS=XPFS+DSTL*COSA
        YPFS=YPFS+DSTL*SINA
      ELSE IF (IXPO.GT.0) THEN
        XPFS=XPFS-DSTR*COSA
        YPFS=YPFS-DSTR*SINA
      END IF
C
      IYPO=(IPCF+4)/3-1
C
      IF (IYPO.LT.0) THEN
        XPFS=XPFS-DSTB*SINA
        YPFS=YPFS+DSTB*COSA
      ELSE IF (IYPO.GT.0) THEN
        XPFS=XPFS+DSTT*SINA
        YPFS=YPFS-DSTT*COSA
      END IF
C
      XLBC=CFUX(XPFS)
      IF (ICFELL('CTCFLB',15).NE.0) RETURN
      YLBC=CFUY(YPFS)
      IF (ICFELL('CTCFLB',16).NE.0) RETURN
C
      IF (IACT.EQ.1) THEN
        IF (MOD(IBCF/2,2).NE.0) THEN
          JLBC=ILBC
          IF (JLBC.GE.0) THEN
            CALL GQFACI (IGER,ISFC)
            IF (IGER.NE.0) THEN
              CALL SETER ('CTCFLB - ERROR EXIT FROM GQFACI',17,1)
              RETURN
            END IF
            IF (ISFC.NE.JLBC) CALL GSFACI (JLBC)
          END IF
          CALL HLUCTCHCF (+2)
          IF (ICFELL('CTCFLB',18).NE.0) RETURN
          BFXC(1)=CFWX(XPFS-DSTL*COSA+DSTB*SINA)
          IF (ICFELL('CTCFLB',19).NE.0) RETURN
          BFYC(1)=CFWY(YPFS-DSTL*SINA-DSTB*COSA)
          IF (ICFELL('CTCFLB',20).NE.0) RETURN
          BFXC(2)=CFWX(XPFS+DSTR*COSA+DSTB*SINA)
          IF (ICFELL('CTCFLB',21).NE.0) RETURN
          BFYC(2)=CFWY(YPFS+DSTR*SINA-DSTB*COSA)
          IF (ICFELL('CTCFLB',22).NE.0) RETURN
          BFXC(3)=CFWX(XPFS+DSTR*COSA-DSTT*SINA)
          IF (ICFELL('CTCFLB',23).NE.0) RETURN
          BFYC(3)=CFWY(YPFS+DSTR*SINA+DSTT*COSA)
          IF (ICFELL('CTCFLB',24).NE.0) RETURN
          BFXC(4)=CFWX(XPFS-DSTL*COSA-DSTT*SINA)
          IF (ICFELL('CTCFLB',25).NE.0) RETURN
          BFYC(4)=CFWY(YPFS-DSTL*SINA+DSTT*COSA)
          IF (ICFELL('CTCFLB',26).NE.0) RETURN
          CALL GFA (4,BFXC,BFYC)
          CALL HLUCTCHCF (-2)
          IF (ICFELL('CTCFLB',27).NE.0) RETURN
          IF (JLBC.GE.0) THEN
            IF (ISFC.NE.JLBC) CALL GSFACI (ISFC)
          END IF
        END IF
        CALL GQPLCI (IGER,ISLC)
        IF (IGER.NE.0) THEN
          CALL SETER ('CTCFLB - ERROR EXIT FROM GQPLCI',28,1)
          RETURN
        END IF
        CALL GQTXCI (IGER,ISTC)
        IF (IGER.NE.0) THEN
          CALL SETER ('CTCFLB - ERROR EXIT FROM GQTXCI',29,1)
          RETURN
        END IF
        IF (ICCF.GE.0) THEN
          JCCF=ICCF
        ELSE
          JCCF=ISTC
        END IF
        JSLC=ISLC
        JSTC=ISTC
        IF (JSLC.NE.JCCF) THEN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CTCFLB',30).NE.0) RETURN
          CALL GSPLCI (JCCF)
          JSLC=JCCF
        END IF
        IF (JSTC.NE.JCCF) THEN
          CALL GSTXCI (JCCF)
          JSTC=JCCF
        END IF
        CALL GQCLIP (IGER,IGCF,DUMI)
        IF (IGER.NE.0) THEN
          CALL SETER ('CTCFLB - ERROR EXIT FROM GQCLIP',31,1)
          RETURN
        END IF
        IF (IGCF.NE.0) THEN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CTCFLB',32).NE.0) RETURN
          CALL GSCLIP (0)
        END IF
        CALL HLUCTCHCF (+3)
        IF (ICFELL('CTCFLB',33).NE.0) RETURN
        CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,ANCF,0.)
        IF (ICFELL('CTCFLB',34).NE.0) RETURN
        CALL HLUCTCHCF (-3)
        IF (ICFELL('CTCFLB',35).NE.0) RETURN
        IF (IGCF.NE.0) THEN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CTCFLB',36).NE.0) RETURN
          CALL GSCLIP (IGCF)
        END IF
        IF (MOD(IBCF,2).NE.0) THEN
          WDTH=WLCF
          IF (WDTH.GT.0.) THEN
            CALL GQLWSC (IGER,SFLW)
            IF (IGER.NE.0) THEN
              CALL SETER ('CTCFLB - ERROR EXIT FROM GQLWSC',37,1)
              RETURN
            END IF
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('CTCFLB',38).NE.0) RETURN
            CALL GSLWSC (WDTH)
          END IF
          CALL HLUCTCHCF (+4)
          IF (ICFELL('CTCFLB',39).NE.0) RETURN
          CALL PLOTIF (XPFS-DSTL*COSA+DSTB*SINA,
     +                 YPFS-DSTL*SINA-DSTB*COSA,0)
          IF (ICFELL('CTCFLB',40).NE.0) RETURN
          CALL PLOTIF (XPFS+DSTR*COSA+DSTB*SINA,
     +                 YPFS+DSTR*SINA-DSTB*COSA,1)
          IF (ICFELL('CTCFLB',41).NE.0) RETURN
          CALL PLOTIF (XPFS+DSTR*COSA-DSTT*SINA,
     +                 YPFS+DSTR*SINA+DSTT*COSA,1)
          IF (ICFELL('CTCFLB',42).NE.0) RETURN
          CALL PLOTIF (XPFS-DSTL*COSA-DSTT*SINA,
     +                 YPFS-DSTL*SINA+DSTT*COSA,1)
          IF (ICFELL('CTCFLB',43).NE.0) RETURN
          CALL PLOTIF (XPFS-DSTL*COSA+DSTB*SINA,
     +                 YPFS-DSTL*SINA-DSTB*COSA,1)
          IF (ICFELL('CTCFLB',44).NE.0) RETURN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CTCFLB',45).NE.0) RETURN
          CALL HLUCTCHCF (-4)
          IF (ICFELL('CTCFLB',46).NE.0) RETURN
          IF (WDTH.GT.0.) THEN
            CALL PLOTIF (0.,0.,2)
            IF (ICFELL('CTCFLB',47).NE.0) RETURN
            CALL GSLWSC (SFLW)
          END IF
        END IF
        IF (ISLC.NE.JSLC) THEN
          CALL PLOTIF (0.,0.,2)
          IF (ICFELL('CTCFLB',48).NE.0) RETURN
          CALL GSPLCI (ISLC)
        END IF
        IF (ISTC.NE.JSTC) CALL GSTXCI (ISTC)
      ELSE
        CALL CTGRWS (RWRK,1,10,IWSE)
        IF (IWSE.NE.0.OR.ICFELL('CTCFLB',49).NE.0) RETURN
        ANLB=.017453292519943*ANCF
        SALB=SIN(ANLB)
        CALB=COS(ANLB)
        RWRK(IR01+ 1)=CFUX(XPFS-DSTL*CALB+DSTB*SALB)
        IF (ICFELL('CTCFLB',50).NE.0) RETURN
        RWRK(IR01+ 2)=CFUX(XPFS+DSTR*CALB+DSTB*SALB)
        IF (ICFELL('CTCFLB',51).NE.0) RETURN
        RWRK(IR01+ 3)=CFUX(XPFS+DSTR*CALB-DSTT*SALB)
        IF (ICFELL('CTCFLB',52).NE.0) RETURN
        RWRK(IR01+ 4)=CFUX(XPFS-DSTL*CALB-DSTT*SALB)
        IF (ICFELL('CTCFLB',53).NE.0) RETURN
        RWRK(IR01+ 5)=RWRK(IR01+1)
        RWRK(IR01+ 6)=CFUY(YPFS-DSTL*SALB-DSTB*CALB)
        IF (ICFELL('CTCFLB',54).NE.0) RETURN
        RWRK(IR01+ 7)=CFUY(YPFS+DSTR*SALB-DSTB*CALB)
        IF (ICFELL('CTCFLB',55).NE.0) RETURN
        RWRK(IR01+ 8)=CFUY(YPFS+DSTR*SALB+DSTT*CALB)
        IF (ICFELL('CTCFLB',56).NE.0) RETURN
        RWRK(IR01+ 9)=CFUY(YPFS-DSTL*SALB+DSTT*CALB)
        IF (ICFELL('CTCFLB',57).NE.0) RETURN
        RWRK(IR01+10)=RWRK(IR01+6)
        IF ((XWDL.LT.XWDR.AND.YWDB.LT.YWDT).OR.(XWDL.GT.XWDR.AND.YWDB.GT
     +.YWDT)) THEN
          CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGLB,-1,0)
          IF (ICFELL('CTCFLB',58).NE.0) RETURN
        ELSE
          CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGLB,0,-1)
          IF (ICFELL('CTCFLB',59).NE.0) RETURN
        END IF
        LR01=0
      END IF
C
C Done.
C
      RETURN
C
      END
