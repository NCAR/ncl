C
C $Id: ctsetr.f,v 1.1 2003-05-28 15:44:34 kennison Exp $
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
      SUBROUTINE CTSETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C This subroutine is called to set the real value of a specified
C parameter.
C
C WHCH is the name of the parameter whose value is to be set.
C
C RVAL is a real variable containing the new value of the parameter.
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
      IF (ICFELL('CTSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (LEN(WHCH).LT.3) THEN
        CTMB(1:36)='CTSETR - PARAMETER NAME TOO SHORT - '
        CTMB(37:36+LEN(WHCH))=WHCH
        CALL SETER (CTMB(1:36+LEN(WHCH)),2,1)
        RETURN
      END IF
C
C Check for incorrect use of the index parameter.
C
      IF (WHCH(1:3).EQ.'AIA'.OR.WHCH(1:3).EQ.'aia'.OR.WHCH(1:3).EQ.'CLC'
     +.OR.WHCH(1:3).EQ.'clc'.OR.WHCH(1:3).EQ.'CLD'.OR.WHCH(1:3).EQ.'cld'
     +.OR.WHCH(1:3).EQ.'CLL'.OR.WHCH(1:3).EQ.'cll'.OR.WHCH(1:3).EQ.'CLU'
     +.OR.WHCH(1:3).EQ.'clu') THEN
        IF (IPAI.GE.1.AND.IPAI.LE.NCLV) THEN
          JPAI=IPAI
        ELSE IF (IPAI.LE.-1.AND.IPAI.GE.-3) THEN
          JPAI=256+ABS(IPAI)
        ELSE
          GO TO 10002
        END IF
      ELSE IF ((WHCH(1:3).EQ.'AIB'.OR.WHCH(1:3).EQ.'aib'.OR.WHCH(1:3).EQ
     +.'CLV'.OR.WHCH(1:3).EQ.'clv'.OR.WHCH(1:3).EQ.'LLC'.OR.WHCH(1:3).EQ
     +.'llc').AND.(IPAI.LT.1.OR.IPAI.GT.NCLV)) THEN
        GO TO 10002
      ELSE IF ((WHCH(1:3).EQ.'CIT'.OR.WHCH(1:3).EQ.'cit'.OR.WHCH(1:3).EQ
     +.'LIT'.OR.WHCH(1:3).EQ.'lit').AND.(IPAI.LT.1.OR.IPAI.GT.10)) THEN
        GO TO 10002
      END IF
C
      GO TO 10005
10002 CONTINUE
        CTMB(1:36)='CTSETR - SETTING XXX - PAI INCORRECT'
        CTMB(18:20)=WHCH(1:3)
        CALL SETER (CTMB(1:36),3,1)
        RETURN
10005 CONTINUE
C
C Set the appropriate parameter value.
C
      IF (WHCH(1:3).EQ.'AIA'.OR.WHCH(1:3).EQ.'aia') THEN
        IAIA(JPAI)=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'AIB'.OR.WHCH(1:3).EQ.'aib') THEN
        IAIB(IPAI)=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'CAF'.OR.WHCH(1:3).EQ.'caf') THEN
        ICAF=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'CFA'.OR.WHCH(1:3).EQ.'cfa') THEN
        ANCF=RVAL
      ELSE IF (WHCH(1:3).EQ.'CFB'.OR.WHCH(1:3).EQ.'cfb') THEN
        IBCF=MAX(0,MIN(3,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'CFC'.OR.WHCH(1:3).EQ.'cfc') THEN
        ICCF=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'CFL'.OR.WHCH(1:3).EQ.'cfl') THEN
        WLCF=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'CFP'.OR.WHCH(1:3).EQ.'cfp') THEN
        IPCF=MAX(-4,MIN(4,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'CFS'.OR.WHCH(1:3).EQ.'cfs') THEN
        WCCF=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'CFW'.OR.WHCH(1:3).EQ.'cfw') THEN
        WWCF=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'CFX'.OR.WHCH(1:3).EQ.'cfx') THEN
        CXCF=RVAL
      ELSE IF (WHCH(1:3).EQ.'CFY'.OR.WHCH(1:3).EQ.'cfy') THEN
        CYCF=RVAL
      ELSE IF (WHCH(1:3).EQ.'CIS'.OR.WHCH(1:3).EQ.'cis') THEN
        CINS=RVAL
      ELSE IF (WHCH(1:3).EQ.'CIT'.OR.WHCH(1:3).EQ.'cit') THEN
        CINT(IPAI)=RVAL
      ELSE IF (WHCH(1:3).EQ.'CIU'.OR.WHCH(1:3).EQ.'ciu') THEN
        CINU=RVAL
      ELSE IF (WHCH(1:3).EQ.'CLC'.OR.WHCH(1:3).EQ.'clc') THEN
        ICCL(JPAI)=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'CLD'.OR.WHCH(1:3).EQ.'cld') THEN
        CLDP(JPAI)=' '
        ITMP=INT(RVAL)
        DO 10006 I=16,1,-1
          IF (IAND(ITMP,1).NE.0) THEN
            CLDP(JPAI)(I:I)='$'
          ELSE
            CLDP(JPAI)(I:I)=''''
          END IF
          ITMP=ISHIFT(ITMP,-1)
10006   CONTINUE
      ELSE IF (WHCH(1:3).EQ.'CLL'.OR.WHCH(1:3).EQ.'cll') THEN
        CLWA(JPAI)=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'CLS'.OR.WHCH(1:3).EQ.'cls') THEN
        ICLS=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'CLU'.OR.WHCH(1:3).EQ.'clu') THEN
        ICLU(JPAI)=MAX(0,MIN(3,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'CLV'.OR.WHCH(1:3).EQ.'clv') THEN
        CLEV(IPAI)=RVAL
        ICLU(IPAI)=1
        IAIA(IPAI)=IPAI+1
        IAIB(IPAI)=IPAI
        CLBL(IPAI)=' '
        NCLB(IPAI)=-1
        CLDP(IPAI)='$$$$$$$$$$$$$$$$'
        ICCL(IPAI)=-1
        ICLL(IPAI)=-1
        CLWA(IPAI)=0.
        IF (IPAI.EQ.1) CINU=0.
      ELSE IF (WHCH(1:3).EQ.'CMN'.OR.WHCH(1:3).EQ.'cmn') THEN
        UCMN=RVAL
      ELSE IF (WHCH(1:3).EQ.'CMX'.OR.WHCH(1:3).EQ.'cmx') THEN
        UCMX=RVAL
      ELSE IF (WHCH(1:3).EQ.'CWM'.OR.WHCH(1:3).EQ.'cwm') THEN
        CHWM=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'DPS'.OR.WHCH(1:3).EQ.'dps') THEN
        WOCH=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'DPU'.OR.WHCH(1:3).EQ.'dpu') THEN
        IDUF=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'DPV'.OR.WHCH(1:3).EQ.'dpv') THEN
        WODA=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'DVA'.OR.WHCH(1:3).EQ.'dva') THEN
        DVAL=RVAL
      ELSE IF (WHCH(1:3).EQ.'GIC'.OR.WHCH(1:3).EQ.'gic') THEN
        IGCL=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'GIL'.OR.WHCH(1:3).EQ.'gil') THEN
        IGLB=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'GIS'.OR.WHCH(1:3).EQ.'gis') THEN
        IGVS=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'HCF'.OR.WHCH(1:3).EQ.'hcf') THEN
        IHCF=MAX(-4,MIN(+4,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'HCL'.OR.WHCH(1:3).EQ.'hcl') THEN
        HCHL=RVAL
      ELSE IF (WHCH(1:3).EQ.'HCS'.OR.WHCH(1:3).EQ.'hcs') THEN
        HCHS=MAX(.0001,MIN(10.,RVAL))
      ELSE IF (WHCH(1:3).EQ.'HIC'.OR.WHCH(1:3).EQ.'hic') THEN
        ICHI=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'HLA'.OR.WHCH(1:3).EQ.'hla') THEN
        ANHL=RVAL
      ELSE IF (WHCH(1:3).EQ.'HLB'.OR.WHCH(1:3).EQ.'hlb') THEN
        IBHL=MAX(0,MIN(3,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'HLC'.OR.WHCH(1:3).EQ.'hlc') THEN
        ICHL=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'HLE'.OR.WHCH(1:3).EQ.'hle') THEN
        IHLE=MAX(0,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'HLL'.OR.WHCH(1:3).EQ.'hll') THEN
        WLHL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'HLO'.OR.WHCH(1:3).EQ.'hlo') THEN
        IOHL=MAX(0,MIN(15,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'HLR'.OR.WHCH(1:3).EQ.'hlr') THEN
        HLSR=RVAL
      ELSE IF (WHCH(1:3).EQ.'HLS'.OR.WHCH(1:3).EQ.'hls') THEN
        WCHL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'HLW'.OR.WHCH(1:3).EQ.'hlw') THEN
        WWHL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'ILA'.OR.WHCH(1:3).EQ.'ila') THEN
        ANIL=RVAL
      ELSE IF (WHCH(1:3).EQ.'ILB'.OR.WHCH(1:3).EQ.'ilb') THEN
        IBIL=MAX(0,MIN(3,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'ILC'.OR.WHCH(1:3).EQ.'ilc') THEN
        ICIL=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'ILL'.OR.WHCH(1:3).EQ.'ill') THEN
        WLIL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'ILP'.OR.WHCH(1:3).EQ.'ilp') THEN
        IPIL=MAX(-4,MIN(4,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'ILS'.OR.WHCH(1:3).EQ.'ils') THEN
        WCIL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'ILW'.OR.WHCH(1:3).EQ.'ilw') THEN
        WWIL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'ILX'.OR.WHCH(1:3).EQ.'ilx') THEN
        CXIL=RVAL
      ELSE IF (WHCH(1:3).EQ.'ILY'.OR.WHCH(1:3).EQ.'ily') THEN
        CYIL=RVAL
      ELSE IF (WHCH(1:3).EQ.'IWM'.OR.WHCH(1:3).EQ.'iwm') THEN
        LIWM=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'LBC'.OR.WHCH(1:3).EQ.'lbc') THEN
        ILBC=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'LIS'.OR.WHCH(1:3).EQ.'lis') THEN
        LINS=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'LIT'.OR.WHCH(1:3).EQ.'lit') THEN
        LINT(IPAI)=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'LLA'.OR.WHCH(1:3).EQ.'lla') THEN
        ANLL=RVAL
      ELSE IF (WHCH(1:3).EQ.'LLB'.OR.WHCH(1:3).EQ.'llb') THEN
        IBLL=MAX(0,MIN(3,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'LLC'.OR.WHCH(1:3).EQ.'llc') THEN
        ICLL(IPAI)=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'LLL'.OR.WHCH(1:3).EQ.'lll') THEN
        WLLL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'LLO'.OR.WHCH(1:3).EQ.'llo') THEN
        IOLL=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'LLP'.OR.WHCH(1:3).EQ.'llp') THEN
        IPLL=MAX(-3,MIN(3,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'LLS'.OR.WHCH(1:3).EQ.'lls') THEN
        WCLL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'LLW'.OR.WHCH(1:3).EQ.'llw') THEN
        WWLL=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'LOC'.OR.WHCH(1:3).EQ.'loc') THEN
        ICLO=MAX(-1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'MAP'.OR.WHCH(1:3).EQ.'map') THEN
        IMPF=MAX(0,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'NCL'.OR.WHCH(1:3).EQ.'ncl') THEN
        NCLV=INT(RVAL)
        IF (NCLV.LT.1.OR.NCLV.GT.256) THEN
          CALL SETER ('CTSETR - NCL LESS THAN 1 OR GREATER THAN 256',4,1
     +)
          RETURN
        END IF
      ELSE IF (WHCH(1:3).EQ.'NEL'.OR.WHCH(1:3).EQ.'nel') THEN
        NEXL=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'NET'.OR.WHCH(1:3).EQ.'net') THEN
        NEXT=MAX(0,MIN(2,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'NEU'.OR.WHCH(1:3).EQ.'neu') THEN
        NEXU=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'NLS'.OR.WHCH(1:3).EQ.'nls') THEN
        NLSD=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'NLZ'.OR.WHCH(1:3).EQ.'nlz') THEN
        NLZF=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'NOF'.OR.WHCH(1:3).EQ.'nof') THEN
        NOMF=MAX(0,MIN(7,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'NSD'.OR.WHCH(1:3).EQ.'nsd') THEN
        NSDL=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'NVS'.OR.WHCH(1:3).EQ.'nvs') THEN
        NOVS=MAX(0,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'ORV'.OR.WHCH(1:3).EQ.'orv') THEN
        OORV=RVAL
      ELSE IF (WHCH(1:3).EQ.'PAI'.OR.WHCH(1:3).EQ.'pai') THEN
        IPAI=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'PC1'.OR.WHCH(1:3).EQ.'pc1') THEN
        GSDM=RVAL
      ELSE IF (WHCH(1:3).EQ.'PC2'.OR.WHCH(1:3).EQ.'pc2') THEN
        FNCM=RVAL
      ELSE IF (WHCH(1:3).EQ.'PC3'.OR.WHCH(1:3).EQ.'pc3') THEN
        CDMX=RVAL
      ELSE IF (WHCH(1:3).EQ.'PC4'.OR.WHCH(1:3).EQ.'pc4') THEN
        DOPT=RVAL
      ELSE IF (WHCH(1:3).EQ.'PC5'.OR.WHCH(1:3).EQ.'pc5') THEN
        DFLD=RVAL
      ELSE IF (WHCH(1:3).EQ.'PC6'.OR.WHCH(1:3).EQ.'pc6') THEN
        DBLM=RVAL
      ELSE IF (WHCH(1:3).EQ.'PIC'.OR.WHCH(1:3).EQ.'pic') THEN
        IPIC=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'PIE'.OR.WHCH(1:3).EQ.'pie') THEN
        IPIE=INT(RVAL)
      ELSE IF (WHCH(1:3).EQ.'PIT'.OR.WHCH(1:3).EQ.'pit') THEN
        PITH=RVAL
      ELSE IF (WHCH(1:3).EQ.'PW1'.OR.WHCH(1:3).EQ.'pw1') THEN
        WTGR=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'PW2'.OR.WHCH(1:3).EQ.'pw2') THEN
        WTNC=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'PW3'.OR.WHCH(1:3).EQ.'pw3') THEN
        WTCD=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'PW4'.OR.WHCH(1:3).EQ.'pw4') THEN
        WTOD=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'RC1'.OR.WHCH(1:3).EQ.'rc1') THEN
        DBLF=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'RC2'.OR.WHCH(1:3).EQ.'rc2') THEN
        DBLN=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'RC3'.OR.WHCH(1:3).EQ.'rc3') THEN
        DBLV=MAX(0.,RVAL)
      ELSE IF (WHCH(1:3).EQ.'RWC'.OR.WHCH(1:3).EQ.'rwc') THEN
        LRWC=MAX(5,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'RWG'.OR.WHCH(1:3).EQ.'rwg') THEN
        LRWG=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'RWM'.OR.WHCH(1:3).EQ.'rwm') THEN
        LRWM=MAX(2,INT(RVAL))
      ELSE IF (WHCH(1:3).EQ.'SET'.OR.WHCH(1:3).EQ.'set') THEN
        ISET=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:3).EQ.'SFS'.OR.WHCH(1:3).EQ.'sfs'.OR.WHCH(1:3).EQ.
     +'SFU'.OR.WHCH(1:3).EQ.'sfu') THEN
        SCFS=RVAL
      ELSE IF (WHCH(1:3).EQ.'SSL'.OR.WHCH(1:3).EQ.'ssl') THEN
        SEGL=MAX(.0001,RVAL)
      ELSE IF (WHCH(1:3).EQ.'T2D'.OR.WHCH(1:3).EQ.'t2d') THEN
        T2DS=RVAL
      ELSE IF (WHCH(1:3).EQ.'VPB'.OR.WHCH(1:3).EQ.'vpb') THEN
        UVPB=MAX(0.,MIN(1.,RVAL))
      ELSE IF (WHCH(1:3).EQ.'VPL'.OR.WHCH(1:3).EQ.'vpl') THEN
        UVPL=MAX(0.,MIN(1.,RVAL))
      ELSE IF (WHCH(1:3).EQ.'VPR'.OR.WHCH(1:3).EQ.'vpr') THEN
        UVPR=MAX(0.,MIN(1.,RVAL))
      ELSE IF (WHCH(1:3).EQ.'VPS'.OR.WHCH(1:3).EQ.'vps') THEN
        UVPS=RVAL
      ELSE IF (WHCH(1:3).EQ.'VPT'.OR.WHCH(1:3).EQ.'vpt') THEN
        UVPT=MAX(0.,MIN(1.,RVAL))
      ELSE IF (WHCH(1:3).EQ.'WDB'.OR.WHCH(1:3).EQ.'wdb') THEN
        UWDB=RVAL
      ELSE IF (WHCH(1:3).EQ.'WDL'.OR.WHCH(1:3).EQ.'wdl') THEN
        UWDL=RVAL
      ELSE IF (WHCH(1:3).EQ.'WDR'.OR.WHCH(1:3).EQ.'wdr') THEN
        UWDR=RVAL
      ELSE IF (WHCH(1:3).EQ.'WDT'.OR.WHCH(1:3).EQ.'wdt') THEN
        UWDT=RVAL
      ELSE IF (WHCH(1:3).EQ.'WSO'.OR.WHCH(1:3).EQ.'wso') THEN
        IWSO=MAX(0,MIN(3,INT(RVAL)))
      ELSE
        CTMB(1:36)='CTSETR - PARAMETER NAME NOT KNOWN - '
        CTMB(37:39)=WHCH(1:3)
        CALL SETER (CTMB(1:39),5,1)
        RETURN
      END IF
C
C Done.
C
      RETURN
C
      END
