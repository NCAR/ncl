C
C	$Id: cplbdr.f,v 1.1.1.1 1992-04-17 22:32:44 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CPLBDR (ZDAT,RWRK,IWRK)
C
      DIMENSION ZDAT(IZD1,*),RWRK(*),IWRK(*),IAMA(1)
C
C The function of the routine CPLBDR is to draw all of the contour-line
C labels.
C
C ZDAT is the user's data array.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C
C Declare all of the CONPACK common blocks.
C
C
C CPCOM1 contains integer and real variables.
C
      COMMON /CPCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CPCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CPCOM1/ CLDT(256),CLEV(256),CLWA(259),CXCF
      COMMON /CPCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DOPT
      COMMON /CPCOM1/ EPSI,FNCM,GRAV,GRSD,GSDM,HCHL,HCHS,IAIA(259)
      COMMON /CPCOM1/ IAIB(256),IBCF,IBHL,IBIL,IBLL,ICAF,ICCF
      COMMON /CPCOM1/ ICCL(259),ICFF,ICHI,ICHL,ICIL,ICLL(256)
      COMMON /CPCOM1/ ICLO,ICLP(256),ICLS,ICLU(259),ICLV,ICLW
      COMMON /CPCOM1/ IDUF,IGCL,IGLB,IGRM,IGRN,IGVS,IHCF,IHLX,IHLY
      COMMON /CPCOM1/ IIWS(2),IIWU,ILBC,IMPF,INCX(8),INCY(8)
      COMMON /CPCOM1/ INHL,INIL,INIT,INLL,IOCF,IOHL,IOLL,IPAI,IPCF
      COMMON /CPCOM1/ IPIC,IPIE,IPIL,IPLL,IRWS(4),IRWU,ISET,IWSO
      COMMON /CPCOM1/ IZD1,IZDM,IZDN,IZDS,JODP,JOMA,JOTZ,LCTM,LEA1
      COMMON /CPCOM1/ LEA2,LEA3,LEE1,LEE2,LEE3,LINS,LINT(10),LINU
      COMMON /CPCOM1/ LIWK,LIWM,LIWS(2),LNLG,LRWC,LRWG,LRWK
      COMMON /CPCOM1/ LRWM,LRWS(4),LSDD,LSDL,LSDM,LTCF,LTHI
      COMMON /CPCOM1/ LTIL,LTLO,MIRO,NCLB(256),NCLV,NDGL,NEXL
      COMMON /CPCOM1/ NEXT,NEXU,NLBS,NLSD,NLZF,NOMF,NOVS,NR04,NSDL
      COMMON /CPCOM1/ NSDR,OORV,SCFS,SCFU,SEGL,SVAL,T2DS,T3DS,UCMN
      COMMON /CPCOM1/ UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL,UWDR
      COMMON /CPCOM1/ UWDT,UXA1,UXAM,UYA1,UYAN,WCCF,WCHL,WCIL,WCLL
      COMMON /CPCOM1/ WLCF,WLHL,WLIL,WLLL,WOCH,WODA,WTCD,WTGR,WTNC
      COMMON /CPCOM1/ WTOD,WWCF,WWHL,WWIL,WWLL,XAT1,XATM,XLBC,XVPL
      COMMON /CPCOM1/ XVPR,XWDL,XWDR,YAT1,YATN,YLBC,YVPB,YVPT,YWDB
      COMMON /CPCOM1/ YWDT,ZDVL,ZMAX,ZMIN
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CPCOM1/
C
C CPCOM2 holds character parameters.
C
      COMMON /CPCOM2/ CHEX,CLBL(256),CLDP(259),CTMA,CTMB,FRMT
      COMMON /CPCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*40 CLBL
      CHARACTER*32 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*40 TXCF
      CHARACTER*20 TXHI
      CHARACTER*100 TXIL
      CHARACTER*20 TXLO
      SAVE   /CPCOM2/
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
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CPLBDR - INITIALIZATION CALL NOT DONE',1,2)
        STOP
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C If the constant-field flag is set, write the constant-field message
C and quit.
C
      IF (ICFF.NE.0) THEN
        CALL CPCFLB (1,RWRK,IWRK)
        RETURN
      END IF
C
C Make sure contour-label positions have been chosen.
C
      CALL CPPKLP (ZDAT,RWRK,IWRK)
C
C If there are still no labels in the label list, do the informational
C label and the high/low labels directly and quit.
C
      IF (NLBS.LE.0) THEN
        CALL CPINLB (ZDAT,RWRK,IWRK,2,IAMA)
        CALL CPHLLB (ZDAT,RWRK,IWRK,2,IAMA)
        RETURN
      END IF
C
C Redo the SET call so that we can use fractional-system coordinates.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XVPL,XVPR,YVPB,YVPT,1)
C
C Set up color-index controls.
C
      CALL GQPLCI (IGER,ISLC)
      IF (IGER.NE.0) THEN
        CALL SETER ('CPLBDR - ERROR EXIT FROM GQPLCI',2,2)
        STOP
      END IF
      CALL GQTXCI (IGER,ISTC)
      IF (IGER.NE.0) THEN
        CALL SETER ('CPLBDR - ERROR EXIT FROM GQTXCI',3,2)
        STOP
      END IF
      CALL GQFACI (IGER,ISFC)
      IF (IGER.NE.0) THEN
        CALL SETER ('CPLBDR - ERROR EXIT FROM GQFACI',4,2)
        STOP
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
          IF (RWRK(IR04-ICLB+1).EQ.0.) THEN
            ITYP=1
            ZDVL=0.
            CALL CPSBST (TXIL(1:LTIL),CTMA,LCTM)
            WCHR=(XVPR-XVPL)*CHWM*WCIL
            IBOX=IBIL
            JCOL=JCIL
            WDTH=WLIL
          ELSE IF (RWRK(IR04-ICLB+1).EQ.1.) THEN
            ITYP=2
            ZDVL=RWRK(IR04-ICLB+2)
            CALL CPSBST (TXHI(1:LTHI),CTMA,LCTM)
            WCHR=(XVPR-XVPL)*CHWM*WCHL
            IBOX=IBHL
            JCOL=JCHI
            WDTH=WLHL
          ELSE
            ITYP=3
            ZDVL=RWRK(IR04-ICLB+2)
            CALL CPSBST (TXLO(1:LTLO),CTMA,LCTM)
            WCHR=(XVPR-XVPL)*CHWM*WCHL
            IBOX=IBHL
            JCOL=JCLO
            WDTH=WLHL
          END IF
C
          IF (IBOX.NE.0) THEN
            DLLB=RWRK(IR04-ICLB+3)
            DRLB=RWRK(IR04-ICLB+4)
            DBLB=RWRK(IR04-ICLB+5)
            DTLB=RWRK(IR04-ICLB+6)
          END IF
C
        ELSE
C
          ITYP=4
          JCOL=ISTC
          IF (ICLL(ICLB).GE.0) JCOL=ICLL(ICLB)
C
          WCHR=(XVPR-XVPL)*CHWM*WCLL
C
          LCTM=NCLB(ICLB)
          CTMA(1:LCTM)=CLBL(ICLB)(1:LCTM)
C
          IBOX=IBLL
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
        IF (MOD(IBOX/2,2).NE.0) THEN
          IF (JSFC.NE.JLBC) THEN
            CALL GSFACI (JLBC)
            JSFC=JLBC
          END IF
          IF (ITYP.EQ.1) THEN
            CALL CPCHIL (+2)
          ELSE IF (ITYP.EQ.2) THEN
            CALL CPCHHL (+2)
          ELSE IF (ITYP.EQ.3) THEN
            CALL CPCHHL (+6)
          ELSE
            IPAI=ICLB
            CALL CPCHLL (+2)
          END IF
          IF (CTMA(1:LCTM).NE.' ') THEN
            BFXC(1)=XCLB-DLLB*CALB+DBLB*SALB
            BFYC(1)=YCLB-DLLB*SALB-DBLB*CALB
            BFXC(2)=XCLB+DRLB*CALB+DBLB*SALB
            BFYC(2)=YCLB+DRLB*SALB-DBLB*CALB
            BFXC(3)=XCLB+DRLB*CALB-DTLB*SALB
            BFYC(3)=YCLB+DRLB*SALB+DTLB*CALB
            BFXC(4)=XCLB-DLLB*CALB-DTLB*SALB
            BFYC(4)=YCLB-DLLB*SALB+DTLB*CALB
            CALL GFA (4,BFXC,BFYC)
          END IF
          IF (ITYP.EQ.1) THEN
            CALL CPCHIL (-2)
          ELSE IF (ITYP.EQ.2) THEN
            CALL CPCHHL (-2)
          ELSE IF (ITYP.EQ.3) THEN
            CALL CPCHHL (-6)
          ELSE
            IPAI=ICLB
            CALL CPCHLL (-2)
          END IF
        END IF
C
        IF (JSLC.NE.JCOL) THEN
          CALL PLOTIF (0.,0.,2)
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
          CALL GQCLIP (IGER,IGCF,DUMI)
          IF (IGER.NE.0) THEN
            CALL SETER ('CPLBDR - ERROR EXIT FROM GQCLIP',5,2)
            STOP
          END IF
          IF (IGCF.NE.0) THEN
            CALL PLOTIF (0.,0.,2)
            CALL GSCLIP (0)
          END IF
          CALL CPCHIL (+3)
        ELSE IF (ITYP.EQ.2) THEN
          CALL CPCHHL (+3)
        ELSE IF (ITYP.EQ.3) THEN
          CALL CPCHHL (+7)
        ELSE
          IPAI=ICLB
          CALL CPCHLL (+3)
        END IF
        IF (CTMA(1:LCTM).NE.' ') THEN
          CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCHR,ANGD,0.)
        END IF
        IF (ITYP.EQ.1) THEN
          CALL CPCHIL (-3)
          IF (IGCF.NE.0) THEN
            CALL PLOTIF (0.,0.,2)
            CALL GSCLIP (IGCF)
          END IF
        ELSE IF (ITYP.EQ.2) THEN
          CALL CPCHHL (-3)
        ELSE IF (ITYP.EQ.3) THEN
          CALL CPCHHL (-7)
        ELSE
          IPAI=ICLB
          CALL CPCHLL (-3)
        END IF
C
        IF (MOD(IBOX,2).NE.0) THEN
          IF (WDTH.GT.0.) THEN
            CALL GQLWSC (IGER,SFLW)
            IF (IGER.NE.0) THEN
              CALL SETER ('CPLBDR - ERROR EXIT FROM GQLWSC',6,2)
              STOP
            END IF
            CALL PLOTIF (0.,0.,2)
            CALL GSLWSC (WDTH)
          END IF
          IF (ITYP.EQ.1) THEN
            CALL CPCHIL (+4)
          ELSE IF (ITYP.EQ.2) THEN
            CALL CPCHHL (+4)
          ELSE IF (ITYP.EQ.3) THEN
            CALL CPCHHL (+8)
          ELSE
            IPAI=ICLB
            CALL CPCHLL (+4)
          END IF
          IF (CTMA(1:LCTM).NE.' ') THEN
            CALL PLOTIF (XCLB-DLLB*CALB+DBLB*SALB,
     +                   YCLB-DLLB*SALB-DBLB*CALB,0)
            CALL PLOTIF (XCLB+DRLB*CALB+DBLB*SALB,
     +                   YCLB+DRLB*SALB-DBLB*CALB,1)
            CALL PLOTIF (XCLB+DRLB*CALB-DTLB*SALB,
     +                   YCLB+DRLB*SALB+DTLB*CALB,1)
            CALL PLOTIF (XCLB-DLLB*CALB-DTLB*SALB,
     +                   YCLB-DLLB*SALB+DTLB*CALB,1)
            CALL PLOTIF (XCLB-DLLB*CALB+DBLB*SALB,
     +                   YCLB-DLLB*SALB-DBLB*CALB,1)
            CALL PLOTIF (0.,0.,2)
          END IF
          IF (ITYP.EQ.1) THEN
            CALL CPCHIL (-4)
          ELSE IF (ITYP.EQ.2) THEN
            CALL CPCHHL (-4)
          ELSE IF (ITYP.EQ.3) THEN
            CALL CPCHHL (-8)
          ELSE
            IPAI=ICLB
            CALL CPCHLL (-4)
          END IF
          IF (WDTH.GT.0.) THEN
            CALL PLOTIF (0.,0.,2)
            CALL GSLWSC (SFLW)
          END IF
        END IF
C
10001 CONTINUE
C
C Return the color indices to their original values.
C
      IF (JSLC.NE.ISLC) THEN
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (ISLC)
      END IF
      IF (JSTC.NE.ISTC) CALL GSTXCI (ISTC)
      IF (JSFC.NE.ISFC) CALL GSFACI (ISFC)
C
C Restore the original SET parameters.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C Done.
C
      RETURN
C
      END
