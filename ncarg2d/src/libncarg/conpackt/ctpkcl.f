C
C $Id: ctpkcl.f,v 1.1 2003-05-28 15:44:32 kennison Exp $
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
      SUBROUTINE CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C The routine CTPKCL is called to pick a set of contour levels.
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
      IF (ICFELL('CTPKCL - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTPKCL - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C If contour level selection is suppressed, do nothing.
C
      IF (ICLS.EQ.0) RETURN
C
C If the constant-field flag is set, do nothing.
C
      IF (ICFF.NE.0) RETURN
C
C If the contour-selection flag is negative and is equal to "-n",
C generate "n" contour levels, equally spaced between the minimum and
C the maximum.  By default, none of these levels will be labelled.
C
      IF (ICLS.LT.0) THEN
C
        IF (-ICLS.GT.256) THEN
          CALL SETER ('CTPKCL - TOO MANY CONTOUR LEVELS',3,1)
          RETURN
        END IF
C
        NCLV=MIN(-ICLS,256)
        CINU=(DMAX-DMIN)/REAL(NCLV+1)
        LINU=0
C
        DO 10001 I=1,NCLV
          CLEV(I)=DMIN+REAL(I)*CINU
          ICLU(I)=1
          IAIA(I)=I+1
          IAIB(I)=I
          CLBL(I)=' '
          NCLB(I)=-1
          CLDP(I)='$$$$$$$$$$$$$$$$'
          ICCL(I)=-1
          ICLL(I)=-1
          CLWA(I)=0.
10001   CONTINUE
C
C Otherwise (if the contour-selection flag is positive), generate the
C contour levels at equal intervals, either as specified by the user
C or as chosen in order to get roughly the right number of levels.
C Certain levels will be labelled.
C
      ELSE
C
        IF (CINS.LE.0.) THEN
          CINU=0.
        ELSE
          CINU=CINS
          LINU=LINS
          IF (UCMN.LE.UCMX) THEN
            NCLV=0
10002       CONTINUE
              NCLV=NCLV+1
              CLEV(NCLV)=UCMN+REAL(NCLV-1)*CINU
              IF (ABS(CLEV(NCLV)).LT..001*CINU) CLEV(NCLV)=0.
              IF (MOD(NCLV-1,LINU).NE.0) THEN
                ICLU(NCLV)=1
              ELSE
                ICLU(NCLV)=3
              END IF
              IAIA(NCLV)=NCLV+1
              IAIB(NCLV)=NCLV
              CLBL(NCLV)=' '
              NCLB(NCLV)=-1
              CLDP(NCLV)='$$$$$$$$$$$$$$$$'
              ICCL(NCLV)=-1
              ICLL(NCLV)=-1
              CLWA(NCLV)=0.
              IF (NCLV.EQ.256.OR.CLEV(NCLV)+.999*CINU.GT.UCMX) GO TO 100
     +03
            GO TO 10002
10003       CONTINUE
            GO TO 101
          END IF
        END IF
C
        IF (CINU.EQ.0.) THEN
          CINU=(DMAX-DMIN)/REAL(ICLS)
          LINU=1
          ITMP=INT(10000.+ALOG10(CINU))-10000
          CINU=CINU/10.**ITMP
          IF (CINU.LT.1.) THEN
            ITMP=ITMP-1
            CINU=CINU*10.
          ELSE IF (CINU.GE.10.) THEN
            ITMP=ITMP+1
            CINU=CINU/10.
          END IF
          IINV=0
          DO 10004 I=1,10
            IF (CINT(I).NE.0..AND.CINT(I).LE.CINU) IINV=I
10004     CONTINUE
          IF (IINV.NE.0) THEN
            CINU=CINT(IINV)
            LINU=LINT(IINV)
          END IF
          IF (ITMP.LT.0) THEN
            CINU=CINU*(1./10.**(-ITMP))
          ELSE IF (ITMP.GT.0) THEN
            CINU=CINU*10.**ITMP
          END IF
        END IF
        NCLV=0
        RTM2=DMIN+.001*(DMAX-DMIN)
        IF (RTM2.LT.0.) THEN
          RTM1=-AINT(-RTM2/CINU)
        ELSE
          RTM1=1.+AINT(RTM2/CINU)
        END IF
        RTM2=DMAX-.001*(DMAX-DMIN)
10005   CONTINUE
        IF (.NOT.(NCLV.LT.256.AND.RTM1*CINU.LT.RTM2)) GO TO 10006
          NCLV=NCLV+1
          CLEV(NCLV)=RTM1*CINU
          IF (MOD(RTM1,REAL(LINU)).NE.0) THEN
            ICLU(NCLV)=1
          ELSE
            ICLU(NCLV)=3
          END IF
          IAIA(NCLV)=NCLV+1
          IAIB(NCLV)=NCLV
          CLBL(NCLV)=' '
          NCLB(NCLV)=-1
          CLDP(NCLV)='$$$$$$$$$$$$$$$$'
          ICCL(NCLV)=-1
          ICLL(NCLV)=-1
          CLWA(NCLV)=0.
          RTM1=RTM1+1.
        GO TO 10005
10006   CONTINUE
      END IF
C
C Done.
C
  101 RETURN
C
      END
