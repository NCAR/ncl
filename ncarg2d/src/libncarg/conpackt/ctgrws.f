C
C $Id: ctgrws.f,v 1.2 2004-03-19 22:51:55 kennison Exp $
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
      SUBROUTINE CTGRWS (RWRK,IOWS,LOWS,IERR)
C
      DIMENSION RWRK(*)
C
C This subroutine is called to get a block of space, of a specified
C size, in the user's real workspace array.  The block may or may not
C have been used before.
C
C IOWS is the index (into the arrays IRWS and LRWS) of the values
C saying where the block starts and how long it is.
C
C LOWS is the desired length.  The value 0 indicates that the maximum
C amount is desired; it will be replaced by the actual amount assigned.
C
C IERR is a returned error flag.  It will be 0 if no workspace overflow
C occurred, 1 if an overflow did occur.
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
C Check for argument error.
C
      IF (IOWS.LT.1.OR.IOWS.GT.4.OR.LOWS.LT.0) THEN
        CALL SETER ('CTGRWS - ARGUMENT ERROR - SEE SPECIALIST',1,1)
        RETURN
      END IF
C
C Clear error flag.
C
      IERR=0
C
C See if the desired amount of space is available.
C
      NLFT=LRWK
C
      DO 10001 I=1,4
        IF (I.NE.IOWS.AND.LRWS(I).GT.0) NLFT=NLFT-LRWS(I)
10001 CONTINUE
C
C If caller wants it all, arrange for that.
C
      IF (LOWS.LE.0) LOWS=NLFT
C
C Update the real-workspace-used parameter.
C
      IRWU=MAX(IRWU,LRWK-NLFT+LOWS)
C
C If too little space is available, take whatever action the user has
C specified.
C
      IF (NLFT.LT.LOWS) THEN
        IF (IWSO.LE.1)
     +    WRITE (I1MACH(4),'('' CTGRWS'',
     +                       I8,'' WORDS REQUESTED'',
     +                       I8,'' WORDS AVAILABLE'')') LOWS,NLFT
        IF (IWSO.LE.0) THEN
          CALL SETER ('CTGRWS - REAL WORKSPACE OVERFLOW',2,2)
          STOP
        ELSE IF (IWSO.GE.3) THEN
          CALL SETER ('CTGRWS - REAL WORKSPACE OVERFLOW',3,1)
        ELSE
          IERR=1
        END IF
        RETURN
      END IF
C
C It may be that a reduction in size has been requested.  That's easy.
C
      IF (LOWS.LE.LRWS(IOWS)) THEN
        LRWS(IOWS)=LOWS
        RETURN
      END IF
C
C Otherwise, what we do depends on whether the workspace associated
C with this index exists already.
C
      IF (LRWS(IOWS).LE.0) THEN
C
C It does not exist.  Find (or create) an area large enough.  First,
C check for an open space large enough.
C
        JRWS=0
10002   CONTINUE
          KRWS=LRWK
          IMIN=0
          DO 10003 I=1,4
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              KRWS=IRWS(I)
              IMIN=I
            END IF
10003     CONTINUE
          IF (KRWS-JRWS.GE.LOWS) THEN
            IRWS(IOWS)=JRWS
            LRWS(IOWS)=LOWS
            RETURN
          END IF
          IF (IMIN.NE.0) THEN
            JRWS=IRWS(IMIN)+LRWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0)) GO TO 10002
C
C If no space large enough was found, pack all the existing blocks
C into the beginning of the array, which will leave enough space at
C the end of it.
C
        JRWS=0
10004   CONTINUE
          KRWS=LRWK
          IMIN=0
          DO 10005 I=1,4
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              KRWS=IRWS(I)
              IMIN=I
            END IF
10005     CONTINUE
          IF (IMIN.NE.0) THEN
            IF (IRWS(IMIN).NE.JRWS) THEN
              DO 10006 I=1,LRWS(IMIN)
                RWRK(JRWS+I)=RWRK(IRWS(IMIN)+I)
10006         CONTINUE
              IRWS(IMIN)=JRWS
            END IF
            JRWS=JRWS+LRWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0)) GO TO 10004
C
        IRWS(IOWS)=JRWS
        LRWS(IOWS)=LOWS
        RETURN
C
      ELSE
C
C It exists.  Extend its length.  First, see if that can be done
C without moving anything around.
C
        JRWS=IRWS(IOWS)+LRWS(IOWS)
        KRWS=LRWK
        DO 10007 I=1,4
          IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) THEN
            KRWS=IRWS(I)
          END IF
10007   CONTINUE
        IF (KRWS-JRWS.GE.LOWS) THEN
          LRWS(IOWS)=LOWS
          RETURN
        END IF
C
C Blocks have to be moved.  Move those that precede the one to be
C lengthened and that one itself toward the beginning of the workspace.
C
        JRWS=0
10008   CONTINUE
          KRWS=LRWK
          IMIN=0
          DO 10009 I=1,4
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              KRWS=IRWS(I)
              IMIN=I
            END IF
10009     CONTINUE
          IF (IMIN.NE.0) THEN
            IF (IRWS(IMIN).NE.JRWS) THEN
              DO 10010 I=1,LRWS(IMIN)
                RWRK(JRWS+I)=RWRK(IRWS(IMIN)+I)
10010         CONTINUE
              IRWS(IMIN)=JRWS
            END IF
            JRWS=JRWS+LRWS(IMIN)
          END IF
        IF (.NOT.(IMIN.EQ.0.OR.IMIN.EQ.IOWS)) GO TO 10008
C
C Move blocks that follow the one to be lengthened toward the end of
C the workspace.
C
        KRWS=LRWK
10011   CONTINUE
          JRWS=IRWS(IOWS)+LRWS(IOWS)
          IMAX=0
          DO 10012 I=1,4
            IF (IRWS(I).GE.JRWS.AND.IRWS(I).LT.KRWS.AND.LRWS(I).GT.0) TH
     +EN
              JRWS=IRWS(I)+LRWS(I)
              IMAX=I
            END IF
10012     CONTINUE
          IF (IMAX.NE.0) THEN
            IF (JRWS.NE.KRWS) THEN
              DO 10013 I=LRWS(IMAX),1,-1
                RWRK(KRWS-LRWS(IMAX)+I)=RWRK(JRWS-LRWS(IMAX)+I)
10013         CONTINUE
              IRWS(IMAX)=KRWS-LRWS(IMAX)
            END IF
            KRWS=IRWS(IMAX)
          END IF
        IF (.NOT.(IMAX.EQ.0)) GO TO 10011
C
C There should now be room, so just update the length of the block.
C
        LRWS(IOWS)=LOWS
        RETURN
C
      END IF
C
      END
