C
C $Id: cthchm.f,v 1.1 2003-05-28 15:44:30 kennison Exp $
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
      SUBROUTINE CTHCHM (RWRK,IPTX,IPTY,NXYC,IAMA,IWRK,RTPL)
C
      DIMENSION RWRK(*),IAMA(*),IWRK(*)
C
C (RWRK(I),I=IPTX+1,IPTX+NXYC) and (RWRK(I),I=IPTY+1,IPTY+NXYC) contain
C the X and Y coordinates (in the user coordinate system) of points
C that define a segment of a contour line.  The function of the routine
C CTHCHM is to hachure the segment, if appropriate, masking the hachures
C against the area map in IAMA in the manner determined by the user
C routine RTPL.
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
C Define arrays to hold the endpoints of hachures to be masked against
C the area map.
C
      DIMENSION XCPT(2),YCPT(2)
C
C Define required constants.
C
      DATA DTOR / .017453292519943 /
      DATA RTOD / 57.2957795130823 /
C
C Determine whether or not hachuring is to be done for this segment.
C
      IF (ABS(IHCF).GT.1) THEN
C
        IF (RWRK(IPTX+NXYC).NE.RWRK(IPTX+1).OR.RWRK(IPTY+NXYC).NE.RWRK(I
     +PTY+1)) THEN
          IF (IOCF.NE.0) GO TO 101
          IF (ABS(IHCF).EQ.2) GO TO 101
          IF (ABS(IHCF).EQ.3) RETURN
          ANGN=RTOD*ARRAT2(RWRK(IPTY+2)-RWRK(IPTY+1),
     +                     RWRK(IPTX+2)-RWRK(IPTX+1))
        ELSE
C
          ANGN=RTOD*ARRAT2(RWRK(IPTY+NXYC)-RWRK(IPTY+NXYC-1),
     +                     RWRK(IPTX+NXYC)-RWRK(IPTX+NXYC-1))
        END IF
C
        ANGT=0.
C
        DO 10001 I=1,NXYC-1
          ANGO=ANGN
          ANGN=RTOD*ARRAT2(RWRK(IPTY+I+1)-RWRK(IPTY+I),
     +                     RWRK(IPTX+I+1)-RWRK(IPTX+I))
          IF (ABS(ANGN-ANGO).GT.180.) ANGO=ANGO+SIGN(360.,ANGN-ANGO)
          ANGT=ANGT+ANGN-ANGO
10001   CONTINUE
C
        IF ((MIRO.EQ.0.AND.ANGT.LT.0.).OR.(MIRO.NE.0.AND.ANGT.GT.0.)) TH
     +EN
          IF (IHCF.GT.0) RETURN
        ELSE
          IF (IHCF.LT.0) RETURN
        END IF
C
      END IF
C
C Convert all the X and Y coordinates to the fractional system.
C
  101 CONTINUE
      DO 10002 I=1,NXYC
        RWRK(IPTX+I)=CUFX(RWRK(IPTX+I))
        IF (ICFELL('CTHCHM',1).NE.0) RETURN
        RWRK(IPTY+I)=CUFY(RWRK(IPTY+I))
        IF (ICFELL('CTHCHM',2).NE.0) RETURN
10002 CONTINUE
C
C Compute the total length of the polyline.
C
      TLEN=0.
C
      DO 10003 I=1,NXYC-1
        TLEN=TLEN+SQRT((RWRK(IPTX+I+1)-RWRK(IPTX+I))**2+
     +                 (RWRK(IPTY+I+1)-RWRK(IPTY+I))**2)
10003 CONTINUE
C
C Decide how long the hachures ought to be and on which side of the
C polyline they ought to go.
C
      HCHD=HCHL*(XVPR-XVPL)
      IF (MIRO.NE.0) HCHD=-HCHD
      IF ((XWDL.LT.XWDR.AND.YWDB.GT.YWDT).OR.
     +    (XWDL.GT.XWDR.AND.YWDB.LT.YWDT)) HCHD=-HCHD
C
C Draw hachures along the polyline.
C
      TEMP=REAL(INT(TLEN/(HCHS*(XVPR-XVPL))))
      IF (TEMP.LE.0.) RETURN
      DBHM=TLEN/TEMP
      PNHM=DBHM/2.
C
      I=0
      CLEN=0.
C
10004 CONTINUE
      IF (.NOT.(I.LT.NXYC-1)) GO TO 10005
        I=I+1
        SLEN=SQRT((RWRK(IPTX+I+1)-RWRK(IPTX+I))**2+
     +            (RWRK(IPTY+I+1)-RWRK(IPTY+I))**2)
10006   CONTINUE
        IF (.NOT.(PNHM.LT.CLEN+SLEN)) GO TO 10007
          FRCT=(PNHM-CLEN)/SLEN
          XCPT(1)=RWRK(IPTX+I)+FRCT*(RWRK(IPTX+I+1)-RWRK(IPTX+I))
          YCPT(1)=RWRK(IPTY+I)+FRCT*(RWRK(IPTY+I+1)-RWRK(IPTY+I))
          XCPT(2)=XCPT(1)-HCHD*(RWRK(IPTY+I+1)-RWRK(IPTY+I))/SLEN
          YCPT(2)=YCPT(1)+HCHD*(RWRK(IPTX+I+1)-RWRK(IPTX+I))/SLEN
          XCPT(1)=CFUX(XCPT(1))
          IF (ICFELL('CTHCHM',3).NE.0) RETURN
          YCPT(1)=CFUY(YCPT(1))
          IF (ICFELL('CTHCHM',4).NE.0) RETURN
          XCPT(2)=CFUX(XCPT(2))
          IF (ICFELL('CTHCHM',5).NE.0) RETURN
          YCPT(2)=CFUY(YCPT(2))
          IF (ICFELL('CTHCHM',6).NE.0) RETURN
          CALL ARDRLN (IAMA,XCPT,YCPT,2,
     +                 RWRK(IR02+1),RWRK(IR02+1+LRWM),LRWM,
     +                 IWRK(II02+1),IWRK(II02+1+LIWM),LIWM,RTPL)
          IF (ICFELL('CTHCHM',7).NE.0) RETURN
          PNHM=PNHM+DBHM
        GO TO 10006
10007   CONTINUE
        CLEN=CLEN+SLEN
      GO TO 10004
10005 CONTINUE
C
C Done.
C
      RETURN
C
      END
