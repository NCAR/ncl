C
C $Id: ctclam.f,v 1.1 2003-05-28 15:44:28 kennison Exp $
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
      SUBROUTINE CTCLAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*),IAMA(*)
C
C This routine adds contour lines to an area map.
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
C IAMA is the user's area map.
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
C Declare the common block that holds the clipping-window parameters
C for the routine CTWLAM.
C
      COMMON /CTWCMN/ XWMN,XWMX,YWMN,YWMX
C
C Define a couple of little workspace arrays required by CTTROE.
C
      DIMENSION RWKL(12),RWKR(12)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTCLAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTCLAM - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTCLAM',3).NE.0) RETURN
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
        IF (ICFELL('CTCLAM',4).NE.0) RETURN
      END IF
C
C Get indices for the contour levels in ascending order.
C
      IF (NCLV.GT.0) CALL CTSORT (CLEV,NCLV,ICLP)
C
C Get a little real workspace to use and re-do the call to SET so that
C we can use fractional coordinates.
C
      CALL CTGRWS (RWRK,1,10,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CTCLAM',5).NE.0) RETURN
      CALL SET (XVPL,XVPR,YVPB,YVPT,XVPL,XVPR,YVPB,YVPT,1)
      IF (ICFELL('CTCLAM',6).NE.0) RETURN
C
C Add the viewport perimeter to the area map.  This avoids problems
C which arise when mapping is turned on and the mapping function has
C a discontinuity (as, for example, a cylindrical equidistant EZMAP
C projection does).  This used to be done only when the mapping flag
C was turned on, but now it is done unconditionally, so as to force
C an area identifier of "-1" outside the viewport.  The area identifier
C on the inside of the viewport is set to zero, rather than to a value
C associated with a contour level.
C
      RWRK(IR01+ 1)=XVPL
      RWRK(IR01+ 2)=XVPR
      RWRK(IR01+ 3)=XVPR
      RWRK(IR01+ 4)=XVPL
      RWRK(IR01+ 5)=XVPL
      RWRK(IR01+ 6)=YVPB
      RWRK(IR01+ 7)=YVPB
      RWRK(IR01+ 8)=YVPT
      RWRK(IR01+ 9)=YVPT
      RWRK(IR01+10)=YVPB
C
      CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGCL,0,-1)
      IF (ICFELL('CTCLAM',7).NE.0) RETURN
C
C If it is to be done, put into the area map edges creating a set of
C vertical strips.
C
      IF (NOVS.NE.0) THEN
        CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+6),5,IGVS,0,-1)
        IF (ICFELL('CTCLAM',8).NE.0) RETURN
        DO 10001 IOVS=1,NOVS-1
          RWRK(IR01+1)=XVPL+REAL(IOVS)*(XVPR-XVPL)/REAL(NOVS)
          RWRK(IR01+2)=RWRK(IR01+1)
          CALL AREDAM (IAMA,RWRK(IR01+1),RWRK(IR01+9),2,IGVS,0,0)
          IF (ICFELL('CTCLAM',9).NE.0) RETURN
10001   CONTINUE
      END IF
C
C Discard the real workspace used above and re-call SET.
C
      LR01=0
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTCLAM',10).NE.0) RETURN
C
C Put edges of areas which are invisible into the area map.  This one
C is done first because the area-identifier information on the visible
C side is not as good as that provided by the other edges.  Of course,
C it is only done if the mapping flag is turned on and there is the
C possibility that some points are invisible under the mapping.
C
      IF (IMPF.NE.0.AND.OORV.NE.0.) THEN
C
        XWMN=XVPL
        XWMX=XVPR
        YWMN=YVPB
        YWMX=YVPT
C
        IJMP=0
        IAIC=0
C
10002   CONTINUE
          CALL CTTRVE (RPNT,IEDG,ITRI,RWRK,IWRK,IJMP,IRW1,IRW2,NRWK)
          IF (ICFELL('CTCLAM',15).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10003
          DO 10004 I=1,NRWK
            RWRK(IRW1+I)=CUFX(RWRK(IRW1+I))
            IF (ICFELL('CTCLAM',16).NE.0) RETURN
            RWRK(IRW2+I)=CUFY(RWRK(IRW2+I))
            IF (ICFELL('CTCLAM',17).NE.0) RETURN
10004     CONTINUE
          CALL CTTROE (RWRK(IRW1+1),RWRK(IRW2+1),NRWK,+.0005,RWKL,
     +                           IOCF,IAMA,IGCL,IAIA(258),IAIC)
          IF (ICFELL('CTCLAM',18).NE.0) RETURN
          CALL CTTROE (RWRK(IRW1+1),RWRK(IRW2+1),NRWK,-.0005,RWKR,
     +                           IOCF,IAMA,IGCL,IAIA(258),IAIC)
          IF (ICFELL('CTCLAM',19).NE.0) RETURN
        GO TO 10002
10003   CONTINUE
C
      END IF
C
C Add the edge of the grid.
C
      IJMP=0
      IAIC=0
C
10005 CONTINUE
        CALL CTTREG (RPNT,IEDG,ITRI,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,
     +                                                       NRWK)
        IF (ICFELL('CTCLAM',20).NE.0) RETURN
        IF (IJMP.EQ.0) GO TO 10006
        IF (MIRO.EQ.0) THEN
          CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIC,IAIA(257))
          IF (ICFELL('CTCLAM',21).NE.0) RETURN
        ELSE
          CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,IGCL,
     +                                              IAIA(257),IAIC)
          IF (ICFELL('CTCLAM',22).NE.0) RETURN
        END IF
      GO TO 10005
10006 CONTINUE
C
C If the constant-field flag is not set, add the selected contour lines
C to the area map.
C
      CLVP=0.
C
      IF (ICFF.EQ.0) THEN
C
          I = 1
          GO TO 10009
10007     CONTINUE
          I =I +1
10009     CONTINUE
          IF (I .GT.(NCLV)) GO TO 10008
C
          ICLV=ICLP(I)
C
          IF (I.EQ.1.OR.CLEV(ICLV).NE.CLVP) THEN
C
            CLVP=CLEV(ICLV)
C
            IF (CLEV(ICLV).GT.DMIN.AND.CLEV(ICLV).LT.DMAX) THEN
C
              JAIA=IAIA(ICLV)
              JAIB=IAIB(ICLV)
C
              DO 10010 J=I+1,NCLV
                JCLV=ICLP(J)
                IF (CLEV(JCLV).NE.CLEV(ICLV)) GO TO 101
                IF (IAIA(JCLV).NE.0) THEN
                  IF (JAIA.NE.0.AND.JAIA.NE.IAIA(JCLV)) THEN
                    CALL SETER ('CTCLAM - CONTRADICTORY AREA-IDENTIFIER
     +INFORMATION',26,1)
                    RETURN
                  END IF
                  JAIA=IAIA(JCLV)
                END IF
                IF (IAIB(JCLV).NE.0) THEN
                  IF (JAIB.NE.0.AND.JAIB.NE.IAIB(JCLV)) THEN
                    CALL SETER ('CTCLAM - CONTRADICTORY AREA-IDENTIFIER
     +INFORMATION',27,1)
                    RETURN
                  END IF
                  JAIB=IAIB(JCLV)
                END IF
10010         CONTINUE
C
  101         IF (JAIA.NE.0.OR.JAIB.NE.0) THEN
C
                IJMP=0
C
10011           CONTINUE
                  CALL CTTRCL (RPNT,IEDG,ITRI,RWRK,IWRK,CLEV(ICLV),
     +                                         IJMP,IRW1,IRW2,NRWK)
                  IF (ICFELL('CTCLAM',28).NE.0) RETURN
                  IF (IJMP.EQ.0) GO TO 10012
                  IF (MIRO.EQ.0) THEN
                    CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                                                 IGCL,JAIB,JAIA)
                    IF (ICFELL('CTCLAM',29).NE.0) RETURN
                  ELSE
                    CALL AREDAM (IAMA,RWRK(IRW1+1),RWRK(IRW2+1),NRWK,
     +                                                 IGCL,JAIA,JAIB)
                    IF (ICFELL('CTCLAM',30).NE.0) RETURN
                  END IF
                GO TO 10011
10012           CONTINUE
C
              END IF
C
            END IF
C
          END IF
C
        GO TO 10007
10008   CONTINUE
C
      END IF
C
C Done.
C
      RETURN
C
      END
