C
C $Id: ctcldr.f,v 1.2 2004-03-19 22:51:54 kennison Exp $
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
      SUBROUTINE CTCLDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C This routine draws contour lines.
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
C Declare the dash-package common block which contains the smoothing
C flag, so that it may be temporarily turned off as needed.
C
      COMMON /SMFLAG/ ISMO
C
C Declare local variables in which to manipulate DASHPACK parameters.
C
      CHARACTER*1 CHRB,CHRG,CHRS
      CHARACTER*16 CDPS
      CHARACTER*256 CHDP
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTCLDR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTCLDR - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Do the proper SET call.
C
      CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
      IF (ICFELL('CTCLDR',3).NE.0) RETURN
C
C If no contour levels are defined, try to pick a set of levels.
C
      IF (NCLV.LE.0) THEN
        CALL CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
        IF (ICFELL('CTCLDR',4).NE.0) RETURN
      END IF
C
C Get indices for the contour levels in ascending order.
C
      IF (NCLV.GT.0) CALL CTSORT (CLEV,NCLV,ICLP)
C
C Initialize whichever dash package (if any) is to be used.
C
      IF (IDUF.LT.0) THEN
C
        CALL DPGETC ('CRB',CHRB)
        IF (ICFELL('CTCLDR',5).NE.0) RETURN
        CALL DPGETC ('CRG',CHRG)
        IF (ICFELL('CTCLDR',6).NE.0) RETURN
        CALL DPGETC ('CRS',CHRS)
        IF (ICFELL('CTCLDR',7).NE.0) RETURN
        CALL DPGETI ('DPL',IDPL)
        IF (ICFELL('CTCLDR',8).NE.0) RETURN
        CALL DPGETI ('DPS',IDPS)
        IF (ICFELL('CTCLDR',9).NE.0) RETURN
        CALL DPGETC ('DPT',CHDP)
        IF (ICFELL('CTCLDR',10).NE.0) RETURN
        CALL DPGETR ('TCS',RTCS)
        IF (ICFELL('CTCLDR',11).NE.0) RETURN
        CALL DPGETR ('WOC',RWOC)
        IF (ICFELL('CTCLDR',12).NE.0) RETURN
        CALL DPGETR ('WOG',RWOG)
        IF (ICFELL('CTCLDR',13).NE.0) RETURN
        CALL DPGETR ('WOS',RWOS)
        IF (ICFELL('CTCLDR',14).NE.0) RETURN
C
        CALL DPSETI ('DPS',0)
        IF (ICFELL('CTCLDR',15).NE.0) RETURN
        CDPS=CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//
     +       CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS//CHRS
        CALL DPSETC ('DPT',CDPS)
        IF (ICFELL('CTCLDR',16).NE.0) RETURN
        CALL DPSETR ('TCS',-1.)
        IF (ICFELL('CTCLDR',17).NE.0) RETURN
        CALL DPSETR ('WOC',CHWM*WOCH*(XVPR-XVPL))
        IF (ICFELL('CTCLDR',18).NE.0) RETURN
        CALL DPSETR ('WOG',CHWM*WODA*(XVPR-XVPL))
        IF (ICFELL('CTCLDR',19).NE.0) RETURN
        CALL DPSETR ('WOS',CHWM*WODA*(XVPR-XVPL))
        IF (ICFELL('CTCLDR',20).NE.0) RETURN
C
      ELSE IF (IDUF.GT.0) THEN
C
        CALL GETSI (IP2X,IP2Y)
        IF (ICFELL('CTCLDR',21).NE.0) RETURN
        ILDA=MAX(1,INT(CHWM*WODA*(XVPR-XVPL)*(2.**IP2X-1.)+.5))
        ILCH=MAX(4,INT(CHWM*WOCH*(XVPR-XVPL)*(2.**IP2X-1.)+.5))
        CALL DASHDC ('$$$$$$$$$$$$$$$$',ILDA,ILCH)
        IF (ICFELL('CTCLDR',22).NE.0) RETURN
        ISMS=ISMO
        ISMO=1
C
      END IF
C
C If the constant-field flag is set, just output a warning message.
C
      IF (.NOT.(ICFF.NE.0)) GO TO 10001
C
        CALL CTCFLB (1,RWRK,IWRK)
        IF (ICFELL('CTCLDR',23).NE.0) RETURN
C
C Otherwise, draw contours.
C
      GO TO 10002
10001 CONTINUE
C
C If labels are being written by the dash package, make sure the labels
C are completely defined.
C
        IF (.NOT.(ABS(IPLL).EQ.1)) GO TO 10003
          CALL CTPKLB (RPNT,IEDG,ITRI,RWRK,IWRK)
          IF (ICFELL('CTCLDR',24).NE.0) RETURN
          CALL CTSTLS (RPNT,IEDG,ITRI,RWRK,IWRK)
          IF (ICFELL('CTCLDR',25).NE.0) RETURN
10003   CONTINUE
C
C Loop through the selected contour levels, drawing contour lines for
C the appropriate ones.
C
          ICLV = 1
          GO TO 10006
10004     CONTINUE
          ICLV =ICLV +1
10006     CONTINUE
          IF (ICLV .GT.(NCLV)) GO TO 10005
C
          IF (.NOT.(CLEV(ICLV).GT.DMIN.AND.CLEV(ICLV).LT.DMAX)) GO TO 10
     +007
C
C If dash patterns are in use, find the length of the dash pattern at
C this contour level.
C
            IF (.NOT.(IDUF.NE.0)) GO TO 10008
              L10010=    1
              GO TO 10010
10009         CONTINUE
10008       CONTINUE
C
C If only the line is being drawn, the dash-pattern-use flag determines
C whether it will be done using CURVE, DPCURV, or CURVED.
C
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.1)) GO TO 10011
C
              IF (.NOT.(IDUF.LT.0)) GO TO 10012
                CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
                IF (ICFELL('CTCLDR',26).NE.0) RETURN
              GO TO 10013
10012         CONTINUE
              IF (.NOT.(IDUF.GT.0)) GO TO 10014
                CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
                IF (ICFELL('CTCLDR',27).NE.0) RETURN
10013         CONTINUE
10014         CONTINUE
C
              L10016=    1
              GO TO 10016
10015         CONTINUE
C
C If only the labels are being drawn, it can be handled here only if
C the dash-pattern use flag indicates that DPCURV or CURVED is to be
C used and the label-positioning flag implies that the labels are to
C be incorporated into the dash pattern.
C
            GO TO 10017
10011       CONTINUE
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.2)) GO TO 10018
C
              IF (.NOT.(ABS(IPLL).EQ.1.AND.IDUF.NE.0)) GO TO 10019
                NCHL=NCLB(ICLV)
                NCHD=MAX(1,MIN(ABS(IDUF)*LCLD,500-NCHL))
                CTMA=' '
                IF (.NOT.(IDUF.LT.0)) GO TO 10020
                  DO 10021 ICHD=1,NCHD
                    CTMA(ICHD:ICHD)=CHRG
10021             CONTINUE
                  LCTM=NCHD+NCHL
                  CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                  CALL DPSETC ('DPT',CTMA(1:LCTM))
                  IF (ICFELL('CTCLDR',28).NE.0) RETURN
                GO TO 10022
10020           CONTINUE
                IF (.NOT.(IDUF.GT.0)) GO TO 10023
                  DO 10024 ICHD=1,NCHD
                    CTMA(ICHD:ICHD)=''''
10024             CONTINUE
                  LCTM=NCHD+NCHL
                  CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                  CALL DASHDC (CTMA(1:LCTM),ILDA,ILCH)
                  IF (ICFELL('CTCLDR',29).NE.0) RETURN
10022           CONTINUE
10023           CONTINUE
                L10016=    2
                GO TO 10016
10025           CONTINUE
10019         CONTINUE
C
C If both lines and labels are being drawn, there are various cases,
C depending on whether dashed lines are being used and how labels are
C being positioned.
C
            GO TO 10017
10018       CONTINUE
            IF (.NOT.(MOD(ICLU(ICLV),4).EQ.3)) GO TO 10026
C
              IF (.NOT.(IDUF.NE.0)) GO TO 10027
                IF (.NOT.(ABS(IPLL).EQ.1)) GO TO 10028
                  NCHL=NCLB(ICLV)
                  NCHD=MAX(1,MIN(ABS(IDUF)*LCLD,500-NCHL))
                  CTMA=' '
                  DO 10029 ICHD=1,NCHD
                    JCHD=MOD(ICHD-1,LCLD)+1
                    CTMA(ICHD:ICHD)=CLDP(ICLV)(JCHD:JCHD)
10029             CONTINUE
                  IF (.NOT.(IDUF.LT.0)) GO TO 10030
                    LCTM=NCHD+NCHL
                    CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                    CALL DPSETC ('DPT',CTMA(1:LCTM))
                    IF (ICFELL('CTCLDR',30).NE.0) RETURN
                  GO TO 10031
10030             CONTINUE
                    LCTM=NCHD+NCHL
                    CTMA(NCHD+1:LCTM)=CLBL(ICLV)(1:NCHL)
                    CALL DASHDC (CTMA(1:LCTM),ILDA,ILCH)
                    IF (ICFELL('CTCLDR',31).NE.0) RETURN
10031             CONTINUE
                GO TO 10032
10028           CONTINUE
                  IF (.NOT.(IDUF.LT.0)) GO TO 10033
                    CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
                    IF (ICFELL('CTCLDR',32).NE.0) RETURN
                  GO TO 10034
10033             CONTINUE
                    CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
                    IF (ICFELL('CTCLDR',33).NE.0) RETURN
10034             CONTINUE
10032           CONTINUE
10027         CONTINUE
C
              L10016=    3
              GO TO 10016
10035         CONTINUE
C
10017       CONTINUE
10026       CONTINUE
C
10007     CONTINUE
C
        GO TO 10004
10005   CONTINUE
C
10002 CONTINUE
C
C Draw boundaries for areas that are invisible.
C
      IF (.NOT.(ICLU(258).NE.0.AND.IMPF.NE.0.AND.OORV.NE.0.))
     +GO TO 10036
        ICLV=258
        IF (.NOT.(IDUF.NE.0)) GO TO 10037
          L10010=    2
          GO TO 10010
10038     CONTINUE
          IF (.NOT.(IDUF.LT.0)) GO TO 10039
            CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
            IF (ICFELL('CTCLDR',39).NE.0) RETURN
          GO TO 10040
10039     CONTINUE
            CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
            IF (ICFELL('CTCLDR',40).NE.0) RETURN
10040     CONTINUE
10037   CONTINUE
        L10042=    1
        GO TO 10042
10041   CONTINUE
        IJMP=0
10043   CONTINUE
          CALL CTTRVE (RPNT,IEDG,ITRI,RWRK,IWRK,IJMP,IRW1,IRW2,NRWK)
          IF (ICFELL('CTCLDR',42).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10044
          CALL CTDRSG (RWRK,IRW1,IRW2,NRWK)
          IF (ICFELL('CTCLDR',43).NE.0) RETURN
        GO TO 10043
10044   CONTINUE
        L10046=    1
        GO TO 10046
10045   CONTINUE
10036 CONTINUE
C
C Draw the edge of the grid.
C
      IF (.NOT.(ICLU(257).NE.0)) GO TO 10047
        ICLV=257
        IF (.NOT.(IDUF.NE.0)) GO TO 10048
          L10010=    3
          GO TO 10010
10049     CONTINUE
          IF (.NOT.(IDUF.LT.0)) GO TO 10050
            CALL DPSETC ('DPT',CLDP(ICLV)(1:LCLD))
            IF (ICFELL('CTCLDR',44).NE.0) RETURN
          GO TO 10051
10050     CONTINUE
            CALL DASHDC (CLDP(ICLV)(1:LCLD),ILDA,ILCH)
            IF (ICFELL('CTCLDR',45).NE.0) RETURN
10051     CONTINUE
10048   CONTINUE
        L10042=    2
        GO TO 10042
10052   CONTINUE
        IJMP=0
        IAIC=-9
10053   CONTINUE
          CALL CTTREG (RPNT,IEDG,ITRI,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,
     +                                                         NRWK)
          IF (ICFELL('CTCLDR',46).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10054
          CALL CTDRSG (RWRK,IRW1,IRW2,NRWK)
          IF (ICFELL('CTCLDR',47).NE.0) RETURN
        GO TO 10053
10054   CONTINUE
        L10046=    2
        GO TO 10046
10055   CONTINUE
10047 CONTINUE
C
C Restore the state of the dash package (if any) that was used.
C
      IF (IDUF.LT.0) THEN
C
        CALL DPSETI ('DPS',IDPS)
        IF (ICFELL('CTCLDR',48).NE.0) RETURN
        CALL DPSETC ('DPT',CHDP(1:IDPL))
        IF (ICFELL('CTCLDR',49).NE.0) RETURN
        CALL DPSETR ('TCS',RTCS)
        IF (ICFELL('CTCLDR',50).NE.0) RETURN
        CALL DPSETR ('WOC',RWOC)
        IF (ICFELL('CTCLDR',51).NE.0) RETURN
        CALL DPSETR ('WOG',RWOG)
        IF (ICFELL('CTCLDR',52).NE.0) RETURN
        CALL DPSETR ('WOS',RWOS)
        IF (ICFELL('CTCLDR',53).NE.0) RETURN
C
      ELSE IF (IDUF.GT.0) THEN
C
        CALL DASHDC ('$$$$$$$$$$$$$$$$',ILDA,ILCH)
        IF (ICFELL('CTCLDR',54).NE.0) RETURN
        ISMO=ISMS
C
      END IF
C
C Done.
C
      RETURN
C
C The following internal procedure finds the length of a dash pattern.
C
10010 CONTINUE
        LCLD=1
        DO 10056 I=1,128
          IF (CLDP(ICLV)(I:I).NE.' ') LCLD=I
10056   CONTINUE
      GO TO (10009,10038,10049) , L10010
C
C The following internal procedure calls CTTRCL to draw the contour
C line at a given level.  The user-change routine is called before
C and after the calls to CTTRCL.
C
10016 CONTINUE
        L10042=    3
        GO TO 10042
10057   CONTINUE
        IJMP=0
10058   CONTINUE
          CALL CTTRCL (RPNT,IEDG,ITRI,RWRK,IWRK,CLEV(ICLV),IJMP,IRW1,
     +                                                     IRW2,NRWK)
          IF (ICFELL('CTCLDR',55).NE.0) RETURN
          IF (IJMP.EQ.0) GO TO 10059
          CALL CTDRSG (RWRK,IRW1,IRW2,NRWK)
          IF (ICFELL('CTCLDR',56).NE.0) RETURN
          IF (IHCF.NE.0) THEN
            CALL CTHCHR (RWRK,IRW1,IRW2,NRWK)
            IF (ICFELL('CTCLDR',57).NE.0) RETURN
          END IF
        GO TO 10058
10059   CONTINUE
        L10046=    3
        GO TO 10046
10060   CONTINUE
      GO TO (10015,10025,10035) , L10016
C
C The following internal procedures set and reset line color and width
C before and after a particular line is drawn.
C
10042 CONTINUE
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CTCLDR',58).NE.0) RETURN
        JCCL=ICCL(ICLV)
        IF (JCCL.GE.0) THEN
          CALL GQPLCI (IGER,ISLC)
          IF (IGER.NE.0) THEN
            CALL SETER ('CTCLDR - ERROR EXIT FROM GQPLCI',59,1)
            RETURN
          END IF
          CALL GQTXCI (IGER,ISTC)
          IF (IGER.NE.0) THEN
            CALL SETER ('CTCLDR - ERROR EXIT FROM GQTXCI',60,1)
            RETURN
          END IF
          CALL GSPLCI (JCCL)
          CALL GSTXCI (JCCL)
        END IF
        CLWS=CLWA(ICLV)
        IF (CLWS.GT.0.) THEN
          CALL GQLWSC (IGER,SFLW)
          IF (IGER.NE.0) THEN
            CALL SETER ('CTCLDR - ERROR EXIT FROM GQLWSC',61,1)
            RETURN
          END IF
          CALL GSLWSC (CLWS)
        END IF
        IPAI=ICLV
        IF (IPAI.GT.256) IPAI=256-IPAI
        CALL HLUCTCHCL (+1)
        IF (ICFELL('CTCLDR',62).NE.0) RETURN
      GO TO (10041,10052,10057) , L10042
C
10046 CONTINUE
        CALL PLOTIF (0.,0.,2)
        IF (ICFELL('CTCLDR',63).NE.0) RETURN
        IPAI=ICLV
        IF (IPAI.GT.256) IPAI=256-IPAI
        CALL HLUCTCHCL (-1)
        IF (ICFELL('CTCLDR',64).NE.0) RETURN
        IF (JCCL.GE.0) THEN
          CALL GSPLCI (ISLC)
          CALL GSTXCI (ISTC)
        END IF
        IF (CLWS.GT.0.) THEN
          CALL GSLWSC (SFLW)
        END IF
      GO TO (10045,10055,10060) , L10046
C
      END
