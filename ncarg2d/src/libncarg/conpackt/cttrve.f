C
C $Id: cttrve.f,v 1.1 2003-05-28 15:44:35 kennison Exp $
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
      SUBROUTINE CTTRVE (RPNT,IEDG,ITRI,RWRK,IWRK,IJMP,IRW1,IRW2,NRWK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C This routine traces the edge of the area which is visible under the
C current mapping, using the limited inverse mapping capabilities of
C CTMXYZ.
C
C As pieces of the edge are generated, control is passed back to the
C caller for processing of them.
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
C IJMP is initially set to zero by the caller.  Upon return, it will be
C zero if all segments have been traced and processed, non-zero if the
C caller is expected to process a segment and recall CTTRVE.
C
C IRW1 and IRW2 are output variables.  If IJMP is non-zero, they are
C base indices of X and Y coordinate arrays in RWRK.
C
C NRWK is an output variable.  If IJMP is non-zero, NRWK is the number
C of coordinates to be processed by the caller.
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
C
C Because of the way this routine is entered and re-entered, we need to
C save every variable it uses.
C
      SAVE
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (103,104) , IJMP
C
C Assign space to use for storing the X and Y coordinates of points.
C
      MPLS=LRWC
      CALL CTGRWS (RWRK,1,2*MPLS,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CTTRVE',1).NE.0) GO TO 102
C
C Compute required constants.  By default, we work with a grid of
C approximately 2500 boxes in the current viewport; each of the boxes
C is roughly square.  The user may set the value of 'PIE' non-zero to
C increase the number of boxes used.
C
      IIDM=MAX(2,INT(SQRT(2500.*(XVPR-XVPL)/(YVPT-YVPB))))
      IIDN=MAX(2,INT(SQRT(2500.*(YVPT-YVPB)/(XVPR-XVPL))))
C
      IIDM=(IIDM-1)*(ABS(IPIE)+1)+1
      IIDN=(IIDN-1)*(ABS(IPIE)+1)+1
C
      RIDM=(XVPR-XVPL)/REAL(IIDM-1)
      RIDN=(YVPT-YVPB)/REAL(IIDN-1)
C
C Zero the count of horizontal segments seen.
C
      NHSS=0
C
C Define the first search point.
C
      IVBX=1
      IVBY=1
      XDUM=CFUX(XVPL)
      IF (ICFELL('CTTRVE',2).NE.0) GO TO 102
      YDUM=CFUY(YVPB)
      IF (ICFELL('CTTRVE',3).NE.0) GO TO 102
      CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XPRN,YPRN)
      IF (ICFELL('CTTRVE',4).NE.0) GO TO 102
C
C Search the viewport for pieces of the visible/invisible edge.  We
C first search the edges of the viewport for open-ended pieces and
C then we search the interior of the viewport for pieces that are
C closed loops.  The common variable IOCF is used to indicate which
C type of piece we are dealing with.  Its value will be modified by
C FOLLOW-THE-LIMB to provide the calling routine with even more
C information about the pieces returned (whether or not the first
C point and the last point of the piece is included in the buffer
C load being returned); this information is passed to CTTROE by the
C routine CTCLAM.
C
10001 CONTINUE
      IF (.NOT.(IVBX.LT.IIDM)) GO TO 10002
        IVBX=IVBX+1
        XPRP=XPRN
        XDUM=CFUX(XVPL+RIDM*REAL(IVBX-1))
        IF (ICFELL('CTTRVE',5).NE.0) GO TO 102
        YDUM=CFUY(YVPB)
        IF (ICFELL('CTTRVE',6).NE.0) GO TO 102
        CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XPRN,YPRN)
        IF (ICFELL('CTTRVE',7).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10003
          IOCF=0
          INCI=1
          L10005=    1
          GO TO 10005
10004     CONTINUE
10003   CONTINUE
      GO TO 10001
10002 CONTINUE
C
10006 CONTINUE
      IF (.NOT.(IVBY.LT.IIDN)) GO TO 10007
        IVBY=IVBY+1
        XPRP=XPRN
        XDUM=CFUX(XVPL+RIDM*REAL(IIDM-1))
        IF (ICFELL('CTTRVE',8).NE.0) GO TO 102
        YDUM=CFUY(YVPB+RIDN*REAL(IVBY-1))
        IF (ICFELL('CTTRVE',9).NE.0) GO TO 102
        CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XPRN,YPRN)
        IF (ICFELL('CTTRVE',10).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10008
          IOCF=0
          INCI=7
          L10005=    2
          GO TO 10005
10009     CONTINUE
10008   CONTINUE
      GO TO 10006
10007 CONTINUE
C
10010 CONTINUE
      IF (.NOT.(IVBX.GT.1)) GO TO 10011
        IVBX=IVBX-1
        XPRP=XPRN
        XDUM=CFUX(XVPL+RIDM*REAL(IVBX-1))
        IF (ICFELL('CTTRVE',11).NE.0) GO TO 102
        YDUM=CFUY(YVPB+RIDN*REAL(IIDN-1))
        IF (ICFELL('CTTRVE',12).NE.0) GO TO 102
        CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XPRN,YPRN)
        IF (ICFELL('CTTRVE',13).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10012
          IOCF=0
          INCI=5
          L10005=    3
          GO TO 10005
10013     CONTINUE
10012   CONTINUE
      GO TO 10010
10011 CONTINUE
C
10014 CONTINUE
      IF (.NOT.(IVBY.GT.1)) GO TO 10015
        IVBY=IVBY-1
        XPRP=XPRN
        XDUM=CFUX(XVPL)
        IF (ICFELL('CTTRVE',14).NE.0) GO TO 102
        YDUM=CFUY(YVPB+RIDN*REAL(IVBY-1))
        IF (ICFELL('CTTRVE',15).NE.0) GO TO 102
        CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XPRN,YPRN)
        IF (ICFELL('CTTRVE',16).NE.0) GO TO 102
        IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10016
          IOCF=0
          INCI=3
          L10005=    4
          GO TO 10005
10017     CONTINUE
10016   CONTINUE
      GO TO 10014
10015 CONTINUE
C
        IVBY = 2
        GO TO 10020
10018   CONTINUE
        IVBY =IVBY +1
10020   CONTINUE
        IF (IVBY .GT.(IIDN-1)) GO TO 10019
        XDUM=CFUX(XVPL)
        IF (ICFELL('CTTRVE',17).NE.0) GO TO 102
        RVBY=CFUY(YVPB+RIDN*REAL(IVBY-1))
        IF (ICFELL('CTTRVE',18).NE.0) GO TO 102
        CALL HLUCTMXYZ (-IMPF,XDUM,RVBY,ZDUM,XPRN,YPRN)
        IF (ICFELL('CTTRVE',19).NE.0) GO TO 102
          IVBX = 2
          GO TO 10023
10021     CONTINUE
          IVBX =IVBX +1
10023     CONTINUE
          IF (IVBX .GT.(IIDM)) GO TO 10022
          XPRP=XPRN
          XDUM=CFUX(XVPL+RIDM*REAL(IVBX-1))
          IF (ICFELL('CTTRVE',20).NE.0) GO TO 102
          CALL HLUCTMXYZ (-IMPF,XDUM,RVBY,ZDUM,XPRN,YPRN)
          IF (ICFELL('CTTRVE',21).NE.0) GO TO 102
          IF (.NOT.(XPRP.EQ.OORV.AND.XPRN.NE.OORV)) GO TO 10024
            IPXY=IIDN*IVBX+IVBY
            DO 10025 I=1,NHSS
              IF (IPXY.EQ.IWRK(II01+I)) GO TO 101
10025       CONTINUE
            IF (.NOT.(NHSS.GE.LI01)) GO TO 10026
              CALL CTGIWS (IWRK,1,LI01+100,IWSE)
              IF (IWSE.NE.0.OR.ICFELL('CTTRVE',22).NE.0) GO TO 102
10026       CONTINUE
            NHSS=NHSS+1
            IWRK(II01+NHSS)=IPXY
            IOCF=1
            INCI=1
            L10005=    5
            GO TO 10005
10027       CONTINUE
  101     CONTINUE
10024     CONTINUE
        GO TO 10021
10022   CONTINUE
      GO TO 10018
10019 CONTINUE
C
C Release the workspaces and let the user know we're done.
C
  102 LI01=0
      LR01=0
      IJMP=0
C
C Done.
C
      RETURN
C
C Limb-following algorithm.  This internal routine moves the limb-
C following vector (defined by the base point (IVBX,IVBY) and the
C components INCX(INCI) and INCY(INCI)) along a limb line.  The
C points defining the limb line are thereby determined.  The process
C stops when either the starting point or the edge of the grid is
C encountered.
C
10005 CONTINUE
C
        NPLS=0
C
        MVBX=IVBX
        MVBY=IVBY
        MNCI=INCI
C
        IVEX=IVBX+INCX(INCI)
        IVEY=IVBY+INCY(INCI)
C
        L10029=    1
        GO TO 10029
10028   CONTINUE
C
10030   CONTINUE
C
          INCI=INCI+1
          IF (INCI.GT.8) INCI=INCI-8
          IVEX=IVBX+INCX(INCI)
          IVEY=IVBY+INCY(INCI)
C
          IF (IVEX.LT.1.OR.IVEX.GT.IIDM.OR.IVEY.LT.1.OR.IVEY.GT.IIDN)
     +    GO TO 10031
C
          XDUM=CFUX(XVPL+RIDM*REAL(IVEX-1))
          IF (ICFELL('CTTRVE',23).NE.0) GO TO 102
          YDUM=CFUY(YVPB+RIDN*REAL(IVEY-1))
          IF (ICFELL('CTTRVE',24).NE.0) GO TO 102
          CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XTMP,YTMP)
          IF (ICFELL('CTTRVE',25).NE.0) GO TO 102
          IF (.NOT.(XTMP.NE.OORV)) GO TO 10032
C
            IVBX=IVEX
            IVBY=IVEY
            INCI=INCI+4
C
          GO TO 10033
10032     CONTINUE
          IF (.NOT.((INCI/2)*2.NE.INCI)) GO TO 10034
C
            L10029=    2
            GO TO 10029
10035       CONTINUE
C
            IF (.NOT.(INCI.EQ.1)) GO TO 10036
              IF (.NOT.(NHSS.GE.LI01)) GO TO 10037
                CALL CTGIWS (IWRK,1,LI01+100,IWSE)
                IF (IWSE.NE.0.OR.ICFELL('CTTRVE',26).NE.0) GO TO 102
10037         CONTINUE
              NHSS=NHSS+1
              IWRK(II01+NHSS)=IIDN*IVBX+IVBY
10036       CONTINUE
C
            IF (IVBX.EQ.MVBX.AND.IVBY.EQ.MVBY.AND.INCI.EQ.MNCI) GO TO 10
     +031
C
10033     CONTINUE
10034     CONTINUE
C
        GO TO 10030
10031   CONTINUE
C
C Note: At this point, if NPLS is 1, and the call was from CTCLAM,
C control has to return there so that so that CTTROE can properly
C do its thing.  If the call came from somewhere else, there should
C be no problem - it's just a little inefficient.
C
        IF (.NOT.(NPLS.NE.0)) GO TO 10038
          IJMP=1
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          IOCF=IOR(IOCF,4)
          RETURN
10038   CONTINUE
C
  103   IVBX=MVBX
        IVBY=MVBY
C
      GO TO (10004,10009,10013,10017,10027) , L10005
C
C The following procedure, given a point on either side of the limb,
C uses a binary-halving technique to determine a point on the limb and
C adds that point to the list.  It also estimates the angle of the
C tangent to the limb; if the angles of the last two tangents indicate
C that the limb is convex as viewed from the visible side, it adds the
C point of intersection of the two tangents to the list before adding
C the new point.
C
10029 CONTINUE
C
        XCVF=XVPL+RIDM*REAL(IVBX-1)
        YCVF=YVPB+RIDN*REAL(IVBY-1)
        XDUM=CFUX(XCVF)
        IF (ICFELL('CTTRVE',27).NE.0) GO TO 102
        YDUM=CFUY(YCVF)
        IF (ICFELL('CTTRVE',28).NE.0) GO TO 102
        CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XCVD,YCVD)
        IF (ICFELL('CTTRVE',29).NE.0) GO TO 102
C
        XCIF=XVPL+RIDM*REAL(IVEX-1)
        YCIF=YVPB+RIDN*REAL(IVEY-1)
C
        ITMP=0
C
10039   CONTINUE
          XCHF=(XCVF+XCIF)/2.
          YCHF=(YCVF+YCIF)/2.
          XDUM=CFUX(XCHF)
          IF (ICFELL('CTTRVE',30).NE.0) GO TO 102
          YDUM=CFUY(YCHF)
          IF (ICFELL('CTTRVE',31).NE.0) GO TO 102
          CALL HLUCTMXYZ (-IMPF,XDUM,YDUM,ZDUM,XCHD,YCHD)
          IF (ICFELL('CTTRVE',32).NE.0) GO TO 102
          IF (.NOT.(XCHD.NE.OORV)) GO TO 10040
            IF (XCHF.EQ.XCVF.AND.YCHF.EQ.YCVF) GO TO 10041
            XCVF=XCHF
            YCVF=YCHF
            XCVD=XCHD
            YCVD=YCHD
          GO TO 10042
10040     CONTINUE
            IF (XCHF.EQ.XCIF.AND.YCHF.EQ.YCIF) GO TO 10041
            XCIF=XCHF
            YCIF=YCHF
10042     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10041
        GO TO 10039
10041   CONTINUE
C
        IF (.NOT.(NPLS.NE.0)) GO TO 10043
          XDUM=CUFX(RWRK(IR01+     NPLS))
          IF (ICFELL('CTTRVE',33).NE.0) GO TO 102
          YDUM=CUFY(RWRK(IR01+MPLS+NPLS))
          IF (ICFELL('CTTRVE',34).NE.0) GO TO 102
          IF (.NOT.(ABS(XCVF-XDUM).LE..0001*ABS(XVPR-XVPL).AND.ABS(YCVF-
     +YDUM).LE..0001*ABS(YVPT-YVPB))) GO TO 10044
            IF (NPLS.EQ.1) GO TO 105
            NPLS=NPLS-1
10044     CONTINUE
10043   CONTINUE
C
        NPLS=NPLS+1
        RWRK(IR01     +NPLS)=CFUX(XCVF)
        IF (ICFELL('CTTRVE',35).NE.0) GO TO 102
        RWRK(IR01+MPLS+NPLS)=CFUY(YCVF)
        IF (ICFELL('CTTRVE',36).NE.0) GO TO 102
C
        IF (.NOT.(NPLS.GE.MPLS)) GO TO 10045
          XSAV=RWRK(IR01     +NPLS)
          YSAV=RWRK(IR01+MPLS+NPLS)
          IJMP=2
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  104     IOCF=IOR(IOCF,2)
          RWRK(IR01     +1)=XSAV
          RWRK(IR01+MPLS+1)=YSAV
          NPLS=1
10045   CONTINUE
C
  105 CONTINUE
      GO TO (10028,10035) , L10029
C
      END
