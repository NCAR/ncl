C
C $Id: cttrcl.f,v 1.1 2003-05-28 15:44:35 kennison Exp $
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
      SUBROUTINE CTTRCL (RPNT,IEDG,ITRI,RWRK,IWRK,CLVL,IJMP,IRW1,IRW2,
     +                                                           NRWK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C Given RPNT, IEDG, and ITRI (which define a triangular mesh), RWRK (a
C real workspace), IWRK (an integer workspace), and CLVL (a particular
C contour level), CTTRCL finds the beginning of each contour line at
C the level CLVL and then traces it.  Control is passed back to the
C caller to process the line segments generated.
C
C RPNT is an array of nodes defining vertices of triangles.
C
C IEDG is an array of nodes defining edges of triangles.
C
C ITRI is an array of nodes defining triangles.
C
C RWRK is a real workspace array.
C
C IWRK is an integer workspace array.
C
C CLVL is the contour level being worked on.
C
C IJMP is initially set to zero by the caller.  Upon return, it will be
C zero if all segments have been traced and processed, non-zero if the
C caller is expected to process a segment and recall CTTRCL.
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
C Because of the way this routine is entered and reentered, we need to
C save every variable it uses.
C
      SAVE
C
C Define an interpolation function.
C
      FRCT(ZDT1,ZDT2)=(CLVL-ZDT1)/(ZDT2-ZDT1)
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (102,103) , IJMP
C
C Assign space to use for storing the coordinates of points on contour
C lines.
C
      IF (.NOT.(T2DS.EQ.0.)) GO TO 10001
        CALL CTGRWS (RWRK,1,2*LRWC,IWSE)
      GO TO 10002
10001 CONTINUE
        CALL CTGRWS (RWRK,1,7*LRWC,IWSE)
10002 CONTINUE
C
      IF (IWSE.NE.0.OR.ICFELL('CTTRCL',1).NE.0) GO TO 101
C
C Set the offset from one portion of the real workspace to the next.
C
      MPLS=LRWC
C
C Set some tolerances.
C
      DBPI=ABS(XWDR-XWDL)*SEGL
      EPSX=ABS(XWDR-XWDL)*EPSI
      EPSY=ABS(YWDT-YWDB)*EPSI
C
      SMLX=.01*ABS(XWDR-XWDL)
      SMLY=.01*ABS(YWDT-YWDB)
C
      PITX=PITH*ABS(XWDR-XWDL)
      PITY=PITH*ABS(YWDT-YWDB)
C
C Initialize four variables to prevent the code from blowing up.  (The
C values only have to be legal values on the machine; they are used in
C one clause of a block-IF, the other clause of which makes the value
C immaterial.)
C
      XBFS=0.
      YBFS=0.
      XELS=0.
      YELS=0.
C
C Zero the utility flags in all of the edge nodes.  (They will be used
C to mark edges we've already visited.)
C
      DO 10003 IIII=0,NEDG-LOEN,LOEN
        IEDG(IIII+5)=0
10003 CONTINUE
C
C Search for open contours (those that start and end on an external
C edge of the mesh).
C
      IOCF=0
C
        IIII = 0
        GO TO 10006
10004   CONTINUE
        IIII =IIII +LOEN
10006   CONTINUE
        IF (LOEN) 10007,10008,10009
10007   CONTINUE
        IF (IIII .LT.(NEDG-LOEN)) GO TO 10005
        GO TO 10008
10009   CONTINUE
        IF (IIII .GT.(NEDG-LOEN)) GO TO 10005
10008   CONTINUE
        IF (.NOT.(IEDG(IIII+5).EQ.0.AND.IEDG(IIII+4).LT.0)) GO TO 10010
          IF (.NOT.(RPNT(IEDG(IIII+1)+4).LE.CLVL.AND.RPNT(IEDG(IIII+2)+4
     +).GT.CLVL)) GO TO 10011
            IPTE=IIII
            L10013=    1
            GO TO 10013
10012       CONTINUE
10011     CONTINUE
10010   CONTINUE
      GO TO 10004
10005 CONTINUE
C
C Search for closed contours (those that never touch an external edge
C of the mesh).
C
      IOCF=1
C
        IIII = 0
        GO TO 10016
10014   CONTINUE
        IIII =IIII +LOEN
10016   CONTINUE
        IF (LOEN) 10017,10018,10019
10017   CONTINUE
        IF (IIII .LT.(NEDG-LOEN)) GO TO 10015
        GO TO 10018
10019   CONTINUE
        IF (IIII .GT.(NEDG-LOEN)) GO TO 10015
10018   CONTINUE
        IF (.NOT.(IEDG(IIII+5).EQ.0)) GO TO 10020
          IF (.NOT.(RPNT(IEDG(IIII+1)+4).LE.CLVL.AND.RPNT(IEDG(IIII+2)+4
     +).GT.CLVL)) GO TO 10021
            IPTE=IIII
            L10013=    2
            GO TO 10013
10022       CONTINUE
10021     CONTINUE
10020   CONTINUE
      GO TO 10014
10015 CONTINUE
C
C Done.
C
  101 LR01=0
      IJMP=0
      RETURN
C
C Line-following algorithm.  This internal routine moves from edge
C to edge in the triangular mesh, generating the points defining the
C contour line, stopping when either a previously-used edge of the
C mesh or an external edge of the mesh is encountered.
C
10013 CONTINUE
C
C Find the coordinates, in the data coordinate system, of the starting
C position of the contour line.  If the point is very close to a grid
C intersection, put it at the intersection; this avoids problems caused
C by very short line segments.  Also, be careful to compute the value
C of DFRA using code exactly like that in CTTREG, thus ensuring that
C points it interpolates where contour lines intersect the edges of
C the grid will match the points generated here.  ??? INCOMPLETE ???
C
        DFRA=FRCT(RPNT(IEDG(IPTE+1)+4),RPNT(IEDG(IPTE+2)+4))
C
        IF (DFRA.LE..00001) DFRA=0.
        IF (DFRA.GE..99999) DFRA=1.
C
        XCND=RPNT(IEDG(IPTE+1)+1)+DFRA*
     +      (RPNT(IEDG(IPTE+2)+1)-RPNT(IEDG(IPTE+1)+1))
C
        YCND=RPNT(IEDG(IPTE+1)+2)+DFRA*
     +      (RPNT(IEDG(IPTE+2)+2)-RPNT(IEDG(IPTE+1)+2))
C
        ZCND=RPNT(IEDG(IPTE+1)+3)+DFRA*
     +      (RPNT(IEDG(IPTE+2)+3)-RPNT(IEDG(IPTE+1)+3))
C
C Map the point (XCND,YCND,ZCND) to the position (XCNU,YCNU).
C
        L10024=    1
        GO TO 10024
10023   CONTINUE
C
C Zero the number of points in the coordinate arrays, initialize the
C flag that indicates we're working on the first segment, and zero
C the variable that keeps track of the ratio of segment length in
C the user coordinate system to segment length in the data coordinate
C system.
C
        NPLS=0
        IFSF=1
        RUDN=0.
C
C Loop to generate the rest of the points on the contour line.
C
10025   CONTINUE
C
C Mark the current edge as having been used.
C
          IEDG(IPTE+5)=1
C
C Exit the loop if there is no triangle to the left of the current edge;
C an open contour line has hit an external edge of the triangular mesh.
C
          IF (IEDG(IPTE+3).LT.0) GO TO 10026
C
C Get a base pointer, IPTT, for the triangle to the left of the current
C edge, and an offset, IPTI, to its pointer to that edge.
C
          IPTT=LOTN*((IEDG(IPTE+3)-1)/LOTN)
          IPTI=IEDG(IPTE+3)-IPTT
C
C Reset IPTE to point to that edge of the triangle where the contour
C line exits.  Logically, the "EXIT" statement should not be reachable.
C
          IPTI=MOD(IPTI,3)+1
          IPTE=ITRI(IPTT+IPTI)
C
          IF (.NOT.(RPNT(IEDG(IPTE+1)+4).GT.CLVL.OR.RPNT(IEDG(IPTE+2)+4)
     +.LE.CLVL)) GO TO 10027
C
            IPTI=MOD(IPTI,3)+1
            IPTE=ITRI(IPTT+IPTI)
C
            IF (RPNT(IEDG(IPTE+1)+4).GT.CLVL.OR.RPNT(IEDG(IPTE+2)+4).LE.
     +CLVL) GO TO 10026
C
10027     CONTINUE
C
C Save the coordinates of the previous point on the contour line and
C compute coordinates of a new one.
C
          XCOD=XCND
          YCOD=YCND
          ZCOD=ZCND
C
          XCOU=XCNU
          YCOU=YCNU
C
          IVOU=IVNU
C
          DFRA=FRCT(RPNT(IEDG(IPTE+1)+4),RPNT(IEDG(IPTE+2)+4))
C
          IF (DFRA.LE..00001) DFRA=0.
          IF (DFRA.GE..99999) DFRA=1.
C
          XCND=RPNT(IEDG(IPTE+1)+1)+DFRA*
     +        (RPNT(IEDG(IPTE+2)+1)-RPNT(IEDG(IPTE+1)+1))
C
          YCND=RPNT(IEDG(IPTE+1)+2)+DFRA*
     +        (RPNT(IEDG(IPTE+2)+2)-RPNT(IEDG(IPTE+1)+2))
C
          ZCND=RPNT(IEDG(IPTE+1)+3)+DFRA*
     +        (RPNT(IEDG(IPTE+2)+3)-RPNT(IEDG(IPTE+1)+3))
C
C Map the point (XCND,YCND,ZCND) to the position (XCNU,YCNU).
C
          L10024=    2
          GO TO 10024
10028     CONTINUE
C
C If the triangle to the right of the edge (the one about to be crossed
C by the contour line) is in a blocked area, dump anything in the buffer
C and clear it.  Otherwise, process the contour-line segment from the
C old point to the new one.
C
          IF (.NOT.(ITRI(LOTN*((IEDG(IPTE+4)-1)/LOTN)+4).NE.0)) GO TO 10
     +029
            L10031=    1
            GO TO 10031
10030       CONTINUE
          GO TO 10032
10029     CONTINUE
            L10034=    1
            GO TO 10034
10033       CONTINUE
10032     CONTINUE
C
C Exit the loop if the current edge has been used before; a closed
C contour line has been completely traced.
C
          IF (IEDG(IPTE+5).NE.0) GO TO 10026
C
C Loop back to find the next point on the contour line.
C
        GO TO 10025
10026   CONTINUE
C
C Process any remaining portion of the contour line.
C
        L10031=    2
        GO TO 10031
10035   CONTINUE
C
C Done.
C
      GO TO (10012,10022) , L10013
C
C The following internal procedure, given a line segment, adds visible
C portions of it to the coordinate arrays.
C
10034 CONTINUE
C
C If point interpolation is turned on, do the first IPIC segments.
C
        IF (.NOT.(IPIC.NE.0)) GO TO 10036
          XSOD=XCOD
          YSOD=YCOD
          ZSOD=ZCOD
          XSND=XCND
          YSND=YCND
          ZSND=ZCND
          XSNU=XCNU
          YSNU=YCNU
          ISNU=IVNU
            INTP = 1
            GO TO 10039
10037       CONTINUE
            INTP =INTP +1
10039       CONTINUE
            IF (INTP .GT.(IABS(IPIC))) GO TO 10038
            XCND=XSOD+(REAL(INTP)/REAL(IABS(IPIC)+1))*(XSND-XSOD)
            YCND=YSOD+(REAL(INTP)/REAL(IABS(IPIC)+1))*(YSND-YSOD)
            ZCND=ZSOD+(REAL(INTP)/REAL(IABS(IPIC)+1))*(ZSND-ZSOD)
            L10024=    3
            GO TO 10024
10040       CONTINUE
            IF (.NOT.(IPIC.GT.0.OR.IVNU.NE.IVOU)) GO TO 10041
              L10043=    1
              GO TO 10043
10042         CONTINUE
              XCOD=XCND
              YCOD=YCND
              ZCOD=ZCND
              XCOU=XCNU
              YCOU=YCNU
              IVOU=IVNU
10041       CONTINUE
          GO TO 10037
10038     CONTINUE
          XCND=XSND
          YCND=YSND
          ZCND=ZSND
          XCNU=XSNU
          YCNU=YSNU
          IVNU=ISNU
10036   CONTINUE
C
C Finish off the job.
C
        L10043=    2
        GO TO 10043
10044   CONTINUE
C
      GO TO (10033) , L10034
C
C The following internal procedure examines the points (XCOD,YCOD,ZCOD),
C which projects into (XCOU,YCOU), and (XCND,YCND,ZCND), which projects
C into (XCNU,YCNU), either of which may be visible or invisible in the
C projection space, and adds visible portions of the line segment
C between them to the polyline being built.
C
10043 CONTINUE
C
        IF (.NOT.(XCND.NE.XCOD.OR.YCND.NE.YCOD.OR.ZCND.NE.ZCOD))
     +  GO TO 10045
C
          IF (.NOT.(NPLS.EQ.0)) GO TO 10046
            IF (.NOT.(IVOU.NE.0)) GO TO 10047
              IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10048
                XCLD=XCOD
                YCLD=YCOD
                ZCLD=ZCOD
                XCLU=XCOU
                YCLU=YCOU
10048         CONTINUE
              RWRK(IR01+1)=XCOU
              RWRK(IR01+MPLS+1)=YCOU
              NPLS=1
            GO TO 10049
10047       CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10050
              XCID=XCOD
              YCID=YCOD
              ZCID=ZCOD
              XCVD=XCND
              YCVD=YCND
              ZCVD=ZCND
              XCVU=XCNU
              YCVU=YCNU
              L10052=    1
              GO TO 10052
10051         CONTINUE
              L10054=    1
              GO TO 10054
10053         CONTINUE
              XCOD=XCVD
              YCOD=YCVD
              ZCOD=ZCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10049       CONTINUE
10050       CONTINUE
          GO TO 10055
10046     CONTINUE
          IF (.NOT.(NPLS.EQ.MPLS)) GO TO 10056
            XSAV=RWRK(IR01+NPLS)
            YSAV=RWRK(IR01+MPLS+NPLS)
            L10031=    3
            GO TO 10031
10057       CONTINUE
            RWRK(IR01+1)=XSAV
            RWRK(IR01+MPLS+1)=YSAV
            NPLS=1
10055     CONTINUE
10056     CONTINUE
C
          IF (.NOT.(IVNU.NE.0)) GO TO 10058
            L10060=    1
            GO TO 10060
10059       CONTINUE
          GO TO 10061
10058     CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10062
            XCVD=XCOD
            YCVD=YCOD
            ZCVD=ZCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XCND
            YCID=YCND
            ZCID=ZCND
            L10052=    2
            GO TO 10052
10063       CONTINUE
            XKND=XCND
            YKND=YCND
            ZKND=ZCND
            XKNU=XCNU
            YKNU=YCNU
            XCND=XCVD
            YCND=YCVD
            ZCND=ZCVD
            XCNU=XCVU
            YCNU=YCVU
            L10060=    2
            GO TO 10060
10064       CONTINUE
            XCND=XKND
            YCND=YKND
            ZCND=ZKND
            XCNU=XKNU
            YCNU=YKNU
            L10031=    4
            GO TO 10031
10065       CONTINUE
10061     CONTINUE
10062     CONTINUE
C
10045   CONTINUE
C
      GO TO (10042,10044) , L10043
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10060 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.(XCND.NE.XCOD.OR.YCND.NE.YCOD.OR.ZCND.NE
     +.ZCOD))) GO TO 10066
          RUDO=RUDN
          RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +         (ABS(XCND-XCOD)+ABS(YCND-YCOD)+ABS(ZCND-ZCOD))
          IF (.NOT.(RUDN.GT.2.*RUDO)) GO TO 10067
            L10069=    1
            GO TO 10069
10068       CONTINUE
10067     CONTINUE
          IF (.NOT.(PITH.GT.0.)) GO TO 10070
            XCTD=XCND
            YCTD=YCND
            ZCTD=ZCND
            XCTU=XCNU
            YCTU=YCNU
            L10072=    1
            GO TO 10072
10071       CONTINUE
10070     CONTINUE
10066   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCNU
        RWRK(IR01+MPLS+NPLS)=YCNU
      GO TO (10059,10064) , L10060
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the contour line is seen.
C It checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10069 CONTINUE
        XC1D=XCOD
        YC1D=YCOD
        ZC1D=ZCOD
        XC1U=XCOU
        YC1U=YCOU
        XC2D=XCND
        YC2D=YCND
        ZC2D=ZCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10073   CONTINUE
          DSTO=ABS(XC2U-XC1U)+ABS(YC2U-YC1U)
          XC3D=(XC1D+XC2D)/2.
          YC3D=(YC1D+YC2D)/2.
          ZC3D=(ZC1D+ZC2D)/2.
          CALL HLUCTMXYZ (IMPF,XC3D,YC3D,ZC3D,XC3U,YC3U)
          IF (ICFELL('CTTRCL',2).NE.0) GO TO 101
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10074
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10075
              ITMP=1000
              GO TO 10076
10075       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10077
              IF (XC3D.EQ.XC1D.AND.YC3D.EQ.YC1D.AND.ZC3D.EQ.ZC1D) GO TO
     +10076
              XC1D=XC3D
              YC1D=YC3D
              ZC1D=ZC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10078
10077       CONTINUE
              IF (XC3D.EQ.XC2D.AND.YC3D.EQ.YC2D.AND.ZC3D.EQ.ZC2D) GO TO
     +10076
              XC2D=XC3D
              YC2D=YC3D
              ZC2D=ZC3D
              XC2U=XC3U
              YC2U=YC3U
10078       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10076
          GO TO 10079
10074     CONTINUE
            XCVD=XCOD
            YCVD=YCOD
            ZCVD=ZCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XC3D
            YCID=YC3D
            ZCID=ZC3D
            L10052=    3
            GO TO 10052
10080       CONTINUE
            L10054=    2
            GO TO 10054
10081       CONTINUE
            L10031=    5
            GO TO 10031
10082       CONTINUE
            XCID=XC3D
            YCID=YC3D
            ZCID=ZC3D
            XCVD=XCND
            YCVD=YCND
            ZCVD=ZCND
            XCVU=XCNU
            YCVU=YCNU
            L10052=    4
            GO TO 10052
10083       CONTINUE
            L10054=    3
            GO TO 10054
10084       CONTINUE
            ITMP=1000
            GO TO 10076
10079     CONTINUE
        GO TO 10073
10076   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10085
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10086
            XCTD=XC1D
            YCTD=YC1D
            ZCTD=ZC1D
            XCTU=XC1U
            YCTU=YC1U
            L10072=    2
            GO TO 10072
10087       CONTINUE
10086     CONTINUE
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XC1U
          RWRK(IR01+MPLS+NPLS)=YC1U
          L10031=    6
          GO TO 10031
10088     CONTINUE
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10089
            XCLD=XC2D
            YCLD=YC2D
            ZCLD=ZC2D
            XCLU=XC2U
            YCLU=YC2U
10089     CONTINUE
          RWRK(IR01+1)=XC2U
          RWRK(IR01+MPLS+1)=YC2U
          NPLS=1
10085   CONTINUE
      GO TO (10068) , L10069
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10052 CONTINUE
        ITMP=0
10090   CONTINUE
          XCHD=(XCVD+XCID)/2.
          YCHD=(YCVD+YCID)/2.
          ZCHD=(ZCVD+ZCID)/2.
          CALL HLUCTMXYZ (IMPF,XCHD,YCHD,ZCHD,XCHU,YCHU)
          IF (ICFELL('CTTRCL',3).NE.0) GO TO 101
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10091
            IF (XCHD.EQ.XCVD.AND.YCHD.EQ.YCVD.AND.ZCHD.EQ.ZCVD) GO TO 10
     +092
            XCVD=XCHD
            YCVD=YCHD
            ZCVD=ZCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10093
10091     CONTINUE
            IF (XCHD.EQ.XCID.AND.YCHD.EQ.YCID.AND.ZCHD.EQ.ZCID) GO TO 10
     +092
            XCID=XCHD
            YCID=YCHD
            ZCID=ZCHD
10093     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10092
        GO TO 10090
10092   CONTINUE
      GO TO (10051,10063,10080,10083) , L10052
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10054 CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10094
          IF (.NOT.(NPLS.EQ.0)) GO TO 10095
            XCLD=XCVD
            YCLD=YCVD
            ZCLD=ZCVD
            XCLU=XCVU
            YCLU=YCVU
          GO TO 10096
10095     CONTINUE
            XCTD=XCVD
            YCTD=YCVD
            ZCTD=ZCVD
            XCTU=XCVU
            YCTU=YCVU
            L10072=    3
            GO TO 10072
10097       CONTINUE
10096     CONTINUE
10094   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCVU
        RWRK(IR01+MPLS+NPLS)=YCVU
      GO TO (10053,10081,10084) , L10054
C
C The following internal procedure is invoked when mapping is being
C done and a new point is about to be added to the polyline buffer.
C It checks for a jump (using a user-defined threshold value) in
C the mapped coordinates of the point and, if such a jump is found,
C interpolates some points in between.  The assumption is made that
C all points in between are visible; if that is found not to be the
C case, no attempt is made to rectify the situation: the user probably
C screwed up the definition of the mapping function.
C
10072 CONTINUE
10098   CONTINUE
        IF (.NOT.(ABS(XCTU-XCLU).GT.PITX.OR.ABS(YCTU-YCLU).GT.PITY))
     +  GO TO 10099
          IFND=0
          XCQD=0.
          YCQD=0.
          ZCQD=0.
          RDST=.50
          RSTP=.25
10100     CONTINUE
            XCPD=XCLD+RDST*(XCTD-XCLD)
            YCPD=YCLD+RDST*(YCTD-YCLD)
            ZCPD=ZCLD+RDST*(ZCTD-ZCLD)
            CALL HLUCTMXYZ (IMPF,XCPD,YCPD,ZCPD,XCPU,YCPU)
            IF (ICFELL('CTTRCL',4).NE.0) GO TO 101
            IF (OORV.NE.0..AND.(XCPU.EQ.OORV.OR.YCPU.EQ.OORV)) GO TO 101
     +01
            IF (.NOT.(ABS(XCPU-XCLU).LT.PITX.AND.ABS(YCPU-YCLU).LT.PITY)
     +)     GO TO 10102
              IFND=1
              XCQD=XCPD
              YCQD=YCPD
              ZCQD=ZCPD
              XCQU=XCPU
              YCQU=YCPU
              IF (ABS(XCQU-XCLU).GT..5*PITX.OR.ABS(YCQU-YCLU).GT..5*PITY
     +)       GO TO 10101
              RDST=RDST+RSTP
            GO TO 10103
10102       CONTINUE
              RDST=RDST-RSTP
10103       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..001) GO TO 10101
          GO TO 10100
10101     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(XCQD.NE.XCLD.OR.YCQD.NE.YCLD.OR.ZCQD.
     +NE.ZCLD))) GO TO 10104
            NPLS=NPLS+1
            RWRK(IR01+NPLS)=XCQU
            RWRK(IR01+MPLS+NPLS)=YCQU
            IF (.NOT.(NPLS.EQ.MPLS)) GO TO 10105
              XSAV=RWRK(IR01+NPLS)
              YSAV=RWRK(IR01+MPLS+NPLS)
              L10031=    7
              GO TO 10031
10106         CONTINUE
              RWRK(IR01+1)=XSAV
              RWRK(IR01+MPLS+1)=YSAV
              NPLS=1
10105       CONTINUE
            XCLD=XCQD
            YCLD=YCQD
            ZCLD=ZCQD
            XCLU=XCQU
            YCLU=YCQU
          GO TO 10107
10104     CONTINUE
            XCLD=XCTD
            YCLD=YCTD
            ZCLD=ZCTD
            XCLU=XCTU
            YCLU=YCTU
10107     CONTINUE
        GO TO 10098
10099   CONTINUE
        XCLD=XCTD
        YCLD=YCTD
        ZCLD=ZCTD
        XCLU=XCTU
        YCLU=YCTU
      GO TO (10071,10087,10097) , L10072
C
C The following internal procedure is given the data-system coordinates
C of a point (XCND,YCND,ZCND) and computes the user-system coordinates
C of the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10024 CONTINUE
C
        IF (.NOT.(IMPF.EQ.0)) GO TO 10108
          XCNU=XCND
          YCNU=YCND
          IVNU=1
        GO TO 10109
10108   CONTINUE
          CALL HLUCTMXYZ (IMPF,XCND,YCND,ZCND,XCNU,YCNU)
          IF (ICFELL('CTTRCL',5).NE.0) GO TO 101
          IF (.NOT.((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)))
     +    GO TO 10110
            IVNU=0
          GO TO 10111
10110     CONTINUE
            IVNU=1
10111     CONTINUE
10109   CONTINUE
C
      GO TO (10023,10028,10040) , L10024
C
C The following internal procedure processes a complete line segment.
C If the 2D smoother is turned on, the routines MSKRV1 and MSKRV2 are
C called to smooth the segment.
C
C
10031 CONTINUE
C
        I=1
C
10112   CONTINUE
          I=I+1
          IF (I.GT.NPLS) GO TO 10113
          IF (.NOT.(ABS(RWRK(IR01+I)-RWRK(IR01+I-1)).LT.EPSX.AND.ABS(RWR
     +K(IR01+MPLS+I)-RWRK(IR01+MPLS+I-1)).LT.EPSY)) GO TO 10114
            IF (.NOT.(I.NE.NPLS)) GO TO 10115
              DO 10116 J=I+1,NPLS
                RWRK(IR01+J-1)=RWRK(IR01+J)
                RWRK(IR01+MPLS+J-1)=RWRK(IR01+MPLS+J)
10116         CONTINUE
            GO TO 10117
10115       CONTINUE
              RWRK(IR01     +NPLS-1)=RWRK(IR01     +NPLS)
              RWRK(IR01+MPLS+NPLS-1)=RWRK(IR01+MPLS+NPLS)
10117       CONTINUE
            I=I-1
            NPLS=NPLS-1
10114     CONTINUE
        GO TO 10112
10113   CONTINUE
C
        IF (.NOT.(NPLS.GT.1)) GO TO 10118
C
          IF (.NOT.(T2DS.EQ.0.)) GO TO 10119
C
            IJMP=2
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
C
10119     CONTINUE
C
            IF (.NOT.(NPLS.GT.3.AND.ABS(RWRK(IR01+NPLS)-RWRK(IR01+1)).LT
     +.EPSX.AND.ABS(RWRK(IR01+MPLS+NPLS)-RWRK(IR01+MPLS+1)).LT.EPSY))
     +      GO TO 10120
              ISLP=4
            GO TO 10121
10120       CONTINUE
            IF (.NOT.(IFSF.EQ.0.AND.ABS(RWRK(IR01+1)-XELS).LT.EPSX.AND.A
     +BS(RWRK(IR01+MPLS+1)-YELS).LT.EPSY)) GO TO 10122
              ISLP=1
              SLP1=SELS
              IF (.NOT.(ABS(RWRK(IR01+NPLS)-XBFS).LT.EPSX.AND.ABS(RWRK(I
     +R01+MPLS+NPLS)-YBFS).LT.EPSY)) GO TO 10123
                ISLP=0
                SLPN=SBFS
10123         CONTINUE
            GO TO 10121
10122       CONTINUE
              ISLP=3
10121       CONTINUE
C
            CALL MSKRV1 (NPLS,RWRK(IR01+1),RWRK(IR01+MPLS+1),
     +                   SLP1,SLPN,RWRK(IR01+2*MPLS+1),
     +                   RWRK(IR01+3*MPLS+1),RWRK(IR01+5*MPLS+1),
     +                   RWRK(IR01+4*MPLS+1),ABS(T2DS),ISLP)
            IF (ICFELL('CTTRCL',6).NE.0) GO TO 101
C
            NINT=MAX(3,1+INT(RWRK(IR01+4*MPLS+NPLS)/DBPI))
C
            NOUT=0
            TUDN=0.
C
              IINT = 0
              GO TO 10126
10124         CONTINUE
              IINT =IINT +1
10126         CONTINUE
              IF (IINT .GT.(NINT)) GO TO 10125
C
              IF (.NOT.(IINT.EQ.0)) GO TO 10127
                XTMP=RWRK(IR01+1)
                YTMP=RWRK(IR01+MPLS+1)
              GO TO 10128
10127         CONTINUE
              IF (.NOT.(IINT.NE.NINT)) GO TO 10129
                CALL MSKRV2 (REAL(IINT)/REAL(NINT),XTMP,YTMP,NPLS,
     +                       RWRK(IR01+1),RWRK(IR01+MPLS+1),
     +                       RWRK(IR01+2*MPLS+1),RWRK(IR01+3*MPLS+1),
     +                       RWRK(IR01+4*MPLS+1),ABS(T2DS),0,DUMI)
                IF (ICFELL('CTTRCL',7).NE.0) GO TO 101
              GO TO 10128
10129         CONTINUE
                XTMP=RWRK(IR01+NPLS)
                YTMP=RWRK(IR01+MPLS+NPLS)
10128         CONTINUE
C
              NOUT=NOUT+1
              RWRK(IR01+5*MPLS+NOUT)=XTMP
              RWRK(IR01+6*MPLS+NOUT)=YTMP
C
              IF (.NOT.((IINT.EQ.NINT.OR.NOUT.EQ.MPLS).AND.NOUT.NE.0))
     +        GO TO 10130
                XTMP=RWRK(IR01+5*MPLS+NOUT)
                YTMP=RWRK(IR01+6*MPLS+NOUT)
                IJMP=1
                IRW1=IR01+5*MPLS
                IRW2=IR01+6*MPLS
                NRWK=NOUT
                RETURN
  102           RWRK(IR01+5*MPLS+1)=XTMP
                RWRK(IR01+6*MPLS+1)=YTMP
                NOUT=1
10130         CONTINUE
C
            GO TO 10124
10125       CONTINUE
C
            IF (.NOT.(IFSF.NE.0)) GO TO 10131
              IFSF=0
              XBFS=RWRK(IR01+1)
              YBFS=RWRK(IR01+MPLS+1)
              CALL MSKRV2 (0.,XTMP,YTMP,NPLS,RWRK(IR01+1),
     +                     RWRK(IR01+MPLS+1),RWRK(IR01+2*MPLS+1),
     +                     RWRK(IR01+3*MPLS+1),RWRK(IR01+4*MPLS+1),
     +                     ABS(T2DS),1,SBFS)
              IF (ICFELL('CTTRCL',8).NE.0) GO TO 101
10131       CONTINUE
C
            XELS=RWRK(IR01+NPLS)
            YELS=RWRK(IR01+MPLS+NPLS)
            CALL MSKRV2 (1.,XTMP,YTMP,NPLS,RWRK(IR01+1),
     +                   RWRK(IR01+MPLS+1),RWRK(IR01+2*MPLS+1),
     +                   RWRK(IR01+3*MPLS+1),RWRK(IR01+4*MPLS+1),
     +                   ABS(T2DS),1,SELS)
            IF (ICFELL('CTTRCL',9).NE.0) GO TO 101
C
C
10118   CONTINUE
C
  103   NPLS=0
        RUDN=0.
C
C Done.
C
      GO TO (10030,10035,10057,10065,10082,10088,10106) , L10031
C
      END
