C
C $Id: cttreg.f,v 1.2 2004-03-19 22:51:59 kennison Exp $
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
      SUBROUTINE CTTREG (RPNT,IEDG,ITRI,RWRK,IWRK,IJMP,IAIC,IRW1,IRW2,
     +                                                           NRWK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C This routine traces the edge of the grid.  Control is passed back to
C the caller with each piece of the edge for processing.
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
C caller is expected to process a segment and recall CTTREG.
C
C IAIC is both an input and an output variable.  If it is initially set
C to -9 by the caller, it will not be changed by CTTREG and no attempt
C will be made to determine what area identifier should be used for the
C area on the contoured side of the edge of the grid.  If its initial
C value is 0, it will have been updated, upon every return with IJMP
C non-zero, to the area identifier for the contoured side of the piece
C of the edge defined by IRW1, IRW2, and NRWK.
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
C Define an arithmetic statement function for use below.
C
      FRCT(ZDT1,ZDT2)=(CLEV(ICLV)-ZDT1)/(ZDT2-ZDT1)
C
C If this is a re-entry after coordinate processing by the caller, jump
C back to the appropriate point in the code.
C
      IF (IJMP.NE.0) GO TO (101,104,105,106,107,108) , IJMP
C
C Assign space to use for storing the X and Y coordinates of points.
C
      MPLS=LRWC
      CALL CTGRWS (RWRK,1,2*MPLS,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CTTREG',1).NE.0) GO TO 102
C
C Compute quantities used to see if two points are essentially
C different from one another.
C
      SMLX=.01*ABS(XWDR-XWDL)
      SMLY=.01*ABS(YWDT-YWDB)
C
C Compute quantities used in detecting jumps in the mapping.
C
      PITX=PITH*ABS(XWDR-XWDL)
      PITY=PITH*ABS(YWDT-YWDB)
C
C Zero the utility flags in all the edge nodes.  (They will be used to
C mark edges we've already visited.)
C
      DO 10001 IIII=0,NEDG-LOEN,LOEN
        IEDG(IIII+5)=0
10001 CONTINUE
C
C Loop through the edge list, searching for starting edges.
C
        IIII = 0
        GO TO 10004
10002   CONTINUE
        IIII =IIII +LOEN
10004   CONTINUE
        IF (LOEN) 10005,10006,10007
10005   CONTINUE
        IF (IIII .LT.(NEDG-LOEN)) GO TO 10003
        GO TO 10006
10007   CONTINUE
        IF (IIII .GT.(NEDG-LOEN)) GO TO 10003
10006   CONTINUE
C
C Use the edge only if it has not already been used.
C
        IF (.NOT.(IEDG(IIII+5).EQ.0)) GO TO 10008
C
C Set a flag saying whether or not there is a non-blocked triangle to
C the left of the edge.
C
          IFLL=0
C
          IF (.NOT.(IEDG(IIII+3).GE.0)) GO TO 10009
            IF (IAND(ITRI(LOTN*((IEDG(IIII+3)-1)/LOTN)+4),ITBM)
     +                                                   .EQ.0) IFLL=1
10009     CONTINUE
C
C Set a flag saying whether or not there is a non-blocked triangle to
C the right of the edge.
C
          IFLR=0
C
          IF (.NOT.(IEDG(IIII+4).GE.0)) GO TO 10010
            IF (IAND(ITRI(LOTN*((IEDG(IIII+4)-1)/LOTN)+4),ITBM)
     +                                                   .EQ.0) IFLR=1
10010     CONTINUE
C
C Use the edge only if it has a non-blocked triangle on one side of
C it, but not on the other side of it.
C
          IF (.NOT.(IFLL.NE.IFLR)) GO TO 10011
C
C Trace the portion of the external boundary starting with this edge.
C
            IPTE=IIII
C
C Let the first and second points of the edge be numbered IP12 and
C 3-IP12; initialize IP12 so that the triangle is on the left.
C
            IF (.NOT.(IFLL.NE.0)) GO TO 10012
              IP12=1
            GO TO 10013
10012       CONTINUE
              IP12=2
10013       CONTINUE
C
C Zero the number of points in the coordinate arrays and zero the
C variable that keeps track of the ratio of segment length in the
C user coordinate system to segment length in the data coordinate
C system.
C
            NPLS=0
            RUDN=0.
C
C Initialize the quantities needed by PROCESS-EDGE-SEGMENT.
C
            XCES=RPNT(IEDG(IPTE+IP12)+1)
            YCES=RPNT(IEDG(IPTE+IP12)+2)
            ZCES=RPNT(IEDG(IPTE+IP12)+3)
            DVES=RPNT(IEDG(IPTE+IP12)+4)
            XCND=XCES
            YCND=YCES
            ZCND=ZCES
            DVND=DVES
            L10015=    1
            GO TO 10015
10014       CONTINUE
C
C Return here with each new edge found by the algorithm.
C
10016       CONTINUE
C
C Mark the edge as used.
C
              IEDG(IPTE+5)=1
C
C Process it.
C
              L10018=    1
              GO TO 10018
10017         CONTINUE
C
C Find a continuation edge.
C
10019         CONTINUE
C
C Move to the next edge (in counterclockwise order) in the triangle to
C the left.
C
                IPTT=LOTN*((IEDG(IPTE+IP12+2)-1)/LOTN)
                IPTI=MOD(IEDG(IPTE+IP12+2)-IPTT,3)+1
                IPTX=ITRI(IPTT+IPTI)
C
C Make the end point of the possible new edge match the end point of the
C old one.
C
                IF (IEDG(IPTX+3-IP12).NE.IEDG(IPTE+3-IP12))
     +                                                     IP12=3-IP12
C
                IPTE=IPTX
C
C If there's a non-blocked triangle to the left of the possible new edge
C jump back to try the next edge in that triangle.
C
                IFLL=0
C
                IF (.NOT.(IEDG(IPTE+IP12+2).GE.0)) GO TO 10020
                  IF (IAND(ITRI(LOTN*((IEDG(IPTE+IP12+2)-1)/LOTN)+4),
     +                                              ITBM).EQ.0) IFLL=1
10020           CONTINUE
C
              IF (.NOT.(IFLL.EQ.0)) GO TO 10019
C
C If the edge found has been used before, quit the loop; otherwise, swap
C its end points and use it as a continuation of the edge being traced.
C
              IF (IEDG(IPTE+5).NE.0) GO TO 10021
C
              IP12=3-IP12
C
            GO TO 10016
10021       CONTINUE
C
C Dump anything left in the buffer.
C
            IF (.NOT.(NPLS.NE.0)) GO TO 10022
              IJMP=1
              IRW1=IR01
              IRW2=IR01+MPLS
              NRWK=NPLS
              RETURN
10022       CONTINUE
C
10011     CONTINUE
C
10008   CONTINUE
C
  101 CONTINUE
      GO TO 10002
10003 CONTINUE
C
C Release the real workspace and let the caller know we're done.
C
  102 LR01=0
      IJMP=0
C
C Done.
C
      RETURN
C
C The following internal procedure processes a segment along the edge
C of the grid.
C
10018 CONTINUE
        XCSS=XCES
        YCSS=YCES
        ZCSS=ZCES
        DVSS=DVES
        XCES=RPNT(IEDG(IPTE+3-IP12)+1)
        YCES=RPNT(IEDG(IPTE+3-IP12)+2)
        ZCES=RPNT(IEDG(IPTE+3-IP12)+3)
        DVES=RPNT(IEDG(IPTE+3-IP12)+4)
          INTP = 1
          GO TO 10025
10023     CONTINUE
          INTP =INTP +1
10025     CONTINUE
          IF (INTP .GT.(IABS(IPIE)+1)) GO TO 10024
          XCOD=XCND
          YCOD=YCND
          ZCOD=ZCND
          DVOD=DVND
          XCOU=XCNU
          YCOU=YCNU
          IVOU=IVNU
          IF (.NOT.(INTP.NE.IABS(IPIE)+1)) GO TO 10026
            FINT=REAL(INTP)/REAL(IABS(IPIE)+1)
            XCND=(1.-FINT)*XCSS+FINT*XCES
            YCND=(1.-FINT)*YCSS+FINT*YCES
            ZCND=(1.-FINT)*ZCSS+FINT*ZCES
            DVND=(1.-FINT)*DVSS+FINT*DVES
          GO TO 10027
10026     CONTINUE
            XCND=XCES
            YCND=YCES
            ZCND=ZCES
            DVND=DVES
10027     CONTINUE
          L10015=    2
          GO TO 10015
10028     CONTINUE
          IF (.NOT.(DVOD.LT.DVND)) GO TO 10029
              I = 1
              GO TO 10032
10030         CONTINUE
              I =I +1
10032         CONTINUE
              IF (I .GT.(NCLV)) GO TO 10031
              ICLV=ICLP(I)
              IF (.NOT.(CLEV(ICLV).GT.DVOD.AND.CLEV(ICLV).LT.DVND)) GO T
     +O 10033
                L10035=    1
                GO TO 10035
10034           CONTINUE
10033         CONTINUE
            GO TO 10030
10031       CONTINUE
          GO TO 10036
10029     CONTINUE
          IF (.NOT.(DVND.LT.DVOD)) GO TO 10037
              I = NCLV
              GO TO 10040
10038         CONTINUE
              I =I -1
10040         CONTINUE
              IF (I .LT.(1)) GO TO 10039
              ICLV=ICLP(I)
              IF (.NOT.(CLEV(ICLV).GT.DVND.AND.CLEV(ICLV).LT.DVOD)) GO T
     +O 10041
                L10035=    2
                GO TO 10035
10042           CONTINUE
10041         CONTINUE
            GO TO 10038
10039       CONTINUE
10036     CONTINUE
10037     CONTINUE
          IF (.NOT.(IPIE.LT.0.AND.INTP.NE.IABS(IPIE)+1)) GO TO 10043
            IFOP=0
          GO TO 10044
10043     CONTINUE
            IFOP=1
10044     CONTINUE
          L10046=    1
          GO TO 10046
10045     CONTINUE
        GO TO 10023
10024   CONTINUE
      GO TO (10017) , L10018
C
C The following internal procedure interpolates a point where a contour
C line intersects the piece of the edge segment that we're working on.
C We are careful to place these points exactly where they are placed by
C the routine CTTRCL, which makes the code look a little unnecessarily
C complicated.
C
10035 CONTINUE
        XCSD=XCND
        YCSD=YCND
        ZCSD=ZCND
        DVSD=DVND
        XCSU=XCNU
        YCSU=YCNU
        IVSU=IVNU
        IF (.NOT.(DVSS.LT.DVES)) GO TO 10047
          DFRA=FRCT(DVSS,DVES)
          IF (DFRA.LE..00001.OR.DFRA.GE..99999) GO TO 103
          XCND=XCSS+DFRA*(XCES-XCSS)
          YCND=YCSS+DFRA*(YCES-YCSS)
          ZCND=ZCSS+DFRA*(ZCES-ZCSS)
        GO TO 10048
10047   CONTINUE
          DFRA=FRCT(DVES,DVSS)
          IF (DFRA.LE..00001.OR.DFRA.GE..99999) GO TO 103
          XCND=XCES+DFRA*(XCSS-XCES)
          YCND=YCES+DFRA*(YCSS-YCES)
          ZCND=ZCES+DFRA*(ZCSS-ZCES)
10048   CONTINUE
        DVND=CLEV(ICLV)
        L10015=    3
        GO TO 10015
10049   CONTINUE
        IFOP=1
        L10046=    2
        GO TO 10046
10050   CONTINUE
        XCOD=XCND
        YCOD=YCND
        ZCOD=ZCND
        DVOD=DVND
        XCOU=XCNU
        YCOU=YCNU
        IVOU=IVNU
        XCND=XCSD
        YCND=YCSD
        ZCND=ZCSD
        DVND=DVSD
        XCNU=XCSU
        YCNU=YCSU
        IVNU=IVSU
  103 CONTINUE
      GO TO (10034,10042) , L10035
C
C The following internal procedure processes a piece of a segment; there
C are several cases, depending on whether both endpoints are visible,
C neither endpoint is visible, or just one of them is visible.
C
10046 CONTINUE
C
        IAID=IAIC
C
        IF (.NOT.(IAIC.NE.-9)) GO TO 10051
          IF (.NOT.(NCLV.LE.0)) GO TO 10052
            IAID=1
          GO TO 10053
10052     CONTINUE
            CALL CTGVAI (.5*(DVND+DVOD),IAID)
10053     CONTINUE
10051   CONTINUE
C
        IF (.NOT.(NPLS.EQ.0)) GO TO 10054
          IF (.NOT.(IVOU.NE.0)) GO TO 10055
            IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10056
              XCLD=XCOD
              YCLD=YCOD
              ZCLD=ZCOD
              XCLU=XCOU
              YCLU=YCOU
10056       CONTINUE
            RWRK(IR01+1)=XCOU
            RWRK(IR01+MPLS+1)=YCOU
            NPLS=1
          GO TO 10057
10055     CONTINUE
          IF (.NOT.(IVNU.NE.0)) GO TO 10058
            XCID=XCOD
            YCID=YCOD
            ZCID=ZCOD
            XCVD=XCND
            YCVD=YCND
            ZCVD=ZCND
            XCVU=XCNU
            YCVU=YCNU
            L10060=    1
            GO TO 10060
10059       CONTINUE
            L10062=    1
            GO TO 10062
10061       CONTINUE
            XCOD=XCVD
            YCOD=YCVD
            ZCOD=ZCVD
            XCOU=XCVU
            YCOU=YCVU
            IVOU=1
10057     CONTINUE
10058     CONTINUE
        GO TO 10063
10054   CONTINUE
        IF (.NOT.(NPLS.EQ.MPLS.OR.IAID.NE.IAIC)) GO TO 10064
          XSAV=RWRK(IR01+NPLS)
          YSAV=RWRK(IR01+MPLS+NPLS)
          IJMP=2
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  104     RWRK(IR01+1)=XSAV
          RWRK(IR01+MPLS+1)=YSAV
          NPLS=1
10063   CONTINUE
10064   CONTINUE
C
        IAIC=IAID
C
        IF (.NOT.(IVNU.NE.0)) GO TO 10065
          L10067=    1
          GO TO 10067
10066     CONTINUE
        GO TO 10068
10065   CONTINUE
        IF (.NOT.(IVOU.NE.0)) GO TO 10069
          XCVD=XCOD
          YCVD=YCOD
          ZCVD=ZCOD
          XCVU=XCOU
          YCVU=YCOU
          XCID=XCND
          YCID=YCND
          ZCID=ZCND
          L10060=    2
          GO TO 10060
10070     CONTINUE
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
          IFOP=1
          L10067=    2
          GO TO 10067
10071     CONTINUE
          XCND=XKND
          YCND=YKND
          ZCND=ZKND
          XCNU=XKNU
          YCNU=YKNU
          IJMP=3
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  105     NPLS=0
          RUDN=0.
10068   CONTINUE
10069   CONTINUE
C
      GO TO (10045,10050) , L10046
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10067 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.(XCND.NE.XCOD.OR.YCND.NE.YCOD.OR.ZCND.NE
     +.ZCOD))) GO TO 10072
          RUDO=RUDN
          RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +         (ABS(XCND-XCOD)+ABS(YCND-YCOD)+ABS(ZCND-ZCOD))
          IF (.NOT.(RUDN.GT.2.*RUDO)) GO TO 10073
            IFOP=1
            L10075=    1
            GO TO 10075
10074       CONTINUE
10073     CONTINUE
          IF (.NOT.(PITH.GT.0.)) GO TO 10076
            XCTD=XCND
            YCTD=YCND
            ZCTD=ZCND
            XCTU=XCNU
            YCTU=YCNU
            L10078=    1
            GO TO 10078
10077       CONTINUE
10076     CONTINUE
10072   CONTINUE
        IF (.NOT.(IFOP.NE.0)) GO TO 10079
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XCNU
          RWRK(IR01+MPLS+NPLS)=YCNU
10079   CONTINUE
      GO TO (10066,10071) , L10067
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the edge is seen.  It checks
C for a possible discontinuity in the mapping function (as can happen,
C for example, when a cylindrical equidistant projection is being used);
C if there is such a discontinuity, we must generate a final point on
C one side of it, dump the polyline, and then start a new polyline on
C the other side.
C
10075 CONTINUE
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
10080   CONTINUE
          DSTO=ABS(XC2U-XC1U)+ABS(YC2U-YC1U)
          XC3D=(XC1D+XC2D)/2.
          YC3D=(YC1D+YC2D)/2.
          ZC3D=(ZC1D+ZC2D)/2.
          CALL HLUCTMXYZ (IMPF,XC3D,YC3D,ZC3D,XC3U,YC3U)
          IF (ICFELL('CTTREG',2).NE.0) GO TO 102
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10081
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10082
              ITMP=1000
              GO TO 10083
10082       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10084
              IF (XC3D.EQ.XC1D.AND.YC3D.EQ.YC1D.AND.ZC3D.EQ.ZC1D) GO TO
     +10083
              XC1D=XC3D
              YC1D=YC3D
              ZC1D=ZC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10085
10084       CONTINUE
              IF (XC3D.EQ.XC2D.AND.YC3D.EQ.YC2D.AND.ZC3D.EQ.ZC2D) GO TO
     +10083
              XC2D=XC3D
              YC2D=YC3D
              ZC2D=ZC3D
              XC2U=XC3U
              YC2U=YC3U
10085       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10083
          GO TO 10086
10081     CONTINUE
            XCVD=XCOD
            YCVD=YCOD
            ZCVD=ZCOD
            XCVU=XCOU
            YCVU=YCOU
            XCID=XC3D
            YCID=YC3D
            ZCID=ZC3D
            L10060=    3
            GO TO 10060
10087       CONTINUE
            L10062=    2
            GO TO 10062
10088       CONTINUE
            IJMP=4
            IRW1=IR01
            IRW2=IR01+MPLS
            NRWK=NPLS
            RETURN
  106       NPLS=0
            RUDN=0.
            XCID=XC3D
            YCID=YC3D
            ZCID=ZC3D
            XCVD=XCND
            YCVD=YCND
            ZCVD=ZCND
            XCVU=XCNU
            YCVU=YCNU
            L10060=    4
            GO TO 10060
10089       CONTINUE
            L10062=    3
            GO TO 10062
10090       CONTINUE
            ITMP=1000
            GO TO 10083
10086     CONTINUE
        GO TO 10080
10083   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10091
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10092
            XCTD=XC1D
            YCTD=YC1D
            ZCTD=ZC1D
            XCTU=XC1U
            YCTU=YC1U
            L10078=    2
            GO TO 10078
10093       CONTINUE
10092     CONTINUE
          NPLS=NPLS+1
          RWRK(IR01+NPLS)=XC1U
          RWRK(IR01+MPLS+NPLS)=YC1U
          IJMP=5
          IRW1=IR01
          IRW2=IR01+MPLS
          NRWK=NPLS
          RETURN
  107     CONTINUE
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10094
            XCLD=XC2D
            YCLD=YC2D
            ZCLD=ZC2D
            XCLU=XC2U
            YCLU=YC2U
10094     CONTINUE
          RWRK(IR01+1)=XC2U
          RWRK(IR01+MPLS+1)=YC2U
          NPLS=1
          RUDN=0.
10091   CONTINUE
      GO TO (10074) , L10075
C
C Given two points in the data coordinate system, one of which maps to
C a visible point and the other of which maps to an invisible point,
C this internal routine searches the line between them for a point near
C the edge of visibility.
C
10060 CONTINUE
        ITMP=0
10095   CONTINUE
          XCHD=(XCVD+XCID)/2.
          YCHD=(YCVD+YCID)/2.
          ZCHD=(ZCVD+ZCID)/2.
          CALL HLUCTMXYZ (IMPF,XCHD,YCHD,ZCHD,XCHU,YCHU)
          IF (ICFELL('CTTREG',3).NE.0) GO TO 102
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10096
            IF (XCHD.EQ.XCVD.AND.YCHD.EQ.YCVD.AND.ZCHD.EQ.ZCVD) GO TO 10
     +097
            XCVD=XCHD
            YCVD=YCHD
            ZCVD=ZCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10098
10096     CONTINUE
            IF (XCHD.EQ.XCID.AND.YCHD.EQ.YCID.AND.ZCHD.EQ.ZCID) GO TO 10
     +097
            XCID=XCHD
            YCID=YCHD
            ZCID=ZCHD
10098     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10097
        GO TO 10095
10097   CONTINUE
      GO TO (10059,10070,10087,10089) , L10060
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10062 CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10099
          IF (.NOT.(NPLS.EQ.0)) GO TO 10100
            XCLD=XCVD
            YCLD=YCVD
            ZCLD=ZCVD
            XCLU=XCVU
            YCLU=YCVU
          GO TO 10101
10100     CONTINUE
            XCTD=XCVD
            YCTD=YCVD
            ZCTD=ZCVD
            XCTU=XCVU
            YCTU=YCVU
            L10078=    3
            GO TO 10078
10102       CONTINUE
10101     CONTINUE
10099   CONTINUE
        NPLS=NPLS+1
        RWRK(IR01+NPLS)=XCVU
        RWRK(IR01+MPLS+NPLS)=YCVU
      GO TO (10061,10088,10090) , L10062
C
C The following internal procedure is invoked when mapping is being
C done and a new point is about to be added to the polyline buffer.
C It checks for a jump greater than a user-defined threshold value in
C the mapped coordinates of the point and, if such a jump is found,
C interpolates some points in between.  The assumption is made that
C all points in between are visible; if that is found not to be the
C case, no attempt is made to rectify the situation: the user probably
C screwed up the definition of the mapping function.
C
10078 CONTINUE
10103   CONTINUE
        IF (.NOT.(ABS(XCTU-XCLU).GT.PITX.OR.ABS(YCTU-YCLU).GT.PITY))
     +  GO TO 10104
          IFND=0
          XCQD=0.
          YCQD=0.
          ZCQD=0.
          RDST=.50
          RSTP=.25
10105     CONTINUE
            XCPD=XCLD+RDST*(XCTD-XCLD)
            YCPD=YCLD+RDST*(YCTD-YCLD)
            ZCPD=ZCLD+RDST*(ZCTD-ZCLD)
            CALL HLUCTMXYZ (IMPF,XCPD,YCPD,ZCPD,XCPU,YCPU)
            IF (ICFELL('CTTREG',4).NE.0) GO TO 102
            IF (OORV.NE.0..AND.(XCPU.EQ.OORV.OR.YCPU.EQ.OORV)) GO TO 101
     +06
            IF (.NOT.(ABS(XCPU-XCLU).LT.PITX.AND.ABS(YCPU-YCLU).LT.PITY)
     +)     GO TO 10107
              IFND=1
              XCQD=XCPD
              YCQD=YCPD
              ZCQD=ZCPD
              XCQU=XCPU
              YCQU=YCPU
              IF (ABS(XCQU-XCLU).GT..5*PITX.OR.ABS(YCQU-YCLU).GT..5*PITY
     +)       GO TO 10106
              RDST=RDST+RSTP
            GO TO 10108
10107       CONTINUE
              RDST=RDST-RSTP
10108       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..001) GO TO 10106
          GO TO 10105
10106     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(XCQD.NE.XCLD.OR.YCQD.NE.YCLD.OR.ZCQD.
     +NE.ZCLD))) GO TO 10109
            IFOP=1
            NPLS=NPLS+1
            RWRK(IR01+NPLS)=XCQU
            RWRK(IR01+MPLS+NPLS)=YCQU
            IF (.NOT.(NPLS.EQ.MPLS)) GO TO 10110
              XSAV=RWRK(IR01+NPLS)
              YSAV=RWRK(IR01+MPLS+NPLS)
              IJMP=6
              IRW1=IR01
              IRW2=IR01+MPLS
              NRWK=NPLS
              RETURN
  108         RWRK(IR01+1)=XSAV
              RWRK(IR01+MPLS+1)=YSAV
              NPLS=1
10110       CONTINUE
            XCLD=XCQD
            YCLD=YCQD
            ZCLD=ZCQD
            XCLU=XCQU
            YCLU=YCQU
          GO TO 10111
10109     CONTINUE
            XCLD=XCTD
            YCLD=YCTD
            ZCLD=ZCTD
            XCLU=XCTU
            YCLU=YCTU
10111     CONTINUE
        GO TO 10103
10104   CONTINUE
        XCLD=XCTD
        YCLD=YCTD
        ZCLD=ZCTD
        XCLU=XCTU
        YCLU=YCTU
      GO TO (10077,10093,10102) , L10078
C
C The following internal procedure is given the data-system coordinates
C of a point (XCND,YCND,ZCND) and computes the user-system coordinates
C of the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10015 CONTINUE
C
        IF (.NOT.(IMPF.EQ.0)) GO TO 10112
          XCNU=XCND
          YCNU=YCND
          IVNU=1
        GO TO 10113
10112   CONTINUE
          CALL HLUCTMXYZ (IMPF,XCND,YCND,ZCND,XCNU,YCNU)
          IF (ICFELL('CTTRCL',5).NE.0) GO TO 102
          IF (.NOT.((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)))
     +    GO TO 10114
            IVNU=0
          GO TO 10115
10114     CONTINUE
            IVNU=1
10115     CONTINUE
10113   CONTINUE
C
      GO TO (10014,10028,10049) , L10015
C
      END
