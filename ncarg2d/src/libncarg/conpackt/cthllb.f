C
C $Id: cthllb.f,v 1.1 2003-05-28 15:44:30 kennison Exp $
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
      SUBROUTINE CTHLLB (RPNT,IEDG,ITRI,RWRK,IWRK)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C CTHLLB generates the high and low labels for the contour field; the
C quantities defining the labels are added to the lists in real
C workspaces 3 and 4.
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
C If the text strings for high and low labels are blank, do nothing.
C
      IF (TXHI(1:LTHI).EQ.' '.AND.TXLO(1:LTLO).EQ.' ') RETURN
C
C Compute the value of the angle at which the labels are written, in
C radians, and the sine and cosine of that angle.
C
      ANLB=.017453292519943*ANHL
      SALB=SIN(ANLB)
      CALB=COS(ANLB)
C
C Compute the width of a character in the fractional system and the
C width of the white space in the fractional system.
C
      WCFS=CHWM*WCHL*(XVPR-XVPL)
      WWFS=CHWM*WWHL*(XVPR-XVPL)
C
C Make PLOTCHAR compute text-extent quantities.
C
      CALL PCGETI ('TE',ITMP)
      IF (ICFELL('CTHLLB',1).NE.0) RETURN
      CALL PCSETI ('TE',1)
      IF (ICFELL('CTHLLB',2).NE.0) RETURN
C
C Compute the square of the specified high/low search radius, which
C will be needed below.
C
      IF (HLSR.LT.0.) THEN
        HLRS=HLSR*HLSR
      ELSE
        HLRS=HLSR*HLSR*MAX(XMAX-XMIN,YMAX-YMIN,ZMAX-ZMIN)**2
      END IF
C
C Tell IFTRAN to use the FORTRAN-66 implementation of BLOCK invocations.
C
C
C Look for highs in the data field.  The algorithm takes advantage
C of two facts: 1) each edge is essentially a directed vector
C pointing uphill; and 2) the edges of each triangle are defined
C in counterclockwise order.
C
      IF (.NOT.(TXHI(1:LTHI).NE.' ')) GO TO 10001
C
C Zero the utility flags in all the edge nodes.  (They will be used to
C mark edges we've already visited.)
C
        DO 10002 I=0,NEDG-LOEN,LOEN
          IEDG(I+5)=0
10002   CONTINUE
C
C Loop through the edge list, searching for starting edges.
C
          I = 0
          GO TO 10005
10003     CONTINUE
          I =I +LOEN
10005     CONTINUE
          IF (LOEN) 10006,10007,10008
10006     CONTINUE
          IF (I .LT.(NEDG-LOEN)) GO TO 10004
          GO TO 10007
10008     CONTINUE
          IF (I .GT.(NEDG-LOEN)) GO TO 10004
10007     CONTINUE
C
C Skip the edge if it has already been used.
C
          IF (IEDG(I+5).NE.0) GO TO 104
C
C Otherwise, construct a path of connected edges along which the field
C values increase, and keep going until a high point is reached.
C
          IPTE=I
C
C Control loops back here to search for the best edge to follow edge
C IPTE.  First, mark the edge as used.
C
  101     IEDG(IPTE+5)=1
C
C IPTA is used for edges under consideration.
C
          IPTA=IPTE
C
C Skip the edge if it has no non-blocked triangle on either its left or
C its right.
C
          IFLL=0
C
          IF (.NOT.(IEDG(IPTA+3).GE.0)) GO TO 10009
            IF (ITRI(LOTN*((IEDG(IPTA+3)-1)/LOTN)+4).EQ.0) IFLL=1
10009     CONTINUE
C
          IFLR=0
C
          IF (.NOT.(IEDG(IPTA+4).GE.0)) GO TO 10010
            IF (ITRI(LOTN*((IEDG(IPTA+4)-1)/LOTN)+4).EQ.0) IFLR=1
10010     CONTINUE
C
          IF (IFLL.EQ.0.AND.IFLR.EQ.0) GO TO 104
C
C IPTB is used for the best following edge found so far.
C
          IPTB=IPTE
C
C There are two ways we can look for edges connected to the end of the
C edge IPTE.  We first look for them in clockwise order; if that search
C terminates by running into an external edge of the triangular mesh, we
C search for the rest in counterclockwise order; the flag IMEE is set
C non-zero to cause the second search to be done.
C
          IMEE=0
C
C If edge IPTA ends at the same point as edge IPTE,
C
  102     CONTINUE
          IF (.NOT.(IEDG(IPTA+2).EQ.IEDG(IPTE+2))) GO TO 10011
C
C mark it as used;
C
            IEDG(IPTA+5)=1
C
C if there is a non-blocked triangle to its left,
C
            IFLL=0
C
            IF (.NOT.(IEDG(IPTA+3).GE.0)) GO TO 10012
              IF (ITRI(LOTN*((IEDG(IPTA+3)-1)/LOTN)+4).EQ.0) IFLL=1
10012       CONTINUE
C
            IF (.NOT.(IFLL.NE.0)) GO TO 10013
C
C move to its next edge,
C
              IPTT=LOTN*((IEDG(IPTA+3)-1)/LOTN)
              IPTI=MOD(IEDG(IPTA+3)-IPTT,3)+1
              IPTA=ITRI(IPTT+IPTI)
C
C and, if that edge is not the one we started with, loop back to
C continue the search;
C
              IF (IPTA.NE.IPTE) GO TO 102
C
C otherwise (no non-blocked triangle to left),
C
            GO TO 10014
10013       CONTINUE
C
C search in the other direction.
C
              IMEE=1
C
10014       CONTINUE
C
C If edge IPTA ends with a different point than edge IPTE does,
C
          GO TO 10015
10011     CONTINUE
C
C a possible following edge has been found; update the "best" pointer.
C
            IF (RPNT(IEDG(IPTA+2)+4).GT.
     +          RPNT(IEDG(IPTB+2)+4)) IPTB=IPTA
C
C If there is a non-blocked triangle to the right, move to its next
C edge; otherwise, search in the opposite direction.
C
            IFLR=0
C
            IF (.NOT.(IEDG(IPTA+4).GE.0)) GO TO 10016
              IF (ITRI(LOTN*((IEDG(IPTA+4)-1)/LOTN)+4).EQ.0) IFLR=1
10016       CONTINUE
C
            IF (.NOT.(IFLR.NE.0)) GO TO 10017
              IPTT=LOTN*((IEDG(IPTA+4)-1)/LOTN)
              IPTI=MOD(IEDG(IPTA+4)-IPTT,3)+1
              IPTA=ITRI(IPTT+IPTI)
              IF (IPTA.NE.IPTE) GO TO 102
            GO TO 10018
10017       CONTINUE
              IMEE=1
10018       CONTINUE
C
10015     CONTINUE
C
C If the mesh edge was encountered while searching in one direction,
C
          IF (.NOT.(IMEE.NE.0)) GO TO 10019
C
C look at triangles in the other direction from edge IPTE.
C
            IPTA=IPTE
C
C If edge IPTA ends with the same point as edge IPTE,
C
  103       CONTINUE
            IF (.NOT.(IEDG(IPTA+2).EQ.IEDG(IPTE+2))) GO TO 10020
C
C mark it as used;
C
              IEDG(IPTA+5)=1
C
C if there is a non-blocked triangle to its right,
C
              IFLR=0
C
              IF (.NOT.(IEDG(IPTA+4).GE.0)) GO TO 10021
                IF (ITRI(LOTN*((IEDG(IPTA+4)-1)/LOTN)+4).EQ.0) IFLR=1
10021         CONTINUE
C
              IF (.NOT.(IFLR.NE.0)) GO TO 10022
C
C move to its previous edge,
C
                IPTT=LOTN*((IEDG(IPTA+4)-1)/LOTN)
                IPTI=MOD(IEDG(IPTA+4)-IPTT+1,3)+1
                IPTA=ITRI(IPTT+IPTI)
C
C and, if that edge is not the one we started with, loop back to
C continue the search.
C
                IF (IPTA.NE.IPTE) GO TO 103
C
10022         CONTINUE
C
C If edge IPTA ends with a different point than edge IPTE does,
C
            GO TO 10023
10020       CONTINUE
C
C a possible following edge has been found; update the "best" pointer.
C
              IF (RPNT(IEDG(IPTA+2)+4).GT.
     +            RPNT(IEDG(IPTB+2)+4)) IPTB=IPTA
C
C If there is a non-blocked triangle to the left, move to its previous
C edge.
C
              IFLL=0
C
              IF (.NOT.(IEDG(IPTA+3).GE.0)) GO TO 10024
                IF (ITRI(LOTN*((IEDG(IPTA+3)-1)/LOTN)+4).EQ.0) IFLL=1
10024         CONTINUE
C
              IF (.NOT.(IFLL.NE.0)) GO TO 10025
                IPTT=LOTN*((IEDG(IPTA+3)-1)/LOTN)
                IPTI=MOD(IEDG(IPTA+3)-IPTT+1,3)+1
                IPTA=ITRI(IPTT+IPTI)
                IF (IPTA.NE.IPTE) GO TO 103
10025         CONTINUE
C
10023       CONTINUE
C
10019     CONTINUE
C
C If a following edge was found,
C
          IF (.NOT.(IPTB.NE.IPTE)) GO TO 10026
C
C and it's one we've used before, skip it (because, if we continued,
C we'd only arrive at a high we found already).
C
            IF (IEDG(IPTB+5).NE.0) GO TO 104
C
C Otherwise, move to it and loop back to continue the search.
C
            IPTE=IPTB
            GO TO 101
C
C If no following edge was found and the endpoint was not on an
C external edge of the mesh,
C
10026     CONTINUE
          IF (.NOT.(IMEE.EQ.0)) GO TO 10027
C
C we have found a possible high, so look at field values at all points
C within a specified distance; if any are found that are greater than
C or equal to the field value at the possible high, skip it.
C
            VAPH=RPNT(IEDG(IPTE+2)+4)
C
            IF (RPNT(IEDG(IPTE+1)+4).EQ.VAPH) GO TO 104
C
            DO 10028 J=0,NPNT-LOPN,LOPN
              IF (.NOT.(J.NE.IEDG(IPTE+2))) GO TO 10029
                IF (.NOT.((RPNT(J+1)-RPNT(IEDG(IPTE+2)+1))**2+(RPNT(J+2)
     +-RPNT(IEDG(IPTE+2)+2))**2+(RPNT(J+3)-RPNT(IEDG(IPTE+2)+3))**2.LT.H
     +LRS))     GO TO 10030
                  IF (RPNT(J+4).GE.VAPH) GO TO 104
10030           CONTINUE
10029         CONTINUE
10028       CONTINUE
C
C Otherwise, mark the high.
C
            IHOL=0
            XTMP=RPNT(IEDG(IPTE+2)+1)
            YTMP=RPNT(IEDG(IPTE+2)+2)
            ZTMP=RPNT(IEDG(IPTE+2)+3)
            DVAL=RPNT(IEDG(IPTE+2)+4)
            L10032=    1
            GO TO 10032
10031       CONTINUE
C
10027     CONTINUE
C
  104   CONTINUE
        GO TO 10003
10004   CONTINUE
C
10001 CONTINUE
C
C Look for lows in the data field.  The algorithm takes advantage
C of two facts: 1) each edge is essentially a directed vector
C pointing uphill; and 2) the edges of each triangle are defined
C in counterclockwise order.
C
      IF (.NOT.(TXLO(1:LTLO).NE.' ')) GO TO 10033
C
C Zero the utility flags in all the edge nodes.  (They will be used to
C mark edges we've already visited.)
C
        DO 10034 I=0,NEDG-LOEN,LOEN
          IEDG(I+5)=0
10034   CONTINUE
C
C Loop through the edge list, searching for starting edges.
C
          I = 0
          GO TO 10037
10035     CONTINUE
          I =I +LOEN
10037     CONTINUE
          IF (LOEN) 10038,10039,10040
10038     CONTINUE
          IF (I .LT.(NEDG-LOEN)) GO TO 10036
          GO TO 10039
10040     CONTINUE
          IF (I .GT.(NEDG-LOEN)) GO TO 10036
10039     CONTINUE
C
C Skip the edge if it has already been used.
C
          IF (IEDG(I+5).NE.0) GO TO 108
C
C Otherwise, construct a path of connected edges along which the field
C values decrease, and keep going until a low point is reached.
C
          IPTE=I
C
C Control loops back here to search for the best edge to precede edge
C IPTE.  First, mark the edge as used.
C
  105     IEDG(IPTE+5)=1
C
C IPTA is used for edges under consideration.
C
          IPTA=IPTE
C
C Skip the edge if it has no non-blocked triangle on either its left or
C its right.
C
          IFLL=0
C
          IF (.NOT.(IEDG(IPTA+3).GE.0)) GO TO 10041
            IF (ITRI(LOTN*((IEDG(IPTA+3)-1)/LOTN)+4).EQ.0) IFLL=1
10041     CONTINUE
C
          IFLR=0
C
          IF (.NOT.(IEDG(IPTA+4).GE.0)) GO TO 10042
            IF (ITRI(LOTN*((IEDG(IPTA+4)-1)/LOTN)+4).EQ.0) IFLR=1
10042     CONTINUE
C
          IF (IFLL.EQ.0.AND.IFLR.EQ.0) GO TO 108
C
C IPTB is used for the best preceding edge found so far.
C
          IPTB=IPTE
C
C There are two ways we can look for edges connected to the start of the
C edge IPTE.  We first look for them in clockwise order; if that search
C terminates by running into an external edge of the triangular mesh, we
C search for the rest in counterclockwise order; the flag IMEE is set
C non-zero to cause the second search to be done.
C
          IMEE=0
C
C If edge IPTA begins at the same point as edge IPTE,
C
  106     CONTINUE
          IF (.NOT.(IEDG(IPTA+1).EQ.IEDG(IPTE+1))) GO TO 10043
C
C mark it as used;
C
            IEDG(IPTA+5)=1
C
C if there is a non-blocked triangle to its right,
C
            IFLR=0
C
            IF (.NOT.(IEDG(IPTA+4).GE.0)) GO TO 10044
              IF (ITRI(LOTN*((IEDG(IPTA+4)-1)/LOTN)+4).EQ.0) IFLR=1
10044       CONTINUE
C
            IF (.NOT.(IFLR.NE.0)) GO TO 10045
C
C move to its next edge,
C
              IPTT=LOTN*((IEDG(IPTA+4)-1)/LOTN)
              IPTI=MOD(IEDG(IPTA+4)-IPTT,3)+1
              IPTA=ITRI(IPTT+IPTI)
C
C and, if that edge is not the one we started with, loop back to
C continue the search;
C
              IF (IPTA.NE.IPTE) GO TO 106
C
C otherwise (no non-blocked triangle to right),
C
            GO TO 10046
10045       CONTINUE
C
C search in the other direction.
C
              IMEE=1
C
10046       CONTINUE
C
C If edge IPTA begins with a different point than edge IPTE does,
C
          GO TO 10047
10043     CONTINUE
C
C a possible preceding edge has been found; update the "best" pointer.
C
            IF (RPNT(IEDG(IPTA+1)+4).LT.
     +          RPNT(IEDG(IPTB+1)+4)) IPTB=IPTA
C
C If there is a non-blocked triangle to the left, move to its next
C edge; otherwise, search in the opposite direction.
C
            IFLL=0
C
            IF (.NOT.(IEDG(IPTA+3).GE.0)) GO TO 10048
              IF (ITRI(LOTN*((IEDG(IPTA+3)-1)/LOTN)+4).EQ.0) IFLL=1
10048       CONTINUE
C
            IF (.NOT.(IFLL.NE.0)) GO TO 10049
              IPTT=LOTN*((IEDG(IPTA+3)-1)/LOTN)
              IPTI=MOD(IEDG(IPTA+3)-IPTT,3)+1
              IPTA=ITRI(IPTT+IPTI)
              IF (IPTA.NE.IPTE) GO TO 106
            GO TO 10050
10049       CONTINUE
              IMEE=1
10050       CONTINUE
C
10047     CONTINUE
C
C If the mesh edge was encountered while searching in one direction,
C
          IF (.NOT.(IMEE.NE.0)) GO TO 10051
C
C look at triangles in the other direction from edge IPTE.
C
            IPTA=IPTE
C
C If edge IPTA begins with the same point as edge IPTE,
C
  107       CONTINUE
            IF (.NOT.(IEDG(IPTA+1).EQ.IEDG(IPTE+1))) GO TO 10052
C
C mark it as used;
C
              IEDG(IPTA+5)=1
C
C if there is a non-blocked triangle to its left,
C
              IFLL=0
C
              IF (.NOT.(IEDG(IPTA+3).GE.0)) GO TO 10053
                IF (ITRI(LOTN*((IEDG(IPTA+3)-1)/LOTN)+4).EQ.0) IFLL=1
10053         CONTINUE
C
              IF (.NOT.(IFLL.NE.0)) GO TO 10054
C
C move to its previous edge,
C
                IPTT=LOTN*((IEDG(IPTA+3)-1)/LOTN)
                IPTI=MOD(IEDG(IPTA+3)-IPTT+1,3)+1
                IPTA=ITRI(IPTT+IPTI)
C
C and, if that edge is not the one we started with, loop back to
C continue the search.
C
                IF (IPTA.NE.IPTE) GO TO 107
C
10054         CONTINUE
C
C If edge IPTA begins with a different point than edge IPTE does,
C
            GO TO 10055
10052       CONTINUE
C
C a possible preceding edge has been found; update the "best" pointer.
C
              IF (RPNT(IEDG(IPTA+1)+4).LT.
     +            RPNT(IEDG(IPTB+1)+4)) IPTB=IPTA
C
C If there is a non-blocked triangle to the right, move to its previous
C edge.
C
              IFLR=0
C
              IF (.NOT.(IEDG(IPTA+4).GE.0)) GO TO 10056
                IF (ITRI(LOTN*((IEDG(IPTA+4)-1)/LOTN)+4).EQ.0) IFLR=1
10056         CONTINUE
C
              IF (.NOT.(IFLR.NE.0)) GO TO 10057
                IPTT=LOTN*((IEDG(IPTA+4)-1)/LOTN)
                IPTI=MOD(IEDG(IPTA+4)-IPTT+1,3)+1
                IPTA=ITRI(IPTT+IPTI)
                IF (IPTA.NE.IPTE) GO TO 107
10057         CONTINUE
C
10055       CONTINUE
C
10051     CONTINUE
C
C If a preceding edge was found,
C
          IF (.NOT.(IPTB.NE.IPTE)) GO TO 10058
C
C and it's one we've used before, skip it (because, if we continued,
C we'd only arrive at a low we found already).
C
            IF (IEDG(IPTB+5).NE.0) GO TO 108
C
C Otherwise, move to it and loop back to continue the search.
C
            IPTE=IPTB
            GO TO 105
C
C If no preceding edge was found and the endpoint was not on an
C external edge of the mesh,
C
10058     CONTINUE
          IF (.NOT.(IMEE.EQ.0)) GO TO 10059
C
C we have found a possible low, so look at field values at all points
C within a specified distance; if any are found that are less than or
C equal to the field value at the possible high, skip it.
C
            VAPL=RPNT(IEDG(IPTE+1)+4)
C
            IF (RPNT(IEDG(IPTE+2)+4).EQ.VAPL) GO TO 108
C
            DO 10060 J=0,NPNT-LOPN,LOPN
              IF (.NOT.(J.NE.IEDG(IPTE+1))) GO TO 10061
                IF (.NOT.((RPNT(J+1)-RPNT(IEDG(IPTE+1)+1))**2+(RPNT(J+2)
     +-RPNT(IEDG(IPTE+1)+2))**2+(RPNT(J+3)-RPNT(IEDG(IPTE+1)+3))**2.LT.H
     +LRS))     GO TO 10062
                  IF (RPNT(J+4).LE.VAPL) GO TO 108
10062           CONTINUE
10061         CONTINUE
10060       CONTINUE
C
C Otherwise, mark the low.
C
            IHOL=1
            XTMP=RPNT(IEDG(IPTE+1)+1)
            YTMP=RPNT(IEDG(IPTE+1)+2)
            ZTMP=RPNT(IEDG(IPTE+1)+3)
            DVAL=RPNT(IEDG(IPTE+1)+4)
            L10032=    2
            GO TO 10032
10063       CONTINUE
C
10059     CONTINUE
C
  108   CONTINUE
        GO TO 10035
10036   CONTINUE
C
10033 CONTINUE
C
C Tell IFTRAN to use the FORTRAN-77 implementation of BLOCK invocations.
C
C
C Discard any integer workspace that may have been used above.
C
  109 LI01=0
C
C Return PLOTCHAR to its default state.
C
      CALL PCSETI ('TE',ITMP)
      IF (ICFELL('CTHLLB',3).NE.0) RETURN
C
C Done.
C
      RETURN
C
C The following internal procedure writes a high (if IHOL=0) or low (if
C IHOL=1) label, centered at the point whose subscript coordinates are
C IPNT and JPNT.
C
10032 CONTINUE
C
        IF (IMPF.EQ.0) THEN
          XLBC=XTMP
          YLBC=YTMP
          IVIS=1
        ELSE
          CALL HLUCTMXYZ (IMPF,XTMP,YTMP,ZTMP,XLBC,YLBC)
          IF (ICFELL('CTHLLB',4).NE.0) RETURN
          IF ((OORV.NE.0.).AND.(XLBC.EQ.OORV.OR.YLBC.EQ.OORV)) THEN
            IVIS=0
          ELSE
            IVIS=1
          END IF
        END IF
C
        IF (IVIS.NE.0) THEN
          XCLB=CUFX(XLBC)
          IF (ICFELL('CTHLLB',5).NE.0) RETURN
          YCLB=CUFY(YLBC)
          IF (ICFELL('CTHLLB',6).NE.0) RETURN
          IF (IHOL.EQ.0) THEN
            CALL CTSBST(TXHI(1:LTHI),CTMA,LCTM)
          ELSE
            CALL CTSBST(TXLO(1:LTLO),CTMA,LCTM)
          END IF
          CALL HLUCTCHHL (+1+4*IHOL)
          IF (ICFELL('CTHLLB',7).NE.0) RETURN
          IF (CTMA(1:LCTM).EQ.' ') GO TO 110
          CALL PLCHHQ (XLBC,YLBC,CTMA(1:LCTM),WCFS,360.,0.)
          IF (ICFELL('CTHLLB',8).NE.0) RETURN
          CALL HLUCTCHHL (-1-4*IHOL)
          IF (ICFELL('CTHLLB',9).NE.0) RETURN
          CALL PCGETR ('DL',DTOL)
          IF (ICFELL('CTHLLB',10).NE.0) RETURN
          CALL PCGETR ('DR',DTOR)
          IF (ICFELL('CTHLLB',11).NE.0) RETURN
          CALL PCGETR ('DB',DTOB)
          IF (ICFELL('CTHLLB',12).NE.0) RETURN
          CALL PCGETR ('DT',DTOT)
          IF (ICFELL('CTHLLB',13).NE.0) RETURN
          DTOL=DTOL+WWFS
          DTOR=DTOR+WWFS
          DTOB=DTOB+WWFS
          DTOT=DTOT+WWFS
          XTRA=.5*CHWM*WCHL*(XVPR-XVPL)
          DSTL=DTOL+XTRA
          DSTR=DTOR+XTRA
          DSTB=DTOB+XTRA
          DSTT=DTOT+XTRA
C
          IF (IOHL.NE.0) THEN
C
            IF (ANLB.EQ.0.) THEN
              XLLB=XCLB-DSTL
              XRLB=XCLB+DSTR
              YBLB=YCLB-DSTB
              YTLB=YCLB+DSTT
            ELSE
              XLBL=XCLB-DSTL*COS(ANLB)+DSTB*SIN(ANLB)
              XRBL=XCLB+DSTR*COS(ANLB)+DSTB*SIN(ANLB)
              XRTL=XCLB+DSTR*COS(ANLB)-DSTT*SIN(ANLB)
              XLTL=XCLB-DSTL*COS(ANLB)-DSTT*SIN(ANLB)
              YLBL=YCLB-DSTL*SIN(ANLB)-DSTB*COS(ANLB)
              YRBL=YCLB+DSTR*SIN(ANLB)-DSTB*COS(ANLB)
              YRTL=YCLB+DSTR*SIN(ANLB)+DSTT*COS(ANLB)
              YLTL=YCLB-DSTL*SIN(ANLB)+DSTT*COS(ANLB)
              XLLB=MIN(XLBL,XRBL,XRTL,XLTL)
              XRLB=MAX(XLBL,XRBL,XRTL,XLTL)
              YBLB=MIN(YLBL,YRBL,YRTL,YLTL)
              YTLB=MAX(YLBL,YRBL,YRTL,YLTL)
            END IF
C
            IF (IOHL/4.EQ.1) THEN
              IF (XLLB.LT.XVPL.OR.XRLB.GT.XVPR.OR.
     +            YBLB.LT.YVPB.OR.YTLB.GT.YVPT) GO TO 110
            ELSE IF (IOHL/4.GE.2) THEN
              DELX=0.
              IF (XLLB.LT.XVPL) DELX=XVPL-XLLB
              IF (XRLB+DELX.GT.XVPR) THEN
                IF (DELX.NE.0.) GO TO 110
                DELX=XVPR-XRLB
              END IF
              DELY=0.
              IF (YBLB.LT.YVPB) DELY=YVPB-YBLB
              IF (YTLB+DELY.GT.YVPT) THEN
                IF (DELY.NE.0.) GO TO 110
                DELY=YVPT-YTLB
              END IF
              XCLB=XCLB+DELX
              XLLB=XLLB+DELX
              XRLB=XRLB+DELX
              YCLB=YCLB+DELY
              YBLB=YBLB+DELY
              YTLB=YTLB+DELY
              XLBC=CFUX(XCLB)
              IF (ICFELL('CTHLLB',14).NE.0) RETURN
              YLBC=CFUY(YCLB)
              IF (ICFELL('CTHLLB',15).NE.0) RETURN
            END IF
C
          END IF
C
          IF (MOD(IOHL,4).NE.0) THEN
C
            ILB1=1
            ILB2=NLBS
            IF (MOD(IOHL,2).EQ.0) ILB1=INHL
            IF (MOD(IOHL/2,2).EQ.0) ILB2=INHL-1
C
              ILBL = ILB1
              GO TO 10066
10064         CONTINUE
              ILBL =ILBL +1
10066         CONTINUE
              IF (ILBL .GT.(ILB2)) GO TO 10065
C
              IF (ILBL.EQ.INIL) ETRA=.5*CHWM*WCIL*(XVPR-XVPL)
              IF (ILBL.EQ.INHL) ETRA=.5*CHWM*WCHL*(XVPR-XVPL)
              XCOL=RWRK(IR03+4*(ILBL-1)+1)
              YCOL=RWRK(IR03+4*(ILBL-1)+2)
              ANOL=RWRK(IR03+4*(ILBL-1)+3)
              SAOL=SIN(ANOL)
              CAOL=COS(ANOL)
              ICOL=INT(RWRK(IR03+4*(ILBL-1)+4))
              ODSL=RWRK(IR04-ICOL+3)+ETRA
              ODSR=RWRK(IR04-ICOL+4)+ETRA
              ODSB=RWRK(IR04-ICOL+5)+ETRA
              ODST=RWRK(IR04-ICOL+6)+ETRA
C
              IF (ANOL.EQ.0.) THEN
                XLOL=XCOL-ODSL
                XROL=XCOL+ODSR
                YBOL=YCOL-ODSB
                YTOL=YCOL+ODST
              ELSE
                XLBO=XCOL-ODSL*CAOL+ODSB*SAOL
                XRBO=XCOL+ODSR*CAOL+ODSB*SAOL
                XRTO=XCOL+ODSR*CAOL-ODST*SAOL
                XLTO=XCOL-ODSL*CAOL-ODST*SAOL
                YLBO=YCOL-ODSL*SAOL-ODSB*CAOL
                YRBO=YCOL+ODSR*SAOL-ODSB*CAOL
                YRTO=YCOL+ODSR*SAOL+ODST*CAOL
                YLTO=YCOL-ODSL*SAOL+ODST*CAOL
                XLOL=MIN(XLBO,XRBO,XRTO,XLTO)
                XROL=MAX(XLBO,XRBO,XRTO,XLTO)
                YBOL=MIN(YLBO,YRBO,YRTO,YLTO)
                YTOL=MAX(YLBO,YRBO,YRTO,YLTO)
              END IF
C
              IF (XRLB.GE.XLOL.AND.XLLB.LE.XROL.AND.
     +            YTLB.GE.YBOL.AND.YBLB.LE.YTOL) GO TO 110
C
            GO TO 10064
10065       CONTINUE
C
          END IF
C
          NLBS=NLBS+1
          IF (4*NLBS.GT.LR03) THEN
            CALL CTGRWS (RWRK,3,MAX(4*NLBS,LR03+100),IWSE)
            IF (IWSE.NE.0) THEN
              NLBS=NLBS-1
              GO TO 109
            ELSE IF (ICFELL('CTHLLB',16).NE.0) THEN
              NLBS=NLBS-1
              RETURN
            END IF
          END IF
          RWRK(IR03+4*(NLBS-1)+1)=XCLB
          RWRK(IR03+4*(NLBS-1)+2)=YCLB
          RWRK(IR03+4*(NLBS-1)+3)=ANLB
          RWRK(IR03+4*(NLBS-1)+4)=-NR04
          NR04=NR04+6
          IF (NR04.GT.LR04) THEN
            CALL CTGRWS (RWRK,4,MAX(NR04,LR04+100),IWSE)
            IF (IWSE.NE.0) THEN
              NLBS=NLBS-1
              GO TO 109
            ELSE IF (ICFELL('CTHLLB',17).NE.0) THEN
              NLBS=NLBS-1
              RETURN
            END IF
          END IF
          RWRK(IR04+NR04-5)=REAL(IHOL+1)
          RWRK(IR04+NR04-4)=DVAL
          RWRK(IR04+NR04-3)=DTOL
          RWRK(IR04+NR04-2)=DTOR
          RWRK(IR04+NR04-1)=DTOB
          RWRK(IR04+NR04  )=DTOT
C
        END IF
C
  110 CONTINUE
      GO TO (10031,10063) , L10032
C
      END
