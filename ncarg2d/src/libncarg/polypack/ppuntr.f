C
C $Id: ppuntr.f,v 1.1 1994-06-20 19:49:06 kennison Exp $
C
      SUBROUTINE PPUNTR (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                        RWRK,IWRK,NWRK,URPT,IERR)
C
      DIMENSION XCCP(NCCP),YCCP(NCCP)
      DIMENSION XCSP(NCSP),YCSP(NCSP)
      DIMENSION RWRK(NWRK),IWRK(NWRK)
C
C The subroutine PPUNTR, given X/Y coordinates defining the vertices
C of a "clip polygon" in (XCCP(I),I=1,NCCP) and (YCCP(I),I=1,NCCP),
C X/Y coordinates defining the vertices of a "subject polygon" in
C (XCSP(I),I=1,NCSP) and (YCSP(I),I=1,NCSP), and the real and integer
C workspaces RWRK and IWRK, each of which is of length NWRK, generates
C a set of trapezoids representing the union of the two polygons and
C delivers each of them to a user-defined trapezoid-processing routine
C called URPT.  Errors, in general, result in an immediate RETURN with
C IERR non-zero; on a normal return, IERR is zero.
C
C For most efficient use of memory, IWRK and RWRK should be EQUIVALENCEd
C to each other.
C
C The algorithm used is that described by Bala R. Vatti in the article
C "A Generic Solution to Polygon Clipping", which was published in the
C July, 1992, issue of "Communications of the ACM" (Vol. 35, No. 7).
C
C The various linked lists used in Vatti's algorithm are implemented as
C follows:
C
C LMT (Local Minimum Table).  Formed initially at the lower end of the
C workspace.  Released 3-word nodes are put on a garbage list and may
C be re-used as part of a trapezoid node.  LMT nodes have the following
C structure:
C
C   0: Y value of a local minimum on one of the two input polygons.
C      LMT nodes are sorted by increasing value of this element.
C
C   1: Index of local minimum (1 to LCCP for clip polygon, LCCP+1 to
C      LCCP+LCSP for subject polygon).
C
C   2: Index of the next node of the LMT.
C
C AET (Active Edge Table).  Occupies space at the lower end of the
C workspace.  Released 10-word nodes are put on a garbage list and may
C be re-used for new AET nodes.  AET nodes have the following structure:
C
C   0: X coordinate at the current scanbeam position.  AET nodes are
C      sorted by increasing value of this element.
C
C   1: X coordinate at the end of the edge segment.  (I added this to
C      get around a problem which arose because Vatti's formulation did
C      not result in correct X coordinates at the end of a segment.)
C
C   2: Y coordinate at the end of the edge segment.
C
C   3: Change in X for a unit increase in Y.
C
C   4: Clip/subject edge flag (0 for clip, 1 for subject).
C
C   5: Left/right flag (0 for left, 1 for right).
C
C   6: Pointer to the next edge in the AET.
C
C   7: Pointer to the previous edge in the AET.
C
C   8: Pointer to the edge segment which succeeds this one.  This value
C      is either positive or negative and has absolute value "n".  If
C      the value is positive, it implies that the indices of the points
C      at the ends of the succeeding edge are "n" and "n+1"; if the
C      value is negative, the indices are "n" and "n-1".  The indices
C      are into the arrays XCCP and YCCP, if element 4 is zero, or XCSP
C      and YCSP, if element 4 is non-zero.
C
C   9: Pointer to trapezoid node to which the edge is "contributing"
C      (0 if no such trapezoid).
C
C Trapezoid Nodes.  Occupy space at the upper end of the workspace.
C Released 3-word nodes are put on a garbage list from which they can
C be re-used for other trapezoids.  Trapezoid nodes have the following
C structure:
C
C   0: X coordinate at the left end of the bottom of the trapezoid.
C
C   1: X coordinate at the right end of the bottom of the trapezoid.
C
C   2: Y coordinate of the bottom of the trapezoid.
C
C SET (Sorted Edge Table).  Occupies space at the lower end of the
C workspace, following the AET.  All space used is reclaimed.  SET
C nodes have the following structure:
C
C   0: X coordinate of edge's intersection with the top of the scanbeam.
C      SET nodes are sorted by decreasing value of this element.
C
C   1: Pointer to a node in the AET.  Says which edge is represented by
C      the node.
C
C   2: Pointer to the next node in the SET.
C
C INT (INtersection Table).  Occupies space at the lower end of the
C workspace, following the AET.  All space used is reclaimed.  INT
C nodes have the following structure:
C
C   0: X coordinate of point of intersection.
C
C   1: Y coordinate of point of intersection.  INT nodes are sorted
C      by increasing value of this element.
C
C   2: Pointer to a node in the AET, identifying one of the two edges
C      that intersect.
C
C   3: Pointer to a later node in the AET, identifying the other edge.
C
C   4: Pointer to the next node in the INT.
C
C Define RBIG to be a large real number.
C
      DATA RBIG / 1.E36 /
C
C Zero error flag.
C
      IERR=0
C
C Decide what the real lengths of the polygons are (depending on whether
C the first point is repeated at the end or not).
C
      LCCP=NCCP
      IF (XCCP(NCCP).EQ.XCCP(1).AND.YCCP(NCCP).EQ.YCCP(1)) LCCP=NCCP-1
C
      LCSP=NCSP
      IF (XCSP(NCSP).EQ.XCSP(1).AND.YCSP(NCSP).EQ.YCSP(1)) LCSP=NCSP-1
C
C Do some simple checks for degenerate cases.
C
      IF (.NOT.(LCCP.LT.3)) GO TO 10001
        GO TO 10003
10001 CONTINUE
C
      IF (.NOT.(LCSP.LT.3)) GO TO 10004
        GO TO 10006
10004 CONTINUE
C
C Initialize the garbage lists, onto which released 3-word and 10-word
C nodes are put for possible re-use.
C
      IG03=0
      IG10=0
C
C Initialize pointers to the last-used elements at the beginning and
C end of the available workspace.  Initially, the whole thing is
C available:
C
      IPWL=0
      IPWU=NWRK+1
C
C Build the "LMT" ("Local Minimum Table").  Initially, it is empty:
C
      ILMT=0
C
C Search for local minima of the clip polygon.  First, find a starting
C place where the Y coordinate changes one way or the other.
C
      INXT=0
C
      DO 10007 I=1,LCCP-1
        IF (.NOT.(YCCP(I).NE.YCCP(I+1))) GO TO 10008
          INXT=I
          YNXT=YCCP(INXT)
          GO TO 101
10008   CONTINUE
10007 CONTINUE
C
C If there is no such starting place, take an error exit.
C
      GO TO 10003
C
C Otherwise, go through the entire polygon from the starting position,
C finding all those places where the Y value increases after having
C decreased.  Each such place constitutes one of the local minima in
C the LMT.
C
  101 IDIR=0
C
      DO 10010 I=0,LCCP
        ILST=INXT
        YLST=YNXT
        INXT=INXT+1
        IF (INXT.GT.LCCP) INXT=INXT-LCCP
        YNXT=YCCP(INXT)
        IF (.NOT.(YNXT.LT.YLST)) GO TO 10011
          IDIR=-1
        GO TO 10012
10011   CONTINUE
        IF (.NOT.(YNXT.GT.YLST)) GO TO 10013
          IF (.NOT.(IDIR.LT.0)) GO TO 10014
            ILMN=IPWL+1
            IPWL=IPWL+3
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10015
              GO TO 10017
10015       CONTINUE
            RWRK(ILMN)=YLST
            IWRK(ILMN+1)=ILST
            ITM1=0
            ITM2=ILMT
10018       CONTINUE
              IF (ITM2.EQ.0) GO TO 10019
              IF (RWRK(ILMN).LE.RWRK(ITM2)) GO TO 10019
              ITM1=ITM2
              ITM2=IWRK(ITM2+2)
            GO TO 10018
10019       CONTINUE
            IF (.NOT.(ITM1.EQ.0)) GO TO 10020
              ILMT=ILMN
            GO TO 10021
10020       CONTINUE
              IWRK(ITM1+2)=ILMN
10021       CONTINUE
            IWRK(ILMN+2)=ITM2
10014     CONTINUE
          IDIR=+1
10012   CONTINUE
10013   CONTINUE
10010 CONTINUE
C
C In the same way, search for local minima of the subject polygon.
C
      INXT=0
C
      DO 10022 I=1,LCSP-1
        IF (.NOT.(YCSP(I).NE.YCSP(I+1))) GO TO 10023
          INXT=I
          YNXT=YCSP(INXT)
          GO TO 102
10023   CONTINUE
10022 CONTINUE
C
      GO TO 10006
C
  102 IDIR=0
C
      DO 10025 I=0,LCSP
        ILST=INXT
        YLST=YNXT
        INXT=INXT+1
        IF (INXT.GT.LCSP) INXT=INXT-LCSP
        YNXT=YCSP(INXT)
        IF (.NOT.(YNXT.LT.YLST)) GO TO 10026
          IDIR=-1
        GO TO 10027
10026   CONTINUE
        IF (.NOT.(YNXT.GT.YLST)) GO TO 10028
          IF (.NOT.(IDIR.LT.0)) GO TO 10029
            ILMN=IPWL+1
            IPWL=IPWL+3
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10030
              GO TO 10017
10030       CONTINUE
            RWRK(ILMN)=YLST
            IWRK(ILMN+1)=LCCP+ILST
            ITM1=0
            ITM2=ILMT
10032       CONTINUE
              IF (ITM2.EQ.0) GO TO 10033
              IF (RWRK(ILMN).LE.RWRK(ITM2)) GO TO 10033
              ITM1=ITM2
              ITM2=IWRK(ITM2+2)
            GO TO 10032
10033       CONTINUE
            IF (.NOT.(ITM1.EQ.0)) GO TO 10034
              ILMT=ILMN
            GO TO 10035
10034       CONTINUE
              IWRK(ITM1+2)=ILMN
10035       CONTINUE
            IWRK(ILMN+2)=ITM2
10029     CONTINUE
          IDIR=+1
10027   CONTINUE
10028   CONTINUE
10025 CONTINUE
C
C Initialize the "AET" ("Active Edge Table") to be empty:
C
      IAET=0
C
C Initialize the variable that normally keeps track of the Y coordinate
C at the top of the current "scanbeam"; the value will be used as the Y
C coordinate at the bottom of the first one.
C
      YTOS=RWRK(ILMT)
C
C Loop through the "scanbeams".
C
10036 CONTINUE
C
C YBOS is the Y coordinate of the bottom of the new scanbeam.
C
        YBOS=YTOS
C
C Loop through those local minima in the LMT having Y coordinate
C YBOS; for each, add to the AET the pair of edges that start at
C that local minimum.
C
10037   CONTINUE
C
C Quit if the end of the LMT has been reached.
C
          IF (ILMT.EQ.0) GO TO 10038
C
C Quit if the Y coordinate of the next local minimum is too large.
C
          IF (RWRK(ILMT).GT.YBOS) GO TO 10038
C
C Retrieve in IMIN the index of the coordinates of the local minimum.
C
          IMIN=IWRK(ILMT+1)
C
C Set ICOS to indicate whether the local minimum comes from the clip
C polygon or the subject polygon.  XMIN and YMIN are the X and Y
C coordinates of the local minimum.  ILST indexes the coordinates of
C the last point along the polygon; the coordinates are XLST and YLST.
C Similarly, INXT indexes the coordinates of the next point along
C the polygon; the coordinates are XNXT and YNXT.
C
          IF (.NOT.(IMIN.LE.LCCP)) GO TO 10039
            ICOS=0
            XMIN=XCCP(IMIN)
            YMIN=YCCP(IMIN)
            ILST=IMIN-1
            IF (ILST.LT.1) ILST=ILST+LCCP
            XLST=XCCP(ILST)
            YLST=YCCP(ILST)
            INXT=IMIN+1
            IF (INXT.GT.LCCP) INXT=INXT-LCCP
            XNXT=XCCP(INXT)
            YNXT=YCCP(INXT)
          GO TO 10040
10039     CONTINUE
            ICOS=1
            IMIN=IMIN-LCCP
            XMIN=XCSP(IMIN)
            YMIN=YCSP(IMIN)
            ILST=IMIN-1
            IF (ILST.LT.1) ILST=ILST+LCSP
            XLST=XCSP(ILST)
            YLST=YCSP(ILST)
            INXT=IMIN+1
            IF (INXT.GT.LCSP) INXT=INXT-LCSP
            XNXT=XCSP(INXT)
            YNXT=YCSP(INXT)
10040     CONTINUE
C
C Now we must scan the AET to determine where to put the new edges.
C After executing the loop below, ITM1 will point to the node after
C which they will be inserted (zero if at beginning) and ITM2 will
C point to the node before which they will be inserted (zero if at
C end).  The variable IOCP will be updated to indicate whether the
C local minimum is inside (1) or outside (0) the clip polygon.
C Similarly, IOSP will be updated to indicate whether the local
C minimum is inside (1) or outside (0) the subject polygon.
C
          ITM1=0
          ITM2=IAET
C
          IOCP=0
          IOSP=0
C
10041     CONTINUE
C
C Exit if the end of the AET has been reached.
C
            IF (ITM2.EQ.0) GO TO 10042
C
C Exit if the new local minimum fits between elements ITM1 and ITM2 of
C the AET.
C
            IF (XMIN.LE.RWRK(ITM2)) GO TO 10042
C
C Advance to the next position in the AET.
C
            ITM1=ITM2
            ITM2=IWRK(ITM2+6)
C
C Update the flags that say where we are relative to the clip and
C subject polygons.
C
            IF (.NOT.(IWRK(ITM1+4).EQ.0)) GO TO 10043
              IOCP=1-IOCP
            GO TO 10044
10043       CONTINUE
              IOSP=1-IOSP
10044       CONTINUE
C
C End of loop through the AET.
C
          GO TO 10041
10042     CONTINUE
C
C Create two new nodes in the AET.  Either re-use 10-word nodes from the
C garbage list or create new ones.
C
          IF (.NOT.(IG10.NE.0)) GO TO 10045
            IPNL=IG10
            IG10=IWRK(IG10)
          GO TO 10046
10045     CONTINUE
            IPNL=IPWL+1
            IPWL=IPWL+10
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10047
              GO TO 10017
10047       CONTINUE
10046     CONTINUE
C
          IF (.NOT.(IG10.NE.0)) GO TO 10049
            IPNN=IG10
            IG10=IWRK(IG10)
          GO TO 10050
10049     CONTINUE
            IPNN=IPWL+1
            IPWL=IPWL+10
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10051
              GO TO 10017
10051       CONTINUE
10050     CONTINUE
C
C Fill in the information about the two new edges:
C
          RWRK(IPNL)=XMIN
          RWRK(IPNN)=XMIN
C
          RWRK(IPNL+1)=XLST
          RWRK(IPNN+1)=XNXT
C
          RWRK(IPNL+2)=YLST
          RWRK(IPNN+2)=YNXT
C
          IF (.NOT.(YLST.NE.YMIN)) GO TO 10053
            RWRK(IPNL+3)=(XLST-XMIN)/(YLST-YMIN)
          GO TO 10054
10053     CONTINUE
            RWRK(IPNL+3)=SIGN(RBIG,XLST-XMIN)
10054     CONTINUE
C
          IF (.NOT.(YNXT.NE.YMIN)) GO TO 10055
            RWRK(IPNN+3)=(XNXT-XMIN)/(YNXT-YMIN)
          GO TO 10056
10055     CONTINUE
            RWRK(IPNN+3)=SIGN(RBIG,XNXT-XMIN)
10056     CONTINUE
C
          IWRK(IPNL+4)=ICOS
          IWRK(IPNN+4)=ICOS
C
          IF (.NOT.(ICOS.EQ.0)) GO TO 10057
            IOPO=IOCP
          GO TO 10058
10057     CONTINUE
            IOPO=IOSP
10058     CONTINUE
C
          IF (.NOT.(RWRK(IPNL+3).LT.RWRK(IPNN+3))) GO TO 10059
C
            IPE1=IPNL
            IPE2=IPNN
C
          GO TO 10060
10059     CONTINUE
C
            IPE1=IPNN
            IPE2=IPNL
C
10060     CONTINUE
C
          IWRK(IPE1+5)=IOPO
          IWRK(IPE2+5)=1-IOPO
C
          IF (.NOT.(ITM1.EQ.0)) GO TO 10061
            IAET=IPE1
          GO TO 10062
10061     CONTINUE
            IWRK(ITM1+6)=IPE1
10062     CONTINUE
C
          IWRK(IPE1+6)=IPE2
          IWRK(IPE2+6)=ITM2
          IF (ITM2.NE.0) IWRK(ITM2+7)=IPE2
          IWRK(IPE2+7)=IPE1
          IWRK(IPE1+7)=ITM1
C
          IWRK(IPNL+8)=-ILST
          IWRK(IPNN+8)=+INXT
C
C If the edges are "contributing", create trapezoid nodes for them
C to "contribute" to and initialize them; otherwise, zero the output
C trapezoid pointers.
C
          IF (.NOT.((IOCP.EQ.0.AND.IOSP.EQ.0).OR.(IOCP.NE.0.AND.IOSP.EQ.
     +0.AND.ICOS.EQ.0).OR.(IOCP.EQ.0.AND.IOSP.NE.0.AND.ICOS.NE.0)))
     +    GO TO 10063
C
            IF (.NOT.(IOCP.EQ.0.AND.IOSP.EQ.0)) GO TO 10064
C
              IF (.NOT.(IG03.NE.0)) GO TO 10065
                IPTN=IG03
                IG03=IWRK(IG03)
              GO TO 10066
10065         CONTINUE
                IPWU=IPWU-3
                IF (.NOT.(IPWU.LE.IPWL)) GO TO 10067
                  GO TO 10017
10067           CONTINUE
                IPTN=IPWU
10066         CONTINUE
C
              RWRK(IPTN  )=XMIN
              RWRK(IPTN+1)=XMIN
              RWRK(IPTN+2)=YMIN
C
              IWRK(IPE1+9)=IPTN
              IWRK(IPE2+9)=IPTN
C
            GO TO 10069
10064       CONTINUE
C
              IPET=IWRK(IPE1+7)
              IPEL=0
C
10070         CONTINUE
                IF (IPET.EQ.0) GO TO 10071
                IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10072
                  IPEL=IPET
                  GO TO 10071
10072           CONTINUE
                IPET=IWRK(IPET+7)
              GO TO 10070
10071         CONTINUE
C
              IPET=IWRK(IPE2+6)
              IPER=0
C
10073         CONTINUE
                IF (IPET.EQ.0) GO TO 10074
                IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10075
                  IPER=IPET
                  GO TO 10074
10075           CONTINUE
                IPET=IWRK(IPET+6)
              GO TO 10073
10074         CONTINUE
C
              IF (.NOT.(IPEL.EQ.0.OR.IPER.EQ.0)) GO TO 10076
                IERR=1
                GO TO 10078
10076         CONTINUE
C
              IF (.NOT.(IWRK(IPEL+9).NE.IWRK(IPER+9))) GO TO 10079
                IERR=2
                GO TO 10078
10079         CONTINUE
C
              IPTN=IWRK(IPEL+9)
C
              CALL URPT (RWRK(IPTN),RWRK(IPTN+1),RWRK(IPTN+2),
     +                   RWRK(IPEL+3),RWRK(IPER+3),YBOS      )
C
              RWRK(IPTN  )=RWRK(IPEL)
              RWRK(IPTN+1)=XMIN
              RWRK(IPTN+2)=YBOS
C
              IWRK(IPE1+9)=IPTN
C
              IF (.NOT.(IG03.NE.0)) GO TO 10081
                IPTN=IG03
                IG03=IWRK(IG03)
              GO TO 10082
10081         CONTINUE
                IPWU=IPWU-3
                IF (.NOT.(IPWU.LE.IPWL)) GO TO 10083
                  GO TO 10017
10083           CONTINUE
                IPTN=IPWU
10082         CONTINUE
C
              RWRK(IPTN  )=XMIN
              RWRK(IPTN+1)=RWRK(IPER)
              RWRK(IPTN+2)=YBOS
C
              IWRK(IPE2+9)=IPTN
              IWRK(IPER+9)=IPTN
C
10069       CONTINUE
C
          GO TO 10085
10063     CONTINUE
C
            IWRK(IPE1+9)=0
            IWRK(IPE2+9)=0
C
10085     CONTINUE
C
C Put the current LMT node on the appropriate garbage list for re-use.
C
          IWRK(ILMT)=IG03
          IG03=ILMT
C
C Advance to the next element of the LMT.
C
          ILMT=IWRK(ILMT+2)
C
C End of the loop through the LMT.
C
        GO TO 10037
10038   CONTINUE
C
C At this point, if the AET is empty, the scanbeam loop is exited.
C
  103 CONTINUE
        IF (IAET.EQ.0) GO TO 10086
C
C Scan the AET to compute the value of the Y coordinate at the top of
C the scanbeam (YTOS) and to look for horizontal edges in the list.
C
        ITMP=IAET
C
        YTOS=RWRK(ITMP+2)
C
        IF (ILMT.NE.0) YTOS=MIN(YTOS,RWRK(ILMT))
C
10087   CONTINUE
C
C Check for a horizontal section.
C
          IF (.NOT.(YTOS.EQ.YBOS)) GO TO 10088
C
C Step through points in the user's arrays until the end of the
C horizontal section is reached, updating the X coordinate and the
C index of the successor edge as we go.
C
            INNP=ABS(IWRK(ITMP+8))
C
10089       CONTINUE
C
              IF (.NOT.(IWRK(ITMP+4).EQ.0)) GO TO 10090
                IF (.NOT.(INNP.LT.1)) GO TO 10091
                  INNP=INNP+LCCP
                GO TO 10092
10091           CONTINUE
                IF (.NOT.(INNP.GT.LCCP)) GO TO 10093
                  INNP=INNP-LCCP
10092           CONTINUE
10093           CONTINUE
                IF (YCCP(INNP).NE.YBOS) GO TO 10094
                RWRK(ITMP)=XCCP(INNP)
              GO TO 10095
10090         CONTINUE
                IF (.NOT.(INNP.LT.1)) GO TO 10096
                  INNP=INNP+LCSP
                GO TO 10097
10096           CONTINUE
                IF (.NOT.(INNP.GT.LCSP)) GO TO 10098
                  INNP=INNP-LCSP
10097           CONTINUE
10098           CONTINUE
                IF (YCSP(INNP).NE.YBOS) GO TO 10094
                RWRK(ITMP)=XCSP(INNP)
10095         CONTINUE
C
              RWRK(ITMP+1)=RWRK(ITMP)
C
              IWRK(ITMP+8)=SIGN(INNP,IWRK(ITMP+8))
              INNP=INNP+SIGN(1,IWRK(ITMP+8))
C
            GO TO 10089
10094       CONTINUE
C
C Compute a quantity that will be used to recognize the successor of
C the horizontal edge.
C
            INNL=ABS(IWRK(ITMP+8))-SIGN(1,IWRK(ITMP+8))
            IF (.NOT.(INNL.LE.0)) GO TO 10099
              IF (.NOT.(IWRK(ITMP+4).EQ.0)) GO TO 10100
                INNL=INNL+LCCP
              GO TO 10101
10100         CONTINUE
                INNL=INNL+LCSP
10101         CONTINUE
10099       CONTINUE
            INNL=-SIGN(INNL,IWRK(ITMP+8))
C
C Zero the pointer to the list of intersection points.
C
            IINT=0
C
C Save the current value of the pointer to the last word currently used
C in the lower end of the workspace, so that the space occupied by the
C list of intersection points can easily be reclaimed.
C
            ISWL=IPWL
C
C Initialize pointers used below.  The horizontal edge is considered
C to intersect edges that it actually passes over.  If there are edges
C in the AET with X coordinates equal to the X coordinate of the end of
C the horizontal edge, it only intersects them if that is necessary in
C order to make it and its successor be next to each other in the AET.
C
            IINN=-1
            IINQ=0
C
C Generate the list of intersection points, either to the left ...
C
            IF (.NOT.(IWRK(ITMP+7).NE.0)) GO TO 10102
C
              IDUM=IWRK(ITMP+7)
C
10103         CONTINUE
C
                IF (RWRK(IDUM).LT.RWRK(ITMP)) GO TO 10104
C
                IF (.NOT.(IWRK(IDUM+4).EQ.IWRK(ITMP+4).AND.IWRK(IDUM+8).
     +EQ.INNL)) GO TO 10105
                  IINQ=IINN
                  GO TO 10104
10105           CONTINUE
C
                IF (.NOT.(IINT.EQ.0)) GO TO 10106
                  IINT=IPWL+1
                GO TO 10107
10106           CONTINUE
                  IWRK(IINN+4)=IPWL+1
10107           CONTINUE
C
                IINN=IPWL+1
                IPWL=IPWL+5
C
                IF (.NOT.(IPWL.GE.IPWU)) GO TO 10108
                  GO TO 10017
10108           CONTINUE
C
                RWRK(IINN)=RWRK(IDUM)
                RWRK(IINN+1)=YBOS
                IWRK(IINN+2)=IDUM
                IWRK(IINN+3)=ITMP
                IWRK(IINN+4)=0
C
                IF (RWRK(IDUM).GT.RWRK(ITMP)) IINQ=IINN
C
                IDUM=IWRK(IDUM+7)
C
                IF (IDUM.EQ.0) GO TO 10104
C
              GO TO 10103
10104         CONTINUE
C
10102       CONTINUE
C
C ... or to the right.
C
            IF (.NOT.(IINQ.EQ.0)) GO TO 10110
C
              IINT=0
              IPWL=ISWL
              IINN=-1
C
              IF (.NOT.(IWRK(ITMP+6).NE.0)) GO TO 10111
C
                IDUM=IWRK(ITMP+6)
C
10112           CONTINUE
C
                  IF (RWRK(IDUM).GT.RWRK(ITMP)) GO TO 10113
C
                  IF (.NOT.(IWRK(IDUM+4).EQ.IWRK(ITMP+4).AND.IWRK(IDUM+8
     +).EQ.INNL)) GO TO 10114
                    IINQ=IINN
                    GO TO 10113
10114             CONTINUE
C
                  IF (.NOT.(IINT.EQ.0)) GO TO 10115
                    IINT=IPWL+1
                  GO TO 10116
10115             CONTINUE
                    IWRK(IINN+4)=IPWL+1
10116             CONTINUE
C
                  IINN=IPWL+1
                  IPWL=IPWL+5
C
                  IF (.NOT.(IPWL.GE.IPWU)) GO TO 10117
                    GO TO 10017
10117             CONTINUE
C
                  RWRK(IINN)=RWRK(IDUM)
                  RWRK(IINN+1)=YBOS
                  IWRK(IINN+2)=ITMP
                  IWRK(IINN+3)=IDUM
                  IWRK(IINN+4)=0
C
                  IF (RWRK(IDUM).LT.RWRK(ITMP)) IINQ=IINN
C
                  IDUM=IWRK(IDUM+6)
C
                  IF (IDUM.EQ.0) GO TO 10113
C
                GO TO 10112
10113           CONTINUE
C
10111         CONTINUE
C
10110       CONTINUE
C
C Clear entries at the end of the intersection list that don't need to
C be considered to be intersections.  (This may clear the whole list.)
C
            IF (.NOT.(IINQ.EQ.0)) GO TO 10119
              IINT=0
              IPWL=ISWL
            GO TO 10120
10119       CONTINUE
            IF (.NOT.(IINQ.GT.0)) GO TO 10121
              IWRK(IINQ+4)=0
10120       CONTINUE
10121       CONTINUE
C
C If any intersection points were found, process them and then reclaim
C the space used for the list.
C
            IF (.NOT.(IINT.NE.0)) GO TO 10122
              L10124=    1
              GO TO 10124
10123         CONTINUE
              IPWL=ISWL
10122       CONTINUE
C
C The horizontal edge is terminating at this point, so handle that.
C
            L10126=    1
            GO TO 10126
10125       CONTINUE
C
C Go back to see if the AET is empty now and, if not, to rescan it for
C more horizontal segments.
C
            GO TO 103
C
10088     CONTINUE
C
C Move to the next node in the AET.
C
          ITMP=IWRK(ITMP+6)
C
C Quit if there are none.
C
          IF (ITMP.EQ.0) GO TO 10127
C
C Update the variable that says where the top of the scanbeam is.
C
          YTOS=MIN(YTOS,RWRK(ITMP+2))
C
        GO TO 10087
10127   CONTINUE
C
C Create a table of all intersections of edges in the AET, sorted in
C order of increasing Y coordinate.  To do this, we also create a table
C of the current edges in the AET, sorted in the opposite order in which
C they intersect the top of the scanbeam.  Initially, the intersection
C table is empty:
C
        IINT=0
C
C The intersection table and the sorted edge table are formed in the
C lower part of the workspace array.  The value of the pointer to the
C last word currently used in that part of the workspace is saved so
C that, when we are done using the INT and the SET, the space used for
C them can be reclaimed by just restoring the value of this pointer:
C
        ISWL=IPWL
C
C Initialize the "Sorted Edge Table" to contain just the first edge
C from the AET.
C
        ISET=IPWL+1
C
        IPWL=IPWL+3
C
        IF (.NOT.(IPWL.GE.IPWU)) GO TO 10128
          GO TO 10017
10128   CONTINUE
C
        RWRK(ISET)=RWRK(IAET+1)+(YTOS-RWRK(IAET+2))*RWRK(IAET+3)
        IWRK(ISET+1)=IAET
        IWRK(ISET+2)=0
C
C Examine each of the remaining edges in the AET, one at a time,
C looking for intersections with edges that have already gone into
C the SET; for each one found, generate an entry in the INT.  Special
C care is taken to ensure that edges which are each other's successors
C end up adjacent to each other in the AET.
C
        ITMP=IWRK(IAET+6)
C
10130   CONTINUE
C
          IF (ITMP.EQ.0) GO TO 10131
C
          XTMP=RWRK(ITMP+1)+(YTOS-RWRK(ITMP+2))*RWRK(ITMP+3)
C
          IST1=0
          IST2=ISET
C
10132     CONTINUE
C
            IF (IST2.EQ.0) GO TO 10133
            IF (XTMP.GT.RWRK(IST2)) GO TO 10133
C
            IF (.NOT.(XTMP.EQ.RWRK(IST2))) GO TO 10134
C
              IST3=IWRK(IST2+2)
              IST4=0
C
10135         CONTINUE
C
                IF (IST3.EQ.0) GO TO 10136
                IF (XTMP.NE.RWRK(IST3)) GO TO 10136
C
                IF (.NOT.(IWRK(IWRK(IST3+1)+4).EQ.IWRK(ITMP+4).AND.IWRK(
     +IWRK(IST3+1)+8).EQ.-IWRK(ITMP+8))) GO TO 10137
                  IST4=1
                  GO TO 10136
10137           CONTINUE
C
                IST3=IWRK(IST3+2)
C
              GO TO 10135
10136         CONTINUE
C
              IF (IST4.EQ.0) GO TO 10133
C
              XINT=XTMP
              YINT=YTOS
C
            GO TO 10138
10134       CONTINUE
C
              IF (.NOT.(ABS(RWRK(ITMP+3)-RWRK(IWRK(IST2+1)+3)).GT.1.E-6)
     +)       GO TO 10139
                YINT=YBOS-(RWRK(ITMP  )-RWRK(IWRK(IST2+1)  ))/
     +                    (RWRK(ITMP+3)-RWRK(IWRK(IST2+1)+3))
              GO TO 10140
10139         CONTINUE
                YINT=.5*(YBOS+YTOS)
10140         CONTINUE
C
              IF (.NOT.(ABS(RWRK(ITMP+3)).LT.ABS(RWRK(IWRK(IST2+1)+3))))
     +        GO TO 10141
                XINT=RWRK(ITMP+1)+(YINT-RWRK(ITMP+2))*RWRK(ITMP+3)
              GO TO 10142
10141         CONTINUE
                XINT=RWRK(IWRK(IST2+1)+1)+(YINT-RWRK(IWRK(IST2+1)+2))*
     +               RWRK(IWRK(IST2+1)+3)
10142         CONTINUE
C
10138       CONTINUE
C
            IINN=IPWL+1
            IPWL=IPWL+5
C
            IF (.NOT.(IPWL.GE.IPWU)) GO TO 10143
              GO TO 10017
10143       CONTINUE
C
            RWRK(IINN)=XINT
            RWRK(IINN+1)=YINT
            IWRK(IINN+2)=IWRK(IST2+1)
            IWRK(IINN+3)=ITMP
C
            IIN1=0
            IIN2=IINT
C
10145       CONTINUE
              IF (IIN2.EQ.0) GO TO 10146
              IF (RWRK(IINN+1).LE.RWRK(IIN2+1)) GO TO 10146
              IIN1=IIN2
              IIN2=IWRK(IIN2+4)
            GO TO 10145
10146       CONTINUE
C
            IF (.NOT.(IIN1.EQ.0)) GO TO 10147
              IINT=IINN
            GO TO 10148
10147       CONTINUE
              IWRK(IIN1+4)=IINN
10148       CONTINUE
C
            IWRK(IINN+4)=IIN2
C
            IST1=IST2
            IST2=IWRK(IST2+2)
C
          GO TO 10132
10133     CONTINUE
C
          ISTN=IPWL+1
          IPWL=IPWL+3
C
          IF (.NOT.(IPWL.GE.IPWU)) GO TO 10149
            GO TO 10017
10149     CONTINUE
C
          IF (.NOT.(IST1.EQ.0)) GO TO 10151
            ISET=ISTN
          GO TO 10152
10151     CONTINUE
            IWRK(IST1+2)=ISTN
10152     CONTINUE
C
          RWRK(ISTN)=XTMP
          IWRK(ISTN+1)=ITMP
          IWRK(ISTN+2)=IST2
C
          ITMP=IWRK(ITMP+6)
C
        GO TO 10130
10131   CONTINUE
C
C If intersections have been found, process them.
C
        IF (.NOT.(IINT.NE.0)) GO TO 10153
          L10124=    2
          GO TO 10124
10154     CONTINUE
10153   CONTINUE
C
C Discard the intersection table and the sorted edge table.
C
        IPWL=ISWL
C
C Loop through all the edges in the AET, updating the X coordinates and
C further processing those that terminate at the top of the scanbeam.
C
        ITMP=IAET
C
10155   CONTINUE
C
C Exit if all the edges have been done.
C
          IF (ITMP.EQ.0) GO TO 10156
C
C Update the X coordinate to its position at the top of the scanbeam.
C
          RWRK(ITMP)=RWRK(ITMP+1)+(YTOS-RWRK(ITMP+2))*RWRK(ITMP+3)
C
C If the edge terminates at the top of this scanbeam, process it.
C
          IF (.NOT.(RWRK(ITMP+2).EQ.YTOS)) GO TO 10157
            L10126=    2
            GO TO 10126
10158       CONTINUE
10157     CONTINUE
C
C Advance to the next edge in the AET.
C
          ITMP=IWRK(ITMP+6)
C
C End of loop on edges in the AET.
C
        GO TO 10155
10156   CONTINUE
C
C End of scanbeam loop.
C
      GO TO 10036
10086 CONTINUE
C
C Normal exit.
C
      RETURN
C
C The following internal procedure processes the list of intersection
C points that IINT points to.  On entry, it may be assumed that IINT
C has been verified to be non-zero.
C
10124 CONTINUE
C
C Loop through all the points of intersection.
C
10159   CONTINUE
C
C Extract the coordinates of the point of intersection and the indices
C of the two AET nodes describing the edges that intersected.
C
  201     CONTINUE
C
          XINT=RWRK(IINT)
          YINT=RWRK(IINT+1)
C
          IPE1=IWRK(IINT+2)
          IPE2=IWRK(IINT+3)
C
C If the two edges are not adjacent in the AET, there's a problem.  We
C look for the next intersection of adjacent edges and move it to the
C beginning of the list.
C
          IF (.NOT.(IWRK(IPE1+6).NE.IPE2)) GO TO 10160
C
            IIN1=IINT
            IIN2=IWRK(IINT+4)
C
10161       CONTINUE
C
              IF (.NOT.(IIN2.EQ.0)) GO TO 10162
                IERR=3
                GO TO 10078
10162         CONTINUE
C
              IF (IWRK(IWRK(IIN2+2)+6).EQ.IWRK(IIN2+3)) GO TO 10164
C
              IIN1=IIN2
              IIN2=IWRK(IIN2+4)
C
            GO TO 10161
10164       CONTINUE
C
            IWRK(IIN1+4)=IWRK(IIN2+4)
            IWRK(IIN2+4)=IINT
            IINT=IIN2
C
            GO TO 201
C
10160     CONTINUE
C
C Check whether or not both edges are from the same input polygon.
C
          IF (.NOT.(IWRK(IPE1+4).EQ.IWRK(IPE2+4))) GO TO 10165
C
C Both edges are from the clip polygon or both are from the subject
C polygon.  If edge 1 is contributing to forming trapezoids, then edge
C 2 should be also, in which case we output one or more trapezoids.  In
C either case, we must swap the left/right flags in the two edges.
C
            IF (.NOT.(IWRK(IPE1+9).NE.0.OR.IWRK(IPE2+9).NE.0)) GO TO 101
     +66
C
              IF (.NOT.(IWRK(IPE1+9).EQ.0.OR.IWRK(IPE2+9).EQ.0)) GO TO 1
     +0167
                IERR=4
                GO TO 10078
10167         CONTINUE
C
              IF (.NOT.(IWRK(IPE1+9).EQ.IWRK(IPE2+9))) GO TO 10169
C
                IPTN=IWRK(IPE1+9)
C
                CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                     RWRK(IPTN+2),RWRK(IPE1+3),
     +                     RWRK(IPE2+3),YINT        )
C
                RWRK(IPTN  )=XINT
                RWRK(IPTN+1)=XINT
                RWRK(IPTN+2)=YINT
C
              GO TO 10170
10169         CONTINUE
C
                IPET=IWRK(IPE1+7)
                IPEL=0
C
10171           CONTINUE
                  IF (IPET.EQ.0) GO TO 10172
                  IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10173
                    IPEL=IPET
                    GO TO 10172
10173             CONTINUE
                  IPET=IWRK(IPET+7)
                GO TO 10171
10172           CONTINUE
C
                IF (.NOT.(IPEL.EQ.0)) GO TO 10174
                  IERR=5
                  GO TO 10078
10174           CONTINUE
C
                IF (.NOT.(IWRK(IPEL+9).NE.IWRK(IPE1+9))) GO TO 10176
                  IERR=6
                  GO TO 10078
10176           CONTINUE
C
                IPTN=IWRK(IPEL+9)
C
                CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                     RWRK(IPTN+2),RWRK(IPEL+3),
     +                     RWRK(IPE1+3),YINT        )
C
                RWRK(IPTN  )=RWRK(IPEL+1)+(YINT-RWRK(IPEL+2))*
     +                       RWRK(IPEL+3)
                RWRK(IPTN+1)=XINT
                RWRK(IPTN+2)=YINT
C
                IPET=IWRK(IPE2+6)
                IPER=0
C
10178           CONTINUE
                  IF (IPET.EQ.0) GO TO 10179
                  IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10180
                    IPER=IPET
                    GO TO 10179
10180             CONTINUE
                  IPET=IWRK(IPET+6)
                GO TO 10178
10179           CONTINUE
C
                IF (.NOT.(IPER.EQ.0)) GO TO 10181
                  IERR=7
                  GO TO 10078
10181           CONTINUE
C
                IF (.NOT.(IWRK(IPER+9).NE.IWRK(IPE2+9))) GO TO 10183
                  IERR=8
                  GO TO 10078
10183           CONTINUE
C
                IPTN=IWRK(IPER+9)
C
                CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                     RWRK(IPTN+2),RWRK(IPE2+3),
     +                     RWRK(IPER+3),YINT        )
C
                RWRK(IPTN  )=XINT
                RWRK(IPTN+1)=RWRK(IPER+1)+(YINT-RWRK(IPER+2))*
     +                       RWRK(IPER+3)
                RWRK(IPTN+2)=YINT
C
10170         CONTINUE
C
10166       CONTINUE
C
            IDUM=IWRK(IPE1+5)
            IWRK(IPE1+5)=IWRK(IPE2+5)
            IWRK(IPE2+5)=IDUM
C
C One edge is from the clip polygon and the other is from the
C subject polygon.  Check for a local minimum.
C
          GO TO 10185
10165     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.1.AND.IWRK(IPE1+5).EQ.0.AND.IWRK(IP
     +E2+4).EQ.0.AND.IWRK(IPE2+5).EQ.1).OR.(IWRK(IPE1+4).EQ.0.AND.IWRK(I
     +PE1+5).EQ.0.AND.IWRK(IPE2+4).EQ.1.AND.IWRK(IPE2+5).EQ.1))) GO TO 1
     +0186
C
C Process a local minimum.
C
            IF (.NOT.(IWRK(IPE1+9).NE.0.OR.IWRK(IPE2+9).NE.0)) GO TO 101
     +87
              IERR=9
              GO TO 10078
10187       CONTINUE
C
            IPET=IWRK(IPE1+7)
            IPEL=0
C
10189       CONTINUE
              IF (IPET.EQ.0) GO TO 10190
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10191
                IPEL=IPET
                GO TO 10190
10191         CONTINUE
              IPET=IWRK(IPET+7)
            GO TO 10189
10190       CONTINUE
C
            IPET=IWRK(IPE2+6)
            IPER=0
C
10192       CONTINUE
              IF (IPET.EQ.0) GO TO 10193
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10194
                IPER=IPET
                GO TO 10193
10194         CONTINUE
              IPET=IWRK(IPET+6)
            GO TO 10192
10193       CONTINUE
C
            IF (.NOT.(IPEL.EQ.0.OR.IPER.EQ.0)) GO TO 10195
              IERR=10
              GO TO 10078
10195       CONTINUE
C
            IF (.NOT.(IWRK(IPEL+9).NE.IWRK(IPER+9))) GO TO 10197
              IERR=11
              GO TO 10078
10197       CONTINUE
C
            IPTN=IWRK(IPEL+9)
C
            CALL URPT (RWRK(IPTN),RWRK(IPTN+1),RWRK(IPTN+2),
     +                 RWRK(IPEL+3),RWRK(IPER+3),YINT      )
C
            RWRK(IPTN  )=RWRK(IPEL+1)+(YINT-RWRK(IPEL+2))*RWRK(IPEL+3)
            RWRK(IPTN+1)=XINT
            RWRK(IPTN+2)=YINT
C
            IWRK(IPE1+9)=IPTN
C
            IF (.NOT.(IG03.NE.0)) GO TO 10199
              IPTN=IG03
              IG03=IWRK(IG03)
            GO TO 10200
10199       CONTINUE
              IPWU=IPWU-3
              IF (.NOT.(IPWU.LE.IPWL)) GO TO 10201
                GO TO 10017
10201         CONTINUE
              IPTN=IPWU
10200       CONTINUE
C
            RWRK(IPTN  )=XINT
            RWRK(IPTN+1)=RWRK(IPER+1)+(YINT-RWRK(IPER+2))*RWRK(IPER+3)
            RWRK(IPTN+2)=YINT
C
            IWRK(IPE2+9)=IPTN
            IWRK(IPER+9)=IPTN
C
C Check for a left intersection.
C
          GO TO 10185
10186     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.0.AND.IWRK(IPE1+5).EQ.0.AND.IWRK(IP
     +E2+4).EQ.1.AND.IWRK(IPE2+5).EQ.0).OR.(IWRK(IPE1+4).EQ.1.AND.IWRK(I
     +PE1+5).EQ.0.AND.IWRK(IPE2+4).EQ.0.AND.IWRK(IPE2+5).EQ.0))) GO TO 1
     +0203
C
C Process a left intersection.
C
            IF (.NOT.(IWRK(IPE1+9).EQ.0)) GO TO 10204
              IERR=12
              GO TO 10078
10204       CONTINUE
C
            IPET=IWRK(IPE1+6)
            IPER=0
C
10206       CONTINUE
              IF (IPET.EQ.0) GO TO 10207
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10208
                IPER=IPET
                GO TO 10207
10208         CONTINUE
              IPET=IWRK(IPET+6)
            GO TO 10206
10207       CONTINUE
C
            IF (.NOT.(IPER.EQ.0)) GO TO 10209
              IERR=13
              GO TO 10078
10209       CONTINUE
C
            IF (.NOT.(IWRK(IPER+9).NE.IWRK(IPE1+9))) GO TO 10211
              IERR=14
              GO TO 10078
10211       CONTINUE
C
            IPTN=IWRK(IPER+9)
C
            CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                 RWRK(IPTN+2),RWRK(IPE1+3),
     +                 RWRK(IPER+3),YINT        )
C
            RWRK(IPTN  )=XINT
            RWRK(IPTN+1)=RWRK(IPER+1)+(YINT-RWRK(IPER+2))*RWRK(IPER+3)
            RWRK(IPTN+2)=YINT
C
C Check for a right intersection.
C
          GO TO 10185
10203     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.0.AND.IWRK(IPE1+5).EQ.1.AND.IWRK(IP
     +E2+4).EQ.1.AND.IWRK(IPE2+5).EQ.1).OR.(IWRK(IPE1+4).EQ.1.AND.IWRK(I
     +PE1+5).EQ.1.AND.IWRK(IPE2+4).EQ.0.AND.IWRK(IPE2+5).EQ.1))) GO TO 1
     +0213
C
C Process a right intersection.
C
            IF (.NOT.(IWRK(IPE2+9).EQ.0)) GO TO 10214
              IERR=15
              GO TO 10078
10214       CONTINUE
C
            IPET=IWRK(IPE2+7)
            IPEL=0
C
10216       CONTINUE
              IF (IPET.EQ.0) GO TO 10217
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10218
                IPEL=IPET
                GO TO 10217
10218         CONTINUE
              IPET=IWRK(IPET+7)
            GO TO 10216
10217       CONTINUE
C
            IF (.NOT.(IPEL.EQ.0)) GO TO 10219
              IERR=16
              GO TO 10078
10219       CONTINUE
C
            IF (.NOT.(IWRK(IPEL+9).NE.IWRK(IPE2+9))) GO TO 10221
              IERR=17
              GO TO 10078
10221       CONTINUE
C
            IPTN=IWRK(IPEL+9)
C
            CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                 RWRK(IPTN+2),RWRK(IPEL+3),
     +                 RWRK(IPE2+3),YINT        )
C
            RWRK(IPTN  )=RWRK(IPEL+1)+(YINT-RWRK(IPEL+2))*RWRK(IPEL+3)
            RWRK(IPTN+1)=XINT
            RWRK(IPTN+2)=YINT
C
C Check for a local maximum.
C
          GO TO 10185
10213     CONTINUE
          IF (.NOT.((IWRK(IPE1+4).EQ.1.AND.IWRK(IPE1+5).EQ.1.AND.IWRK(IP
     +E2+4).EQ.0.AND.IWRK(IPE2+5).EQ.0).OR.(IWRK(IPE1+4).EQ.0.AND.IWRK(I
     +PE1+5).EQ.1.AND.IWRK(IPE2+4).EQ.1.AND.IWRK(IPE2+5).EQ.0))) GO TO 1
     +0223
C
C Process a local maximum.
C
            IF (.NOT.(IWRK(IPE1+9).EQ.0.OR.IWRK(IPE2+9).EQ.0)) GO TO 102
     +24
              IERR=18
              GO TO 10078
10224       CONTINUE
C
            IPET=IWRK(IPE1+7)
            IPEL=0
C
10226       CONTINUE
              IF (IPET.EQ.0) GO TO 10227
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10228
                IPEL=IPET
                GO TO 10227
10228         CONTINUE
              IPET=IWRK(IPET+7)
            GO TO 10226
10227       CONTINUE
C
            IF (.NOT.(IPEL.EQ.0)) GO TO 10229
              IERR=19
              GO TO 10078
10229       CONTINUE
C
            IF (.NOT.(IWRK(IPEL+9).NE.IWRK(IPE1+9))) GO TO 10231
              IERR=20
              GO TO 10078
10231       CONTINUE
C
            IPTN=IWRK(IPEL+9)
C
            CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                 RWRK(IPTN+2),RWRK(IPEL+3),
     +                 RWRK(IPE1+3),YINT        )
C
            IWRK(IPTN)=IG03
            IG03=IPTN
C
            IPET=IWRK(IPE2+6)
            IPER=0
C
10233       CONTINUE
              IF (IPET.EQ.0) GO TO 10234
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10235
                IPER=IPET
                GO TO 10234
10235         CONTINUE
              IPET=IWRK(IPET+6)
            GO TO 10233
10234       CONTINUE
C
            IF (.NOT.(IPER.EQ.0)) GO TO 10236
              IERR=21
              GO TO 10078
10236       CONTINUE
C
            IF (.NOT.(IWRK(IPER+9).NE.IWRK(IPE2+9))) GO TO 10238
              IERR=22
              GO TO 10078
10238       CONTINUE
C
            IPTN=IWRK(IPER+9)
C
            CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                 RWRK(IPTN+2),RWRK(IPE2+3),
     +                 RWRK(IPER+3),YINT        )
C
            RWRK(IPTN  )=RWRK(IPEL+1)+(YINT-RWRK(IPEL+2))*RWRK(IPEL+3)
            RWRK(IPTN+1)=RWRK(IPER+1)+(YINT-RWRK(IPER+2))*RWRK(IPER+3)
            RWRK(IPTN+2)=YINT
C
            IWRK(IPEL+9)=IPTN
C
            IWRK(IPE1+9)=0
            IWRK(IPE2+9)=0
C
10185     CONTINUE
10223     CONTINUE
C
C Swap the positions of edge 1 and edge 2 in the AET.
C
          IF (IWRK(IPE1+7).NE.0) IWRK(IWRK(IPE1+7)+6)=IPE2
          IF (IWRK(IPE2+6).NE.0) IWRK(IWRK(IPE2+6)+7)=IPE1
          IWRK(IPE1+6)=IWRK(IPE2+6)
          IWRK(IPE2+7)=IWRK(IPE1+7)
          IWRK(IPE1+7)=IPE2
          IWRK(IPE2+6)=IPE1
C
C If the AET started with edge 1, it now starts with edge 2.
C
          IF (IAET.EQ.IPE1) IAET=IPE2
C
C Exchange the trapezoid-node pointers of edges 1 and 2.
C
          IDUM=IWRK(IPE1+9)
          IWRK(IPE1+9)=IWRK(IPE2+9)
          IWRK(IPE2+9)=IDUM
C
C Advance to the next point of intersection in the list.
C
          IINT=IWRK(IINT+4)
C
C Quit if there are no more points of intersection to process.
C
          IF (IINT.EQ.0) GO TO 10240
C
C End of loop on points of intersection.
C
        GO TO 10159
10240   CONTINUE
C
C End of internal procedure to process a list of intersections.
C
      GO TO (10123,10154) , L10124
C
C The following internal procedure processes an edge in the AET that is
C terminating at the top of the current scanbeam.  The variable ITMP
C points to the edge that is to be processed.  If the edge is removed
C from the AET (which can happen), the procedure must adjust the value
C of ITMP so that the next-node pointer in the AET node that ITMP
C points at properly specifies the next AET node to be examined.
C
10126 CONTINUE
C
C Find the index, in the user's arrays, of the end point of the
C successor edge.
C
        INNP=ABS(IWRK(ITMP+8))+SIGN(1,IWRK(ITMP+8))
C
C Extract the X and Y coordinates of the end point of the successor
C edge.
C
        IF (.NOT.(IWRK(ITMP+4).EQ.0)) GO TO 10241
          IF (.NOT.(INNP.LT.1)) GO TO 10242
            INNP=INNP+LCCP
          GO TO 10243
10242     CONTINUE
          IF (.NOT.(INNP.GT.LCCP)) GO TO 10244
            INNP=INNP-LCCP
10243     CONTINUE
10244     CONTINUE
          XCNP=XCCP(INNP)
          YCNP=YCCP(INNP)
        GO TO 10245
10241   CONTINUE
          IF (.NOT.(INNP.LT.1)) GO TO 10246
            INNP=INNP+LCSP
          GO TO 10247
10246     CONTINUE
          IF (.NOT.(INNP.GT.LCSP)) GO TO 10248
            INNP=INNP-LCSP
10247     CONTINUE
10248     CONTINUE
          XCNP=XCSP(INNP)
          YCNP=YCSP(INNP)
10245   CONTINUE
C
C Check the vertical position of the end point of the successor edge.
C
        IF (.NOT.(YCNP.GE.YTOS)) GO TO 10249
C
C The end point of the successor edge is above the top of the scanbeam.
C
C Check whether the edge is contributing to the formation of trapezoids.
C
          IF (.NOT.(IWRK(ITMP+9).NE.0)) GO TO 10250
C
C The edge is contributing to the formation of trapezoids.  Output a
C trapezoid.
C
            IPTN=IWRK(ITMP+9)
C
            IPET=IWRK(ITMP+7)
            IPEL=0
C
10251       CONTINUE
              IF (IPET.EQ.0) GO TO 10252
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10253
                IPEL=IPET
                GO TO 10252
10253         CONTINUE
              IPET=IWRK(IPET+7)
            GO TO 10251
10252       CONTINUE
C
            IF (.NOT.(IPEL.NE.0)) GO TO 10254
              IF (.NOT.(IWRK(IPEL+9).EQ.IPTN)) GO TO 10255
                IPE1=IPEL
                IPE2=ITMP
                GO TO 104
10255         CONTINUE
10254       CONTINUE
C
            IPET=IWRK(ITMP+6)
            IPER=0
C
10256       CONTINUE
              IF (IPET.EQ.0) GO TO 10257
              IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10258
                IPER=IPET
                GO TO 10257
10258         CONTINUE
              IPET=IWRK(IPET+6)
            GO TO 10256
10257       CONTINUE
C
            IF (.NOT.(IPER.NE.0)) GO TO 10259
              IF (.NOT.(IWRK(IPER+9).EQ.IPTN)) GO TO 10260
                IPE1=ITMP
                IPE2=IPER
                GO TO 104
10260         CONTINUE
10259       CONTINUE
C
            IERR=23
            GO TO 10078
C
  104       CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                 RWRK(IPTN+2),RWRK(IPE1+3),
     +                 RWRK(IPE2+3),YTOS        )
C
            RWRK(IPTN  )=RWRK(IPE1+1)+(YTOS-RWRK(IPE1+2))*RWRK(IPE1+3)
            RWRK(IPTN+1)=RWRK(IPE2+1)+(YTOS-RWRK(IPE2+2))*RWRK(IPE2+3)
            RWRK(IPTN+2)=YTOS
C
10250     CONTINUE
C
C Update the node to represent its successor edge.
C
          RWRK(ITMP+1)=XCNP
          RWRK(ITMP+2)=YCNP
C
          IF (.NOT.(YCNP.NE.YTOS)) GO TO 10262
            RWRK(ITMP+3)=(XCNP-RWRK(ITMP))/(YCNP-YTOS)
          GO TO 10263
10262     CONTINUE
            RWRK(ITMP+3)=SIGN(RBIG,XCNP-RWRK(ITMP))
10263     CONTINUE
C
          IWRK(ITMP+8)=SIGN(INNP,IWRK(ITMP+8))
C
        GO TO 10264
10249   CONTINUE
C
C The end point of the successor edge is below the top of the scanbeam.
C We have arrived at a local maximum, so handle that case.
C
          IF (.NOT.(IWRK(ITMP+6).EQ.0)) GO TO 10265
            IERR=24
            GO TO 10078
10265     CONTINUE
C
          IF (.NOT.(IWRK(ITMP+9).NE.0)) GO TO 10267
C
            IPE1=ITMP
            IPE2=IWRK(ITMP+6)
C
            IF (.NOT.(IWRK(IPE1+9).EQ.IWRK(IPE2+9))) GO TO 10268
C
              IPTN=IWRK(IPE1+9)
C
              CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                   RWRK(IPTN+2),RWRK(IPE1+3),
     +                   RWRK(IPE2+3),YTOS        )
C
              IWRK(IPTN)=IG03
              IG03=IPTN
C
            GO TO 10269
10268       CONTINUE
C
              IPET=IWRK(IPE1+7)
              IPEL=0
C
10270         CONTINUE
                IF (IPET.EQ.0) GO TO 10271
                IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10272
                  IPEL=IPET
                  GO TO 10271
10272           CONTINUE
                IPET=IWRK(IPET+7)
              GO TO 10270
10271         CONTINUE
C
              IF (.NOT.(IPEL.EQ.0)) GO TO 10273
                IERR=25
                GO TO 10078
10273         CONTINUE
C
              IF (.NOT.(IWRK(IPEL+9).NE.IWRK(IPE1+9))) GO TO 10275
                IERR=26
                GO TO 10078
10275         CONTINUE
C
              IPTN=IWRK(IPEL+9)
C
              CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                   RWRK(IPTN+2),RWRK(IPEL+3),
     +                   RWRK(IPE1+3),YTOS        )
C
              IWRK(IPTN)=IG03
              IG03=IPTN
C
              IPET=IWRK(IPE2+6)
              IPER=0
C
10277         CONTINUE
                IF (IPET.EQ.0) GO TO 10278
                IF (.NOT.(IWRK(IPET+9).NE.0)) GO TO 10279
                  IPER=IPET
                  GO TO 10278
10279           CONTINUE
                IPET=IWRK(IPET+6)
              GO TO 10277
10278         CONTINUE
C
              IF (.NOT.(IPER.EQ.0)) GO TO 10280
                IERR=27
                GO TO 10078
10280         CONTINUE
C
              IF (.NOT.(IWRK(IPER+9).NE.IWRK(IPE2+9))) GO TO 10282
                IERR=28
                GO TO 10078
10282         CONTINUE
C
              IPTN=IWRK(IPER+9)
C
              CALL URPT (RWRK(IPTN  ),RWRK(IPTN+1),
     +                   RWRK(IPTN+2),RWRK(IPE2+3),
     +                   RWRK(IPER+3),YTOS        )
C
              RWRK(IPTN  )=RWRK(IPEL+1)+(YTOS-RWRK(IPEL+2))*
     +                     RWRK(IPEL+3)
              RWRK(IPTN+1)=RWRK(IPER+1)+(YTOS-RWRK(IPER+2))*
     +                     RWRK(IPER+3)
              RWRK(IPTN+2)=YTOS
C
              IWRK(IPEL+9)=IPTN
C
10269       CONTINUE
C
10267     CONTINUE
C
C Delete from the AET the edge ITMP and the edge that follows it.  The
C nodes go back on the garbage list for 10-word nodes.
C
          ITM1=IWRK(ITMP+7)
          ITM2=IWRK(IWRK(ITMP+6)+6)
C
          IF (.NOT.(ITM1.EQ.0)) GO TO 10284
            IAET=ITM2
          GO TO 10285
10284     CONTINUE
            IWRK(ITM1+6)=ITM2
10285     CONTINUE
C
          IF (ITM2.NE.0) IWRK(ITM2+7)=ITM1
C
          IWRK(ITMP)=IWRK(ITMP+6)
          IWRK(IWRK(ITMP))=IG10
          IG10=ITMP
C
C Adjust the pointer into the AET so as to continue looping properly.
C
          ITMP=IWRK(ITMP+6)
C
10264   CONTINUE
C
      GO TO (10125,10158) , L10126
C
C Error exits.
C
10003 CONTINUE
        IERR=1
        RETURN
C
10006 CONTINUE
        IERR=2
        RETURN
C
10017 CONTINUE
        IERR=3
        RETURN
C
10078 CONTINUE
        IERR=3+IERR
        RETURN
C
      END
