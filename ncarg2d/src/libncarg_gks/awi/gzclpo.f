C
C	$Id: gzclpo.f,v 1.1 1994-05-19 19:26:58 fred Exp $
C
      SUBROUTINE GZCLPO (XCCP,YCCP,NCCP,XCSP,YCSP,NCSP,
     +                   RWRK,IWRK,NWRK,IERR)
C
      DIMENSION XCCP(NCCP),YCCP(NCCP)
      DIMENSION XCSP(NCSP),YCSP(NCSP)
      DIMENSION RWRK(NWRK),IWRK(NWRK)
C
C The subroutine GZCLPO, given X/Y coordinates defining the vertices
C of a "clip polygon" in (XCCP(I),I=1,NCCP) and (YCCP(I),I=1,NCCP),
C X/Y coordinates defining the vertices of a "subject polygon" in
C (XCSP(I),I=1,NCSP) and (YCSP(I),I=1,NCSP), and the real and integer
C workspaces RWRK and IWRK, each of which is of length NWRK, generates
C the set of polygons representing pieces of the subject polygon lying
C inside the clip polygon and delivers each of them to be plotted
C with GZPUTR.  Errors, in general, result in an immediate RETURN 
C with IERR non-zero; on a normal return, IERR
C is zero.
C
C For most efficient use of memory, IWRK and RWRK should be EQUIVALENCEd
C to each other.
C
C The algorithm used is that described by Bala R. Vatti in the article
C "A Generic Solution to Polygon Clipping", which was published in the
C July, 1992, issue of "Communications of the ACM" (Vol. 35, No. 7).
C
C This code was produced from an iftran master coded by David Kennison
C at NCAR in 1994.
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
      IF (LCCP.LT.3) THEN
        GO TO 10002
      END IF
C
      IF (LCSP.LT.3) THEN
        GO TO 10004
      END IF
C
C Check to see if the input polygon is entirely within the clipping
C rectangle and treat that case specially.
C
      XCSPMN = XCSP(1)
      XCSPMX = XCSP(1)
      YCSPMN = YCSP(1)
      YCSPMX = YCSP(1)
      DO 100 I=1,LCSP
        XCSPMN = MIN(XCSPMN,XCSP(I))
        XCSPMX = MAX(XCSPMX,XCSP(I))
        YCSPMN = MIN(YCSPMN,YCSP(I))
        YCSPMX = MAX(YCSPMX,YCSP(I))
  100 CONTINUE
      IF (XCSPMN.GE.XCCP(1) .AND. XCSPMX.LE.XCCP(2) .AND.
     +    YCSPMN.GE.YCCP(2) .AND. YCSPMX.LE.YCCP(3)) THEN 
        CALL GZPUTR(NCSP,NCSP,XCSP,YCSP,1,IERR)
        RETURN
      ENDIF
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
      DO 10005 I=1,LCCP-1
        IF (YCCP(I).NE.YCCP(I+1)) THEN
          INXT=I
          YNXT=YCCP(INXT)
          GO TO 101
        END IF
10005 CONTINUE
C
C If there is no such starting place, take an error exit.
C
      GO TO 10002
C
C Otherwise, go through the entire polygon from the starting position,
C finding all those places where the Y value increases after having
C decreased.  Each such place constitutes one of the local minima in
C the LMT.
C
  101 IDIR=0
C
      DO 10007 I=0,LCCP
        ILST=INXT
        YLST=YNXT
        INXT=INXT+1
        IF (INXT.GT.LCCP) INXT=INXT-LCCP
        YNXT=YCCP(INXT)
        IF (YNXT.LT.YLST) THEN
          IDIR=-1
        ELSE IF (YNXT.GT.YLST) THEN
          IF (IDIR.LT.0) THEN
            ILMN=IPWL+1
            IPWL=IPWL+3
            IF (IPWL.GE.IPWU) THEN
              GO TO 10009
            END IF
            RWRK(ILMN)=YLST
            IWRK(ILMN+1)=ILST
            ITM1=0
            ITM2=ILMT
10010       CONTINUE
              IF (ITM2.EQ.0) GO TO 10011
              IF (RWRK(ILMN).LE.RWRK(ITM2)) GO TO 10011
              ITM1=ITM2
              ITM2=IWRK(ITM2+2)
            GO TO 10010
10011       CONTINUE
            IF (ITM1.EQ.0) THEN
              ILMT=ILMN
            ELSE
              IWRK(ITM1+2)=ILMN
            END IF
            IWRK(ILMN+2)=ITM2
          END IF
          IDIR=+1
        END IF
10007 CONTINUE
C
C In the same way, search for local minima of the subject polygon.
C
      INXT=0
C
      DO 10012 I=1,LCSP-1
        IF (YCSP(I).NE.YCSP(I+1)) THEN
          INXT=I
          YNXT=YCSP(INXT)
          GO TO 102
        END IF
10012 CONTINUE
C
      GO TO 10004
C
  102 IDIR=0
C
      DO 10014 I=0,LCSP
        ILST=INXT
        YLST=YNXT
        INXT=INXT+1
        IF (INXT.GT.LCSP) INXT=INXT-LCSP
        YNXT=YCSP(INXT)
        IF (YNXT.LT.YLST) THEN
          IDIR=-1
        ELSE IF (YNXT.GT.YLST) THEN
          IF (IDIR.LT.0) THEN
            ILMN=IPWL+1
            IPWL=IPWL+3
            IF (IPWL.GE.IPWU) THEN
              GO TO 10009
            END IF
            RWRK(ILMN)=YLST
            IWRK(ILMN+1)=LCCP+ILST
            ITM1=0
            ITM2=ILMT
10016       CONTINUE
              IF (ITM2.EQ.0) GO TO 10017
              IF (RWRK(ILMN).LE.RWRK(ITM2)) GO TO 10017
              ITM1=ITM2
              ITM2=IWRK(ITM2+2)
            GO TO 10016
10017       CONTINUE
            IF (ITM1.EQ.0) THEN
              ILMT=ILMN
            ELSE
              IWRK(ITM1+2)=ILMN
            END IF
            IWRK(ILMN+2)=ITM2
          END IF
          IDIR=+1
        END IF
10014 CONTINUE
C
C Initialize the output polygon list pointer to indicate that no
C polygons have been generated yet:
C
      IPPL=0
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
10018 CONTINUE
C
C YBOS is the Y coordinate of the bottom of the new scanbeam.
C
        YBOS=YTOS
C
C Loop through those local minima in the LMT having Y coordinate
C YBOS; for each, add to the AET the pair of edges that start at
C that local minimum.
C
10019   CONTINUE
C
C Quit if the end of the LMT has been reached.
C
          IF (ILMT.EQ.0) GO TO 10020
C
C Quit if the Y coordinate of the next local minimum is too large.
C
          IF (RWRK(ILMT).GT.YBOS) GO TO 10020
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
          IF (IMIN.LE.LCCP) THEN
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
          ELSE
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
          END IF
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
10021     CONTINUE
C
C Exit if the end of the AET has been reached.
C
            IF (ITM2.EQ.0) GO TO 10022
C
C Exit if the new local minimum fits between elements ITM1 and ITM2 of
C the AET.
C
            IF (XMIN.LE.RWRK(ITM2)) GO TO 10022
C
C Advance to the next position in the AET.
C
            ITM1=ITM2
            ITM2=IWRK(ITM2+6)
C
C Update the flags that say where we are relative to the clip and
C subject polygons.
C
            IF (IWRK(ITM1+4).EQ.0) THEN
              IOCP=1-IOCP
            ELSE
              IOSP=1-IOSP
            END IF
C
C End of loop through the AET.
C
          GO TO 10021
10022     CONTINUE
C
C Create two new nodes in the AET.  Either re-use 10-word nodes from the
C garbage list or create new ones.
C
          IF (IG10.NE.0) THEN
            IPNL=IG10
            IG10=IWRK(IG10)
          ELSE
            IPNL=IPWL+1
            IPWL=IPWL+10
            IF (IPWL.GE.IPWU) THEN
              GO TO 10009
            END IF
          END IF
C
          IF (IG10.NE.0) THEN
            IPNN=IG10
            IG10=IWRK(IG10)
          ELSE
            IPNN=IPWL+1
            IPWL=IPWL+10
            IF (IPWL.GE.IPWU) THEN
              GO TO 10009
            END IF
          END IF
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
          IF (YLST.NE.YMIN) THEN
            RWRK(IPNL+3)=(XLST-XMIN)/(YLST-YMIN)
          ELSE
            RWRK(IPNL+3)=SIGN(RBIG,XLST-XMIN)
          END IF
C
          IF (YNXT.NE.YMIN) THEN
            RWRK(IPNN+3)=(XNXT-XMIN)/(YNXT-YMIN)
          ELSE
            RWRK(IPNN+3)=SIGN(RBIG,XNXT-XMIN)
          END IF
C
          IWRK(IPNL+4)=ICOS
          IWRK(IPNN+4)=ICOS
C
          IF (ICOS.EQ.0) THEN
            IOPO=IOCP
          ELSE
            IOPO=IOSP
          END IF
C
          IF (RWRK(IPNL+3).LT.RWRK(IPNN+3)) THEN
C
            IPE1=IPNL
            IPE2=IPNN
C
          ELSE
C
            IPE1=IPNN
            IPE2=IPNL
C
          END IF
C
          IWRK(IPE1+5)=IOPO
          IWRK(IPE2+5)=1-IOPO
C
          IF (ITM1.EQ.0) THEN
            IAET=IPE1
          ELSE
            IWRK(ITM1+6)=IPE1
          END IF
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
C If the edges are "contributing", create an output polygon for them
C to "contribute" to and put the initial point in it; otherwise, zero
C the output-polygon pointers.
C
          IF ((IOCP.NE.0.AND.IOSP.NE.0).OR.(IOCP.EQ.0.AND.IOSP.NE.0.AND.
     +ICOS.EQ.0).OR.(IOCP.NE.0.AND.IOSP.EQ.0.AND.ICOS.NE.0)) THEN
C
            IF (IG03.NE.0) THEN
              IPSN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPSN=IPWU
            END IF
C
            RWRK(IPSN  )=XMIN
            RWRK(IPSN+1)=YMIN
            IWRK(IPSN+2)=0
C
            IF (IG03.NE.0) THEN
              IPPN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPPN=IPWU
            END IF
C
            IWRK(IPPN  )=IPSN
            IWRK(IPPN+1)=IPSN
            IWRK(IPPN+2)=IPPL
C
            IPPL=IPPN
            IWRK(IPNL+9)=IPPN
            IWRK(IPNN+9)=IPPN
C
          ELSE
C
            IWRK(IPNL+9)=0
            IWRK(IPNN+9)=0
C
          END IF
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
        GO TO 10019
10020   CONTINUE
C
C At this point, if the AET is empty, the scanbeam loop is exited.
C
  103 CONTINUE
        IF (IAET.EQ.0) GO TO 10027
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
10028   CONTINUE
C
C Check for a horizontal section.
C
          IF (YTOS.EQ.YBOS) THEN
C
C Step through points in the user's arrays until the end of the
C horizontal section is reached, updating the X coordinate and the
C index of the successor edge as we go.
C
            INNP=ABS(IWRK(ITMP+8))
C
10029       CONTINUE
C
              IF (IWRK(ITMP+4).EQ.0) THEN
                IF (INNP.LT.1) THEN
                  INNP=INNP+LCCP
                ELSE IF (INNP.GT.LCCP) THEN
                  INNP=INNP-LCCP
                END IF
                IF (YCCP(INNP).NE.YBOS) GO TO 10030
                RWRK(ITMP)=XCCP(INNP)
              ELSE
                IF (INNP.LT.1) THEN
                  INNP=INNP+LCSP
                ELSE IF (INNP.GT.LCSP) THEN
                  INNP=INNP-LCSP
                END IF
                IF (YCSP(INNP).NE.YBOS) GO TO 10030
                RWRK(ITMP)=XCSP(INNP)
              END IF
C
              IWRK(ITMP+8)=SIGN(INNP,IWRK(ITMP+8))
              INNP=INNP+SIGN(1,IWRK(ITMP+8))
C
            GO TO 10029
10030       CONTINUE
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
C Generate the list of intersection points, either to the left ...
C
            IF (IWRK(ITMP+7).NE.0) THEN
C
              IDUM=IWRK(ITMP+7)
C
10031         CONTINUE
C
                IF (RWRK(ITMP).GE.RWRK(IDUM)) GO TO 10032
C
                IF (IINT.EQ.0) THEN
                  IINT=IPWL+1
                ELSE
                  IWRK(IINN+4)=IPWL+1
                END IF
C
                IINN=IPWL+1
                IPWL=IPWL+5
C
                IF (IPWL.GE.IPWU) THEN
                  GO TO 10009
                END IF
C
                RWRK(IINN)=RWRK(IDUM)
                RWRK(IINN+1)=YBOS
                IWRK(IINN+2)=IDUM
                IWRK(IINN+3)=ITMP
                IWRK(IINN+4)=0
C
                IDUM=IWRK(IDUM+7)
C
                IF (IDUM.EQ.0) GO TO 10032
C
              GO TO 10031
10032         CONTINUE
C
            END IF
C
C ... or to the right.
C
            IF (IWRK(ITMP+6).NE.0) THEN
C
              IDUM=IWRK(ITMP+6)
C
10034         CONTINUE
C
                IF (RWRK(ITMP).LE.RWRK(IDUM)) GO TO 10035
C
                IF (IINT.EQ.0) THEN
                  IINT=IPWL+1
                ELSE
                  IWRK(IINN+4)=IPWL+1
                END IF
C
                IINN=IPWL+1
                IPWL=IPWL+5
C
                IF (IPWL.GE.IPWU) THEN
                  GO TO 10009
                END IF
C
                RWRK(IINN)=RWRK(IDUM)
                RWRK(IINN+1)=YBOS
                IWRK(IINN+2)=ITMP
                IWRK(IINN+3)=IDUM
                IWRK(IINN+4)=0
C
                IDUM=IWRK(IDUM+6)
C
                IF (IDUM.EQ.0) GO TO 10035
C
              GO TO 10034
10035         CONTINUE
C
            END IF
C
C If any intersection points were found, process them and then reclaim
C the space used for the list.
C
            IF (IINT.NE.0) THEN
              L10038=    1
              GO TO 10038
10037         CONTINUE
              IPWL=ISWL
            END IF
C
C The horizontal edge is terminating at this point, so handle that.
C
            L10040=    1
            GO TO 10040
10039       CONTINUE
C
C Go back to see if the AET is empty now and, if not, to rescan it for
C more horizontal segments.
C
            GO TO 103
C
          END IF
C
C Move to the next node in the AET.
C
          ITMP=IWRK(ITMP+6)
C
C Quit if there are none.
C
          IF (ITMP.EQ.0) GO TO 10041
C
C Update the variable that says where the top of the scanbeam is.
C
          YTOS=MIN(YTOS,RWRK(ITMP+2))
C
        GO TO 10028
10041   CONTINUE
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
        IF (IPWL.GE.IPWU) THEN
          GO TO 10009
        END IF
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
10043   CONTINUE
C
          IF (ITMP.EQ.0) GO TO 10044
C
          XTMP=RWRK(ITMP+1)+(YTOS-RWRK(ITMP+2))*RWRK(ITMP+3)
C
          IST1=0
          IST2=ISET
C
10045     CONTINUE
C
            IF (IST2.EQ.0) GO TO 10046
            IF (XTMP.GT.RWRK(IST2)) GO TO 10046
C
            IF (XTMP.EQ.RWRK(IST2)) THEN
C
              IST3=IWRK(IST2+2)
              IST4=0
C
10047         CONTINUE
C
                IF (IST3.EQ.0) GO TO 10048
                IF (XTMP.NE.RWRK(IST3)) GO TO 10048
C
                IF (IWRK(IWRK(IST3+1)+4).EQ.IWRK(ITMP+4).AND.IWRK(IWRK(I
     +ST3+1)+8).EQ.-IWRK(ITMP+8)) THEN
                  IST4=1
                  GO TO 10048
                END IF
C
                IST3=IWRK(IST3+2)
C
              GO TO 10047
10048         CONTINUE
C
              IF (IST4.EQ.0) GO TO 10046
C
              XINT=XTMP
              YINT=YTOS
C
            ELSE
C
              IF (ABS(RWRK(ITMP+3)-RWRK(IWRK(IST2+1)+3)).GT.1.E-6) THEN
                YINT=YBOS-(RWRK(ITMP  )-RWRK(IWRK(IST2+1)  ))/
     +                    (RWRK(ITMP+3)-RWRK(IWRK(IST2+1)+3))
              ELSE
                YINT=.5*(YBOS+YTOS)
              END IF
C
              XINT=RWRK(ITMP+1)+(YINT-RWRK(ITMP+2))*RWRK(ITMP+3)
C
            END IF
C
            IINN=IPWL+1
            IPWL=IPWL+5
C
            IF (IPWL.GE.IPWU) THEN
              GO TO 10009
            END IF
C
            RWRK(IINN)=XINT
            RWRK(IINN+1)=YINT
            IWRK(IINN+2)=IWRK(IST2+1)
            IWRK(IINN+3)=ITMP
C
            IIN1=0
            IIN2=IINT
C
10050       CONTINUE
              IF (IIN2.EQ.0) GO TO 10051
              IF (RWRK(IINN+1).LE.RWRK(IIN2+1)) GO TO 10051
              IIN1=IIN2
              IIN2=IWRK(IIN2+4)
            GO TO 10050
10051       CONTINUE
C
            IF (IIN1.EQ.0) THEN
              IINT=IINN
            ELSE
              IWRK(IIN1+4)=IINN
            END IF
C
            IWRK(IINN+4)=IIN2
C
            IST1=IST2
            IST2=IWRK(IST2+2)
C
          GO TO 10045
10046     CONTINUE
C
          ISTN=IPWL+1
          IPWL=IPWL+3
C
          IF (IPWL.GE.IPWU) THEN
            GO TO 10009
          END IF
C
          IF (IST1.EQ.0) THEN
            ISET=ISTN
          ELSE
            IWRK(IST1+2)=ISTN
          END IF
C
          RWRK(ISTN)=XTMP
          IWRK(ISTN+1)=ITMP
          IWRK(ISTN+2)=IST2
C
          ITMP=IWRK(ITMP+6)
C
        GO TO 10043
10044   CONTINUE
C
C If intersections have been found, process them.
C
        IF (IINT.NE.0) THEN
          L10038=    2
          GO TO 10038
10053     CONTINUE
        END IF
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
10054   CONTINUE
C
C Exit if all the edges have been done.
C
          IF (ITMP.EQ.0) GO TO 10055
C
C Update the X coordinate to its position at the top of the scanbeam.
C
          RWRK(ITMP)=RWRK(ITMP+1)+(YTOS-RWRK(ITMP+2))*RWRK(ITMP+3)
C
C If the edge terminates at the top of this scanbeam, process it.
C
          IF (RWRK(ITMP+2).EQ.YTOS) THEN
            L10040=    2
            GO TO 10040
10056       CONTINUE
          END IF
C
C Advance to the next edge in the AET.
C
          ITMP=IWRK(ITMP+6)
C
C End of loop on edges in the AET.
C
        GO TO 10054
10055   CONTINUE
C
C End of scanbeam loop.
C
      GO TO 10018
10027 CONTINUE
C
C Dump out all the polygons that have been formed.
C
      MXYC=(IPWU-1-IPWL)/2
      IPXC=IPWL
      IPYC=IPWL+MXYC
10057 CONTINUE
      IF (.NOT.(IPPL.NE.0)) GO TO 10058
        NXYC=0
        ITMP=IWRK(IPPL)
10059   CONTINUE
        IF (.NOT.(ITMP.NE.0)) GO TO 10060
          NXYC=NXYC+1
          IF (NXYC.GE.MXYC) THEN
            GO TO 10009
          END IF
          RWRK(IPXC+NXYC)=RWRK(ITMP)
          RWRK(IPYC+NXYC)=RWRK(ITMP+1)
          ITMP=IWRK(ITMP+2)
        GO TO 10059
10060   CONTINUE
        NXYC=NXYC+1
        RWRK(IPXC+NXYC)=RWRK(IWRK(IPPL))
        RWRK(IPYC+NXYC)=RWRK(IWRK(IPPL)+1)
        CALL GZPUTR(NXYC,NXYC,RWRK(IPXC+1),RWRK(IPYC+1),1,IERR)
        IPPL=IWRK(IPPL+2)
      GO TO 10057
10058 CONTINUE
C
C Normal exit.
C
      RETURN
C
C The following internal procedure processes the list of intersection
C points that IINT points to.  On entry, it may be assumed that IINT
C has been verified to be non-zero.
C
10038 CONTINUE
C
C Loop through all the points of intersection.
C
10062   CONTINUE
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
          IF (IWRK(IPE1+6).NE.IPE2) THEN
C
            IIN1=IINT
            IIN2=IWRK(IINT+4)
C
10063       CONTINUE
C
              IF (IIN2.EQ.0) THEN
                IERR=1
                GO TO 10065
              END IF
C
              IF (IWRK(IWRK(IIN2+2)+6).EQ.IWRK(IIN2+3)) GO TO 10066
C
              IIN1=IIN2
              IIN2=IWRK(IIN2+4)
C
            GO TO 10063
10066       CONTINUE
C
            IWRK(IIN1+4)=IWRK(IIN2+4)
            IWRK(IIN2+4)=IINT
            IINT=IIN2
C
            GO TO 201
C
          END IF
C
C Check whether or not both edges are from the same input polygon.
C
          IF (IWRK(IPE1+4).EQ.IWRK(IPE2+4)) THEN
C
C Both edges are from the clip polygon or both are from the subject
C polygon.  If edge 1 is contributing to an output polygon, then edge
C 2 should be also, in which case we add the point of intersection to
C the left side of one polygon and to the right side of the other
C polygon.  In either case, we must swap the left/right flags in the
C two edges.
C
            IF (IWRK(IPE1+9).NE.0.OR.IWRK(IPE2+9).NE.0) THEN
C
              IF (IWRK(IPE1+9).EQ.0.OR.IWRK(IPE2+9).EQ.0) THEN
                IERR=2
                GO TO 10065
              END IF
C
              IF (IG03.NE.0) THEN
                IPSN=IG03
                IG03=IWRK(IG03)
              ELSE
                IPWU=IPWU-3
                IF (IPWU.LE.IPWL) THEN
                  GO TO 10009
                END IF
                IPSN=IPWU
              END IF
C
              RWRK(IPSN  )=XINT
              RWRK(IPSN+1)=YINT
C
              IF (IWRK(IPE1+5).EQ.0) THEN
                IWRK(IPSN+2)=IWRK(IWRK(IPE1+9))
                IWRK(IWRK(IPE1+9))=IPSN
              ELSE
                IWRK(IPSN+2)=0
                IWRK(IWRK(IWRK(IPE1+9)+1)+2)=IPSN
                IWRK(IWRK(IPE1+9)+1)=IPSN
              END IF
C
              IF (IG03.NE.0) THEN
                IPSN=IG03
                IG03=IWRK(IG03)
              ELSE
                IPWU=IPWU-3
                IF (IPWU.LE.IPWL) THEN
                  GO TO 10009
                END IF
                IPSN=IPWU
              END IF
C
              RWRK(IPSN  )=XINT
              RWRK(IPSN+1)=YINT
C
              IF (IWRK(IPE2+5).EQ.0) THEN
                IWRK(IPSN+2)=IWRK(IWRK(IPE2+9))
                IWRK(IWRK(IPE2+9))=IPSN
              ELSE
                IWRK(IPSN+2)=0
                IWRK(IWRK(IWRK(IPE2+9)+1)+2)=IPSN
                IWRK(IWRK(IPE2+9)+1)=IPSN
              END IF
C
            END IF
C
            IDUM=IWRK(IPE1+5)
            IWRK(IPE1+5)=IWRK(IPE2+5)
            IWRK(IPE2+5)=IDUM
C
C One edge is from the clip polygon and the other is from the
C subject polygon.  Check for a local minimum.
C
          ELSE IF ((IWRK(IPE1+4).EQ.1.AND.IWRK(IPE1+5).EQ.1.AND.IWRK(IPE
     +2+4).EQ.0.AND.IWRK(IPE2+5).EQ.0).OR.(IWRK(IPE1+4).EQ.0.AND.IWRK(IP
     +E1+5).EQ.1.AND.IWRK(IPE2+4).EQ.1.AND.IWRK(IPE2+5).EQ.0)) THEN
C
C Process a local minimum.
C
            IF (IWRK(IPE1+9).NE.0.OR.IWRK(IPE2+9).NE.0) THEN
              IERR=3
              GO TO 10065
            END IF
C
            IF (IG03.NE.0) THEN
              IPSN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPSN=IPWU
            END IF
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
            IWRK(IPSN+2)=0
C
            IF (IG03.NE.0) THEN
              IPPN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPPN=IPWU
            END IF
C
            IWRK(IPPN  )=IPSN
            IWRK(IPPN+1)=IPSN
            IWRK(IPPN+2)=IPPL
            IPPL=IPPN
C
            IWRK(IPE1+9)=IPPN
            IWRK(IPE2+9)=IPPN
C
C Check for a left intersection.
C
          ELSE IF ((IWRK(IPE1+4).EQ.0.AND.IWRK(IPE1+5).EQ.0.AND.IWRK(IPE
     +2+4).EQ.1.AND.IWRK(IPE2+5).EQ.0).OR.(IWRK(IPE1+4).EQ.1.AND.IWRK(IP
     +E1+5).EQ.0.AND.IWRK(IPE2+4).EQ.0.AND.IWRK(IPE2+5).EQ.0)) THEN
C
C Process a left intersection.
C
            IF (IWRK(IPE2+9).EQ.0) THEN
              IERR=4
              GO TO 10065
            END IF
C
            IF (IG03.NE.0) THEN
              IPSN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPSN=IPWU
            END IF
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
C
            IWRK(IPSN+2)=IWRK(IWRK(IPE2+9))
            IWRK(IWRK(IPE2+9))=IPSN
C
C Check for a right intersection.
C
          ELSE IF ((IWRK(IPE1+4).EQ.0.AND.IWRK(IPE1+5).EQ.1.AND.IWRK(IPE
     +2+4).EQ.1.AND.IWRK(IPE2+5).EQ.1).OR.(IWRK(IPE1+4).EQ.1.AND.IWRK(IP
     +E1+5).EQ.1.AND.IWRK(IPE2+4).EQ.0.AND.IWRK(IPE2+5).EQ.1)) THEN
C
C Process a right intersection.
C
            IF (IWRK(IPE1+9).EQ.0) THEN
              IERR=5
              GO TO 10065
            END IF
C
            IF (IG03.NE.0) THEN
              IPSN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPSN=IPWU
            END IF
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
C
            IWRK(IPSN+2)=0
            IWRK(IWRK(IWRK(IPE1+9)+1)+2)=IPSN
            IWRK(IWRK(IPE1+9)+1)=IPSN
C
C Check for a local maximum.
C
          ELSE IF ((IWRK(IPE1+4).EQ.1.AND.IWRK(IPE1+5).EQ.0.AND.IWRK(IPE
     +2+4).EQ.0.AND.IWRK(IPE2+5).EQ.1).OR.(IWRK(IPE1+4).EQ.0.AND.IWRK(IP
     +E1+5).EQ.0.AND.IWRK(IPE2+4).EQ.1.AND.IWRK(IPE2+5).EQ.1)) THEN
C
C Process a local maximum.
C
            IF (IWRK(IPE1+9).EQ.0.OR.IWRK(IPE2+9).EQ.0) THEN
              IERR=6
              GO TO 10065
            END IF
C
            IPP1=IWRK(IPE1+9)
            IPP2=IWRK(IPE2+9)
C
            IWRK(IPE1+9)=0
            IWRK(IPE2+9)=0
C
            IF (IG03.NE.0) THEN
              IPSN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPSN=IPWU
            END IF
C
            RWRK(IPSN  )=XINT
            RWRK(IPSN+1)=YINT
C
            IWRK(IPSN+2)=IWRK(IPP1)
            IWRK(IPP1)=IPSN
C
C See if the meeting edges are contributing to the same polygon.
C
            IF (IPP1.NE.IPP2) THEN
C
C They aren't.  Append the subsidiary nodes of one polygon to the other.
C
              IWRK(IWRK(IPP2+1)+2)=IPSN
              IWRK(IPP2+1)=IWRK(IPP1+1)
C
C Remove from the polygon list the polygon whose subsidiary nodes have
C become part of the other polygon and put its principal node on the
C garbage list for 3-word nodes, so that it can be re-used.
C
              IF (IPPL.EQ.IPP1) THEN
                IPPL=IWRK(IPP1+2)
              ELSE
                ISPL=IPPL
10079           CONTINUE
                  IF (IWRK(ISPL+2).EQ.IPP1) THEN
                    IWRK(ISPL+2)=IWRK(IPP1+2)
                    GO TO 10080
                  END IF
                  ISPL=IWRK(ISPL+2)
                GO TO 10079
10080           CONTINUE
              END IF
C
              IWRK(IPP1)=IG03
              IG03=IPP1
C
C Any AET node that referenced IPP1 must now reference IPP2 instead.
C
              IDUM=IAET
C
10081         CONTINUE
              IF (.NOT.(IDUM.NE.0)) GO TO 10082
                IF (IWRK(IDUM+9).EQ.IPP1) IWRK(IDUM+9)=IPP2
                IDUM=IWRK(IDUM+6)
              GO TO 10081
10082         CONTINUE
C
            END IF
C
          END IF
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
C Exchange the polygon pointers of edges 1 and 2.
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
          IF (IINT.EQ.0) GO TO 10083
C
C End of loop on points of intersection.
C
        GO TO 10062
10083   CONTINUE
C
C End of internal procedure to process a list of intersections.
C
      GO TO (10037,10053) , L10038
C
C The following internal procedure processes an edge in the AET that is
C terminating at the top of the current scanbeam.  The variable ITMP
C points to the edge that is to be processed.  If the edge is removed
C from the AET (which can happen), the procedure must adjust the value
C of ITMP so that the next-node pointer in the AET node that ITMP
C points at properly specifies the next AET node to be examined.
C
10040 CONTINUE
C
C Find the index, in the user's arrays, of the end point of the
C successor edge.
C
        INNP=ABS(IWRK(ITMP+8))+SIGN(1,IWRK(ITMP+8))
C
C Extract the X and Y coordinates of the end point of the successor
C edge.
C
        IF (IWRK(ITMP+4).EQ.0) THEN
          IF (INNP.LT.1) THEN
            INNP=INNP+LCCP
          ELSE IF (INNP.GT.LCCP) THEN
            INNP=INNP-LCCP
          END IF
          XCNP=XCCP(INNP)
          YCNP=YCCP(INNP)
        ELSE
          IF (INNP.LT.1) THEN
            INNP=INNP+LCSP
          ELSE IF (INNP.GT.LCSP) THEN
            INNP=INNP-LCSP
          END IF
          XCNP=XCSP(INNP)
          YCNP=YCSP(INNP)
        END IF
C
C Check the vertical position of the end point of the successor edge.
C
        IF (YCNP.GE.YTOS) THEN
C
C The end point of the successor edge is above the top of the scanbeam.
C
C Check whether the edge is contributing to a polygon.
C
          IF (IWRK(ITMP+9).NE.0) THEN
C
C The edge is contributing to a polygon.  Form a subsidiary polygon
C node to add to that polygon.
C
            IF (IG03.NE.0) THEN
              IPSN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPSN=IPWU
            END IF
C
            RWRK(IPSN  )=RWRK(ITMP)
            RWRK(IPSN+1)=YTOS
C
C Add the end point of the current edge to either the left end or the
C right end of the polygon to which the edge is contributing, whichever
C is appropriate.
C
            IF (IWRK(ITMP+5).EQ.0) THEN
              IWRK(IPSN+2)=IWRK(IWRK(ITMP+9))
              IWRK(IWRK(ITMP+9))=IPSN
            ELSE
              IWRK(IPSN+2)=0
              IWRK(IWRK(IWRK(ITMP+9)+1)+2)=IPSN
              IWRK(IWRK(ITMP+9)+1)=IPSN
            END IF
C
          END IF
C
C Update the node to represent its successor edge.
C
          RWRK(ITMP+1)=XCNP
          RWRK(ITMP+2)=YCNP
C
          IF (YCNP.NE.YTOS) THEN
            RWRK(ITMP+3)=(XCNP-RWRK(ITMP))/(YCNP-YTOS)
          ELSE
            RWRK(ITMP+3)=SIGN(RBIG,XCNP-RWRK(ITMP))
          END IF
C
          IWRK(ITMP+8)=SIGN(INNP,IWRK(ITMP+8))
C
        ELSE
C
C The end point of the successor edge is below the top of the scanbeam.
C We have arrived at a local maximum, so handle that case.
C
          IF (IWRK(ITMP+6).EQ.0) THEN
            IERR=7
            GO TO 10065
          END IF
C
          IPP1=IWRK(ITMP+9)
          IPP2=IWRK(IWRK(ITMP+6)+9)
C
          IF (IPP1.NE.0.OR.IPP2.NE.0) THEN
C
            IF (IPP1.EQ.0.OR.IPP2.EQ.0) THEN
              IERR=8
              GO TO 10065
            END IF
C
            IF (IG03.NE.0) THEN
              IPSN=IG03
              IG03=IWRK(IG03)
            ELSE
              IPWU=IPWU-3
              IF (IPWU.LE.IPWL) THEN
                GO TO 10009
              END IF
              IPSN=IPWU
            END IF
C
            RWRK(IPSN  )=RWRK(ITMP)
            RWRK(IPSN+1)=YTOS
C
            IF (IWRK(ITMP+5).EQ.0) THEN
              IWRK(IPSN+2)=IWRK(IPP1)
              IWRK(IPP1)=IPSN
            ELSE
              IWRK(IPSN+2)=0
              IWRK(IWRK(IPP1+1)+2)=IPSN
              IWRK(IPP1+1)=IPSN
            END IF
C
C See if the meeting edges are contributing to the same polygon.
C
            IF (IPP1.NE.IPP2) THEN
C
C They aren't.  Append the subsidiary nodes of one polygon to the other.
C
              IF (IWRK(ITMP+5).EQ.0) THEN
                IWRK(IWRK(IPP2+1)+2)=IWRK(IPP1)
                IWRK(IPP2+1)=IWRK(IPP1+1)
              ELSE
                IWRK(IWRK(IPP1+1)+2)=IWRK(IPP2)
                IWRK(IPP2)=IWRK(IPP1)
              END IF
C
C Remove from the polygon list the polygon whose subsidiary nodes have
C become part of the other polygon and put its principal node on the
C garbage list for 3-word nodes, so that it can be re-used.
C
              IF (IPPL.EQ.IPP1) THEN
                IPPL=IWRK(IPP1+2)
              ELSE
                ISPL=IPPL
10088           CONTINUE
                  IF (IWRK(ISPL+2).EQ.IPP1) THEN
                    IWRK(ISPL+2)=IWRK(IPP1+2)
                    GO TO 10089
                  END IF
                  ISPL=IWRK(ISPL+2)
                GO TO 10088
10089           CONTINUE
              END IF
C
              IWRK(IPP1)=IG03
              IG03=IPP1
C
C Any AET node that referenced IPP1 must now reference IPP2 instead.
C
              IDUM=IAET
C
10090         CONTINUE
              IF (.NOT.(IDUM.NE.0)) GO TO 10091
                IF (IWRK(IDUM+9).EQ.IPP1) IWRK(IDUM+9)=IPP2
                IDUM=IWRK(IDUM+6)
              GO TO 10090
10091         CONTINUE
C
            END IF
C
          END IF
C
C Delete from the AET the edge ITMP and the edge that follows it.  The
C nodes go back on the garbage list for 10-word nodes.
C
          ITM1=IWRK(ITMP+7)
          ITM2=IWRK(IWRK(ITMP+6)+6)
C
          IF (ITM1.EQ.0) THEN
            IAET=ITM2
          ELSE
            IWRK(ITM1+6)=ITM2
          END IF
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
        END IF
C
      GO TO (10039,10056) , L10040
C
C Error exits.
C
10002 CONTINUE
        IERR=1
        RETURN
C
10004 CONTINUE
        IERR=2
        RETURN
C
10009 CONTINUE
        IERR=3
        RETURN
C
10065 CONTINUE
        IERR=3+IERR
        RETURN
C
      END
