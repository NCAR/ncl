C
C $Id: icaedg.f,v 1.3 2004-03-19 22:52:00 kennison Exp $
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
      FUNCTION ICAEDG (IPP1,IPP2,IEDG,LOEN,IPPE,MPPE,NPPE,RPNT)
C
      DIMENSION IEDG(LOEN,MPPE),IPPE(2,MPPE),RPNT(*)
C
C This function, given the base indices, in the point list, of the two
C points defining an edge, searches the edge list for an edge matching
C it.  If such an edge exists, its index is returned; if not, such an
C edge is created and its index is returned.  The search is effected
C using a quicksort technique, the pointers for which are kept in the
C array IPPE.
C
C If there are any edges in the edge list at all, ...
C
      IF (NPPE.NE.0) THEN
C
C search it.  First, order the pointers to the points in a consistent
C manner and then find the X, Y, and Z coordinates of the edge's
C midpoint, which we use to determine the order of the edges in the
C list.  (Using the values of ITM1 and ITM2 results in very bad
C behavior by the quicksort.)
C
        ITM1=MIN(IPP1,IPP2)
        ITM2=MAX(IPP1,IPP2)
C
        XNEW=(RPNT(ITM1+1)+RPNT(ITM2+1))/2.
        YNEW=(RPNT(ITM1+2)+RPNT(ITM2+2))/2.
        ZNEW=(RPNT(ITM1+3)+RPNT(ITM2+3))/2.
C
C Initialize a search index to point to the first element in the sort
C list.
C
        ITMP=1
C
C Loop.  If the search index now points at the edge we want, return
C its index.
C
  101   IF (ITM1.EQ.IEDG(1,ITMP).AND.ITM2.EQ.IEDG(2,ITMP)) THEN
C
          ICAEDG=ITMP
C
          RETURN
C
        END IF
C
C Find the X, Y, and Z coordinates of this edge's midpoint for
C comparison with the one we seek.
C
        XTMP=(RPNT(IEDG(1,ITMP)+1)+RPNT(IEDG(2,ITMP)+1))/2.
        YTMP=(RPNT(IEDG(1,ITMP)+2)+RPNT(IEDG(2,ITMP)+2))/2.
        ZTMP=(RPNT(IEDG(1,ITMP)+3)+RPNT(IEDG(2,ITMP)+3))/2.
C
C If the edge we want would precede the one pointed at by the search
C index, reset the search index to look at lesser elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
        IF (XNEW.LT.XTMP.OR.(XNEW.EQ.XTMP.AND.YNEW.LT.YTMP).OR.(XNEW.EQ.
     +XTMP.AND.YNEW.EQ.YTMP.AND.ZNEW.LT.ZTMP)) THEN
C
          IF (IPPE(1,ITMP).NE.0) THEN
            ITMP=IPPE(1,ITMP)
            GO TO 101
          END IF
C
          IPPE(1,ITMP)=NPPE+1
C
C If the edge we want would follow the one pointed at by the search
C index, reset the search index to look at greater elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
        ELSE IF (XNEW.GT.XTMP.OR.(XNEW.EQ.XTMP.AND.YNEW.GT.YTMP).OR.(XNE
     +W.EQ.XTMP.AND.YNEW.EQ.YTMP.AND.ZNEW.GT.ZTMP)) THEN
C
          IF (IPPE(2,ITMP).NE.0) THEN
            ITMP=IPPE(2,ITMP)
            GO TO 101
          END IF
C
          IPPE(2,ITMP)=NPPE+1
C
        ELSE
C
          CALL SETER ('ICAEDG - LOGIC ERROR',1,1)
          ICAEDG=-1
          RETURN
C
        END IF
C
      END IF
C
C Create a new edge in the edge list (if there's room, of course), and
C return its index to the caller.
C
      IF (NPPE.GE.MPPE) THEN
C
        CALL SETER ('ICAEDG - EDGE ARRAY IS TOO SMALL',1,1)
        ICAEDG=-1
        RETURN
C
      ELSE
C
        NPPE=NPPE+1
C
        IPPE(1,NPPE)=0
        IPPE(2,NPPE)=0
C
        IEDG(1,NPPE)=MIN(IPP1,IPP2)
        IEDG(2,NPPE)=MAX(IPP1,IPP2)
        IEDG(3,NPPE)=-1
        IEDG(4,NPPE)=-1
        IEDG(5,NPPE)=0
C
        ICAEDG=NPPE
C
      END IF
C
      RETURN
C
      END
