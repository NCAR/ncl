C
C $Id: mdrgdr.f,v 1.8 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGDR (XCRA,YCRA,NCRA,ITYP)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA)
C
C The object of this routine is to break a polygon being drawn into
C pieces that lie along the edges of the unit square and pieces that lie
C strictly within the interior of the unit square.  Only the latter are
C actually drawn.
C
C IBEG is the index of the first point of the current interior piece of
C the polygon being traced, zero if no interior piece is being traced.
C
        IBEG=0
C
C ICRA is the index of the current polygon point being examined.
C
        ICRA=1
C
C Loop through the points of the polygon.
C
  101   IF (ICRA.LT.NCRA) THEN
C
C Advance ICRA to point to the next point.
C
          ICRA=ICRA+1
C
C Test whether the segment from point ICRA-1 to point ICRA is on the
C boundary of the unit square or not.
C
          IF ((XCRA(ICRA-1).EQ.0..AND.XCRA(ICRA).EQ.0.).OR.
     +        (XCRA(ICRA-1).EQ.1..AND.XCRA(ICRA).EQ.1.).OR.
     +        (YCRA(ICRA-1).EQ.0..AND.YCRA(ICRA).EQ.0.).OR.
     +        (YCRA(ICRA-1).EQ.1..AND.YCRA(ICRA).EQ.1.)) THEN
C
C It's on the boundary.  If an interior piece was being traced, draw
C it and resume searching for the beginning of another interior piece.
C
            IF (IBEG.NE.0) THEN
              CALL MDRGDP (XCRA(IBEG),YCRA(IBEG),ICRA-IBEG,ITYP)
              IBEG=0
            END IF
C
          ELSE
C
C It's in the interior.  If no interior piece is currently being
C traced, set the pointer to begin tracing a new such piece.
C
            IF (IBEG.EQ.0) IBEG=ICRA-1
C
          END IF
C
C Loop back for the next point of the polygon.
C
          GO TO 101
C
        END IF
C
C All segments of the polygon have been examined.  If the trace of an
C interior piece was in progress, draw it.
C
        IF (IBEG.NE.0) THEN
          CALL MDRGDP (XCRA(IBEG),YCRA(IBEG),ICRA-IBEG+1,ITYP)
        END IF
C
C Done.
C
        RETURN
C
      END
