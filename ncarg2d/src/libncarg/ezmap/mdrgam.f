C
C $Id: mdrgam.f,v 1.7 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDRGAM (IAMA,XCRA,YCRA,NCRA,IGID,IAIL,IAIR)
C
        DIMENSION IAMA(*),XCRA(NCRA),YCRA(NCRA)
C
C The object of this routine is to break a polygon being sent to an area
C map into pieces that lie along the edges of the unit square and pieces
C that lie strictly within the interior of the unit square.  Only the
C latter are actually sent to the area map.
C
C NPCS counts the number of pieces of the polygon sent to the area map
C so far.
C
        NPCS=0
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
C It's on the boundary.  If an interior piece was being traced, send it
C to the area map, bump the piece count, and resume searching for the
C beginning of another interior piece.
C
            IF (IBEG.NE.0) THEN
              CALL AREDAM (IAMA,XCRA(IBEG),YCRA(IBEG),ICRA-IBEG,
     +                                                   IGID,IAIL,IAIR)
              NPCS=NPCS+1
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
C interior piece was in progress, send it to the area map and bump the
C piece count.
C
        IF (IBEG.NE.0) THEN
          CALL AREDAM (IAMA,XCRA(IBEG),YCRA(IBEG),ICRA-IBEG+1,
     +                                                   IGID,IAIL,IAIR)
          NPCS=NPCS+1
        END IF
C
C If nothing was sent to the area map, all segments of the polygon were
C on the edge of the unit square.  In that special case, we send the
C entire polygon to the area map as an enclosing boundary for what is
C to follow.  We must force the smaller of the area ID's to zero.
C
        IF (NPCS.EQ.0) THEN
          CALL MDRGIP (XCRA,YCRA,NCRA)
          IF (IAIL.LT.IAIR) THEN
            CALL AREDAM (IAMA,XCRA,YCRA,-NCRA,IGID,0,IAIR)
          ELSE
            CALL AREDAM (IAMA,XCRA,YCRA,-NCRA,IGID,IAIL,0)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
