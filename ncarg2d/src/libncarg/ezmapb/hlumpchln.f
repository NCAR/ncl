C
C $Id: hlumpchln.f,v 1.6 1998-05-24 00:40:43 kennison Exp $
C
      SUBROUTINE HLUMPCHLN (IFLG,ILTY,IOAL,IOAR,NPTS,PNTS)
C
        DIMENSION PNTS(*)
C
C This routine is called by MPLNAM, MPLNDM, and MPLNDR before and after
C processing each line defined by a dataset.  IFLG is positive if a line
C is about to be processed, negative if a line was just processed; its
C absolute value is 1 if the call comes from MPLNAM, 2 if the call comes
C from MPLNDM, and 3 if the call comes from MPLNDR.  ILTY is the type of
C the line, IOAL is the identifier of the area to its left, and IOAR is
C the identifier of the area to its right.  NPTS is the number of points
C defining the line, and the array PNTS contains the lat/lon coordinates
C of the points.  HLUMPCHLN is meant to be replaced by an HLU developer;
C it may set line width and color and it may change the values of IOAL
C and IOAR; if it sets NPTS to zero, the line is deleted.
C
C Call MPCHLN, which is replaceable by an LLU user.
C
        CALL MPCHLN (IFLG,ILTY,IOAL,IOAR,NPTS,PNTS)
C
        RETURN
C
      END
