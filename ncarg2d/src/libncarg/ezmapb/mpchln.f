C
C $Id: mpchln.f,v 1.10 2008-07-27 00:17:07 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MPCHLN (IFLG,ILTY,IOAL,IOAR,NPTS,PNTS)
C
        INTEGER IFLG,ILTY,IOAL,IOAR,NPTS
        REAL    PNTS(*)
C
C This routine is called by MDLNAM, MDLNDM, and MDLNDR before and after
C processing each line defined by a dataset.  IFLG is positive if a line
C is about to be processed, negative if a line was just processed; its
C absolute value is 1 if the call comes from MDLNAM, 2 if the call comes
C from MDLNDM, and 3 if the call comes from MDLNDR.  ILTY is the type of
C the line, IOAL is the identifier of the area to its left, and IOAR is
C the identifier of the area to its right.  NPTS is the number of points
C defining the line, and the array PNTS contains the lat/lon coordinates
C of the points.  MPCHLN is meant to be replaced by an LLU user; it may
C set line width and color and it may change the values of IOAL and
C IOAR; if it sets NPTS to zero, the line is deleted.
C
        CALL MDCHLN (IFLG,ILTY,IOAL,IOAR,NPTS,PNTS)
C
        RETURN
C
      END
