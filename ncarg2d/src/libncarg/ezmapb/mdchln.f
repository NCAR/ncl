C
C $Id: mdchln.f,v 1.1 2001-08-16 23:10:45 kennison Exp $
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
      SUBROUTINE MDCHLN (IFLG,ILTY,IOAL,IOAR,NPTS,PNTS)
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
C of the points.  MDCHLN is meant to be replaced by an LLU user; it may
C set line width and color and it may change the values of IOAL and
C IOAR; if it sets NPTS to zero, the line is deleted.
C
        RETURN
C
      END
