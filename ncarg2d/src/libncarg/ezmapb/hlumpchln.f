C
C $Id: hlumpchln.f,v 1.7 2000-07-12 16:23:41 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
