C
C $Id: tdgtrs.f,v 1.3 2000-07-12 16:26:33 haley Exp $
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
      SUBROUTINE TDGTRS (IRST,IA01,IA02,IA03,IA04,IA05,IA06,IA07,RA08,
     +                                                      RA09,RA10)
C
C This routine allows the caller to retrieve the values defining a
C particular rendering style.  The arguments are as follows (all are
C output, those  beginning with an "I" are of type INTEGER, and those
C beginning with an "R" are of type "REAL"):
C
C   IRST is the index of the rendering style for which values are to
C   be retrieved and should be in the range from 1 to 64, inclusive.
C
C   IA01 and IA02 are color indices specifying a range of colors to be
C   used for the "bottom" side of a surface (where function values are
C   less than the value on the surface).  If IA01 is negative, filling
C   of triangles seen from the "bottom" is turned off.  If IA01 is zero
C   or greater, but IA02 is less than or equal to it, the color with
C   index IA01 is used.  If IA01 is zero or greater and IA02 is greater
C   than IA01, then a range of color indices is specified; colors near
C   the beginning of that range are used for triangles that are nearly
C   perpendicular to the line of sight, while colors near the end of
C   that range are used for triangles more nearly parallel to the line
C   of sight.
C
C   IA03 and IA04 are color indices specifying a range of colors to be
C   used for the "top" side of a surface (with function values greater
C   than the value in the surface).  If IA03 is negative, filling
C   of triangles seen from the "top" is turned off.  If IA03 is zero
C   or greater, but IA04 is less than or equal to it, the color with
C   index IA03 is used.  If IA03 is zero or greater and IA04 is greater
C   than IA03, then a range of color indices is specified; colors near
C   the beginning of that range are used for triangles that are nearly
C   perpendicular to the line of sight, while colors near the end of
C   that range are used for triangles more nearly parallel to the line
C   of sight.
C
C   IA05 is the color index specifying a color to be used for lines
C   drawn on the "bottom" side of a surface.  If IA05 is negative, the
C   drawing of these lines is turned off.
C
C   IA06 is the color index specifying a color to be used for lines
C   drawn on the "top" side of a surface.  If IA06 is negative, the
C   drawing of these lines is turned off.
C
C   IA07 is a flag, which, if set non-zero, turns on the drawing of
C   the boundaries of the individual triangles into which surfaces
C   have been decomposed.
C
C   RA08, RA09, and RA10 are the distances between slices in the U, V,
C   and W directions, respectively.
C
C Declare a necessary TDPACK common block.
C
        COMMON /TDCOM3/ IFC1(64),IFC2(64),IFC3(64),IFC4(64),ILC1(64)
        COMMON /TDCOM3/ ILC2(64),ILTD(64),USTP(64),VSTP(64),WSTP(64)
        SAVE   /TDCOM3/
C
C Copy out the values from the common block.
C
        INDX=MAX(1,MIN(64,IRST))
        IA01=IFC1(INDX)
        IA02=IFC2(INDX)
        IA03=IFC3(INDX)
        IA04=IFC4(INDX)
        IA05=ILC1(INDX)
        IA06=ILC2(INDX)
        IA07=ILTD(INDX)
        RA08=USTP(INDX)
        RA09=VSTP(INDX)
        RA10=WSTP(INDX)
C
C Done.
C
        RETURN
C
      END
