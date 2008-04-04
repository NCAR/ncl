C
C	$Id: wmvmap.f,v 1.2 2008-04-04 21:02:58 kennison Exp $
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
      SUBROUTINE WMVECTMAP(X,Y,U,V)
C
C  This subroutine plots wind vectors over a map with the origin 
C  of the vector at lat/lon coordinate (X,Y).  U and V are the 
C  components of the wind vector.
C
      include 'wmcomn.h'
C
      PARAMETER (R2D=57.2957795131,D2R=0.0174532925)
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL WMBLDA
C
C  Adjust the vector direction depending on its location
C  on a map.
C
      CALL MAPTRN(X,Y,X0,Y0)
      ANGR = ATAN2(U,V)
      CALL MAPTRN(X+0.1*COS(ANGR),Y+0.1/COS(D2R*X)*SIN(ANGR),XA,YA)
      ANGTMP = ATAN2(YA-Y0,XA-X0)
      VLEN = SQRT(U*U+V*V)
      UTMP = VLEN*COS(ANGTMP)
      VTMP = VLEN*SIN(ANGTMP)
      CALL WMVECT(X0,Y0,UTMP,VTMP)
C
      RETURN
      END
