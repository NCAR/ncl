C
C $Id: tdmtri.f,v 1.3 2000-08-22 15:07:11 haley Exp $
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
      SUBROUTINE TDMTRI (IMRK,UMRK,VMRK,WMRK,SMRK,RTRI,MTRI,NTRI,IRST,
     +                                  UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
C
        DIMENSION RTRI(10,MTRI)
C
C This routine is called to put a marker of type ABS(IMRK) at the point
C with coordinates (UMRK,VMRK,WMRK) and with radius SMRK.  This is done
C by adding triangles to the triangle list in the array RTRI.  MTRI is
C the maximum number of triangles that can be put in the list and NTRI
C is the number of triangles currently in the list.  IRST is the index
C of the rendering style to be used for the marker.
C
C IMRK may have an absolute value from 1 to 5, inclusive, to select a
C tetrahedron, an octahedron, a cube, an icosahedron, or an elaborated
C icosahedron (effectively, a sphere), respectively.  If IMRK is less
C than zero, the mark is not clipped at the faces of the box defined
C by the last six arguments; otherwise, it is.
C
        IF (IMRK.LT.0) THEN
          CALL TDMRKA (-IMRK,UMRK,VMRK,WMRK,SMRK,RTRI,MTRI,NTRI,IRST)
        ELSE
          CALL TDMRKB ( IMRK,UMRK,VMRK,WMRK,SMRK,RTRI,MTRI,NTRI,IRST,
     +                                 UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
        END IF
C
C Done.
C
        RETURN
C
      END
