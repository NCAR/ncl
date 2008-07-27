C
C $Id: tdmtri.f,v 1.4 2008-07-27 00:17:33 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
