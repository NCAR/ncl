C
C $Id: tdctab.f,v 1.2 2000-07-12 16:26:31 haley Exp $
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
      SUBROUTINE TDCTAB (RTRI,MTRI,NTRI,UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
C
        DIMENSION RTRI(10,MTRI)
C
C This routine is called to clip the last triangle in the triangle list
C against the box defined by UMIN, VMIN, WMIN, UMAX, VMAX, and WMAX.
C
C First, save the number of triangles that we had on entry.
C
        NTRE=NTRI
C
C Clip the final triangle in the list against the plane U=UMIN.
C
        ITRI=NTRE
C
        CALL TDCTAP (RTRI,MTRI,NTRI,ITRI,-1., 0., 0., UMIN)
C
C Clip any resulting triangles against the planes U=UMAX, V=VMIN,
C V=VMAX, W=WMIN, and W=WMAX.  Note: DO loops cannot be used here
C because it is possible that TDCTAP may modify ITRI.
C
        ITRI=NTRE
C
  101   IF (ITRI.LE.NTRI) THEN
          CALL TDCTAP (RTRI,MTRI,NTRI,ITRI, 1., 0., 0.,-UMAX)
          ITRI=ITRI+1
          GO TO 101
        END IF
C
        ITRI=NTRE
C
  102   IF (ITRI.LE.NTRI) THEN
          CALL TDCTAP (RTRI,MTRI,NTRI,ITRI, 0.,-1., 0., VMIN)
          ITRI=ITRI+1
          GO TO 102
        END IF
C
        ITRI=NTRE
C
  103   IF (ITRI.LE.NTRI) THEN
          CALL TDCTAP (RTRI,MTRI,NTRI,ITRI, 0., 1., 0.,-VMAX)
          ITRI=ITRI+1
          GO TO 103
        END IF
C
        ITRI=NTRE
C
  104   IF (ITRI.LE.NTRI) THEN
          CALL TDCTAP (RTRI,MTRI,NTRI,ITRI, 0., 0.,-1., WMIN)
          ITRI=ITRI+1
          GO TO 104
        END IF
C
        ITRI=NTRE
C
  105   IF (ITRI.LE.NTRI) THEN
          CALL TDCTAP (RTRI,MTRI,NTRI,ITRI, 0., 0., 1.,-WMAX)
          ITRI=ITRI+1
          GO TO 105
        END IF
C
C Done.
C
        RETURN
C
      END
