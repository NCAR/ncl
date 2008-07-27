C
C $Id: tdctab.f,v 1.4 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
