C
C $Id: tdmrkb.f,v 1.4 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDMRKB (IMRK,UMRK,VMRK,WMRK,SMRK,RTRI,MTRI,NTRI,IRST,
     +                                  UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
C
        DIMENSION RTRI(10,MTRI)
C
C This routine is called to put a marker of type IMRK at the point with
C coordinates (UMRK,VMRK,WMRK) and with radius SMRK.  This is done by
C adding triangles to the triangle list in the array RTRI.  MTRI is the
C maximum number of triangles that can be put in the list and NTRI is
C the number of triangles currently in the list.  IRST is the index of
C the rendering style to be used for the marker.
C
C IMRK may have a value from 1 to 5, inclusive, to select a tetrahedron,
C an octahedron, a cube, an icosahedron, or an elaborated icosahedron
C (effectively, a sphere), respectively.
C
C This version of the routine clips triangles that extend outside the
C box defined by the last six arguments.
C
C Declare the labelled common block containing the definitions of the
C triangles making up the various markers.
C
        COMMON /TDCOM5/ TMK1(36),TMK2(72),TMK3(108),TMK4(180),TMK5(720)
        SAVE   /TDCOM5/
C
C Get the real equivalent of the rendering style index.
C
        RRST=REAL(IRST)
C
C Depending on what kind of mark is requested, construct and put into
C the triangle array the proper set of triangles to create that mark.
C
        IF (IMRK.EQ.1) THEN
C
C A tetrahedron.
C
          DO 101 I=1,4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=UMRK+SMRK*TMK1(9*I-8)
              RTRI(2,NTRI)=VMRK+SMRK*TMK1(9*I-7)
              RTRI(3,NTRI)=WMRK+SMRK*TMK1(9*I-6)
              RTRI(4,NTRI)=UMRK+SMRK*TMK1(9*I-5)
              RTRI(5,NTRI)=VMRK+SMRK*TMK1(9*I-4)
              RTRI(6,NTRI)=WMRK+SMRK*TMK1(9*I-3)
              RTRI(7,NTRI)=UMRK+SMRK*TMK1(9*I-2)
              RTRI(8,NTRI)=VMRK+SMRK*TMK1(9*I-1)
              RTRI(9,NTRI)=WMRK+SMRK*TMK1(9*I  )
              RTRI(10,NTRI)=RRST
              CALL TDCTAB (RTRI,MTRI,NTRI,UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
            END IF
  101     CONTINUE
C
        ELSE IF (IMRK.EQ.2) THEN
C
C An octahedron.
C
          DO 102 I=1,8
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=UMRK+SMRK*TMK2(9*I-8)
              RTRI(2,NTRI)=VMRK+SMRK*TMK2(9*I-7)
              RTRI(3,NTRI)=WMRK+SMRK*TMK2(9*I-6)
              RTRI(4,NTRI)=UMRK+SMRK*TMK2(9*I-5)
              RTRI(5,NTRI)=VMRK+SMRK*TMK2(9*I-4)
              RTRI(6,NTRI)=WMRK+SMRK*TMK2(9*I-3)
              RTRI(7,NTRI)=UMRK+SMRK*TMK2(9*I-2)
              RTRI(8,NTRI)=VMRK+SMRK*TMK2(9*I-1)
              RTRI(9,NTRI)=WMRK+SMRK*TMK2(9*I  )
              RTRI(10,NTRI)=RRST
              CALL TDCTAB (RTRI,MTRI,NTRI,UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
            END IF
  102     CONTINUE
C
        ELSE IF (IMRK.EQ.3) THEN
C
C A twelve-triangle cube.
C
          DO 103 I=1,12
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=UMRK+SMRK*TMK3(9*I-8)
              RTRI(2,NTRI)=VMRK+SMRK*TMK3(9*I-7)
              RTRI(3,NTRI)=WMRK+SMRK*TMK3(9*I-6)
              RTRI(4,NTRI)=UMRK+SMRK*TMK3(9*I-5)
              RTRI(5,NTRI)=VMRK+SMRK*TMK3(9*I-4)
              RTRI(6,NTRI)=WMRK+SMRK*TMK3(9*I-3)
              RTRI(7,NTRI)=UMRK+SMRK*TMK3(9*I-2)
              RTRI(8,NTRI)=VMRK+SMRK*TMK3(9*I-1)
              RTRI(9,NTRI)=WMRK+SMRK*TMK3(9*I  )
              RTRI(10,NTRI)=RRST
              CALL TDCTAB (RTRI,MTRI,NTRI,UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
            END IF
  103     CONTINUE
C
        ELSE IF (IMRK.EQ.4) THEN
C
C An icosahedron.
C
          DO 104 I=1,20
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=UMRK+SMRK*TMK4(9*I-8)
              RTRI(2,NTRI)=VMRK+SMRK*TMK4(9*I-7)
              RTRI(3,NTRI)=WMRK+SMRK*TMK4(9*I-6)
              RTRI(4,NTRI)=UMRK+SMRK*TMK4(9*I-5)
              RTRI(5,NTRI)=VMRK+SMRK*TMK4(9*I-4)
              RTRI(6,NTRI)=WMRK+SMRK*TMK4(9*I-3)
              RTRI(7,NTRI)=UMRK+SMRK*TMK4(9*I-2)
              RTRI(8,NTRI)=VMRK+SMRK*TMK4(9*I-1)
              RTRI(9,NTRI)=WMRK+SMRK*TMK4(9*I  )
              RTRI(10,NTRI)=RRST
              CALL TDCTAB (RTRI,MTRI,NTRI,UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
            END IF
  104     CONTINUE
C
        ELSE IF (IMRK.EQ.5) THEN
C
C An elaborated icosahedron.
C
          DO 105 I=1,80
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=UMRK+SMRK*TMK5(9*I-8)
              RTRI(2,NTRI)=VMRK+SMRK*TMK5(9*I-7)
              RTRI(3,NTRI)=WMRK+SMRK*TMK5(9*I-6)
              RTRI(4,NTRI)=UMRK+SMRK*TMK5(9*I-5)
              RTRI(5,NTRI)=VMRK+SMRK*TMK5(9*I-4)
              RTRI(6,NTRI)=WMRK+SMRK*TMK5(9*I-3)
              RTRI(7,NTRI)=UMRK+SMRK*TMK5(9*I-2)
              RTRI(8,NTRI)=VMRK+SMRK*TMK5(9*I-1)
              RTRI(9,NTRI)=WMRK+SMRK*TMK5(9*I  )
              RTRI(10,NTRI)=RRST
              CALL TDCTAB (RTRI,MTRI,NTRI,UMIN,VMIN,WMIN,UMAX,VMAX,WMAX)
            END IF
  105     CONTINUE
C
        END IF
C
C Done.
C
        RETURN
C
      END
