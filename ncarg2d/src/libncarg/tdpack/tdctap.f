C
C $Id: tdctap.f,v 1.3 2000-08-22 15:07:07 haley Exp $
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
      SUBROUTINE TDCTAP (RTRI,MTRI,NTRI,ITRI,A,B,C,D)
C
        DIMENSION RTRI(10,MTRI)
C
C This routine is called to clip triangle number ITRI in the triangle
C list against the plane A*U+B*V+C*W+D=0.  The portion of the triangle
C where A*U+B*V+C*W+D is greater than zero is the portion to be clipped.
C
C Declare required arithmetic statement functions.
C
        SIDE(U,V,W)=A*U+B*V+C*W+D
C
        DNOM(U1,V1,W1,U2,V2,W2)=A*(U2-U1)+B*(V2-V1)+C*(W2-W1)
C
C There are eight possible cases, depending on which side of the plane
C the points are on.
C
        IF (SIDE(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI)).LT.0.) THEN
C
          IF (SIDE(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI)).LT.0.) THEN
C
            IF (SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI)).LT.0.) THEN
C
C Point 1 in, point 2 in, point 3 in.
C
              RETURN
C
            ELSE
C
C Point 1 in, point 2 in, point 3 out.
C
              IF (NTRI.LT.MTRI) THEN
                JTRI=ITRI+1
                NTRI=NTRI+1
                IF (NTRI.NE.JTRI) THEN
                  RTRI(1,NTRI)=RTRI(1,JTRI)
                  RTRI(2,NTRI)=RTRI(2,JTRI)
                  RTRI(3,NTRI)=RTRI(3,JTRI)
                  RTRI(4,NTRI)=RTRI(4,JTRI)
                  RTRI(5,NTRI)=RTRI(5,JTRI)
                  RTRI(6,NTRI)=RTRI(6,JTRI)
                  RTRI(7,NTRI)=RTRI(7,JTRI)
                  RTRI(8,NTRI)=RTRI(8,JTRI)
                  RTRI(9,NTRI)=RTRI(9,JTRI)
                  RTRI(10,NTRI)=RTRI(10,JTRI)
                END IF
                T=-SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))/
     +             DNOM(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI),
     +                  RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))
                RTRI(1,JTRI)=RTRI(7,ITRI)+(RTRI(4,ITRI)-RTRI(7,ITRI))*T
                RTRI(2,JTRI)=RTRI(8,ITRI)+(RTRI(5,ITRI)-RTRI(8,ITRI))*T
                RTRI(3,JTRI)=RTRI(9,ITRI)+(RTRI(6,ITRI)-RTRI(9,ITRI))*T
                T=-SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))/
     +             DNOM(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI),
     +                  RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))
                RTRI(4,JTRI)=RTRI(7,ITRI)+(RTRI(1,ITRI)-RTRI(7,ITRI))*T
                RTRI(5,JTRI)=RTRI(8,ITRI)+(RTRI(2,ITRI)-RTRI(8,ITRI))*T
                RTRI(6,JTRI)=RTRI(9,ITRI)+(RTRI(3,ITRI)-RTRI(9,ITRI))*T
                RTRI(7,JTRI)=RTRI(1,ITRI)
                RTRI(8,JTRI)=RTRI(2,ITRI)
                RTRI(9,JTRI)=RTRI(3,ITRI)
                RTRI(10,JTRI)=RTRI(10,ITRI)
                RTRI(7,ITRI)=RTRI(1,JTRI)
                RTRI(8,ITRI)=RTRI(2,JTRI)
                RTRI(9,ITRI)=RTRI(3,JTRI)
                ITRI=JTRI
              END IF
C
            END IF
C
          ELSE
C
            IF (SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI)).LT.0.) THEN
C
C Point 1 in, point 2 out, point 3 in.
C
              IF (NTRI.LT.MTRI) THEN
                JTRI=ITRI+1
                NTRI=NTRI+1
                IF (NTRI.NE.JTRI) THEN
                  RTRI(1,NTRI)=RTRI(1,JTRI)
                  RTRI(2,NTRI)=RTRI(2,JTRI)
                  RTRI(3,NTRI)=RTRI(3,JTRI)
                  RTRI(4,NTRI)=RTRI(4,JTRI)
                  RTRI(5,NTRI)=RTRI(5,JTRI)
                  RTRI(6,NTRI)=RTRI(6,JTRI)
                  RTRI(7,NTRI)=RTRI(7,JTRI)
                  RTRI(8,NTRI)=RTRI(8,JTRI)
                  RTRI(9,NTRI)=RTRI(9,JTRI)
                  RTRI(10,NTRI)=RTRI(10,JTRI)
                END IF
                T=-SIDE(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))/
     +             DNOM(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI),
     +                  RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))
                RTRI(1,JTRI)=RTRI(4,ITRI)+(RTRI(1,ITRI)-RTRI(4,ITRI))*T
                RTRI(2,JTRI)=RTRI(5,ITRI)+(RTRI(2,ITRI)-RTRI(5,ITRI))*T
                RTRI(3,JTRI)=RTRI(6,ITRI)+(RTRI(3,ITRI)-RTRI(6,ITRI))*T
                T=-SIDE(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))/
     +             DNOM(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI),
     +                  RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))
                RTRI(4,JTRI)=RTRI(4,ITRI)+(RTRI(7,ITRI)-RTRI(4,ITRI))*T
                RTRI(5,JTRI)=RTRI(5,ITRI)+(RTRI(8,ITRI)-RTRI(5,ITRI))*T
                RTRI(6,JTRI)=RTRI(6,ITRI)+(RTRI(9,ITRI)-RTRI(6,ITRI))*T
                RTRI(7,JTRI)=RTRI(7,ITRI)
                RTRI(8,JTRI)=RTRI(8,ITRI)
                RTRI(9,JTRI)=RTRI(9,ITRI)
                RTRI(10,JTRI)=RTRI(10,ITRI)
                RTRI(4,ITRI)=RTRI(1,JTRI)
                RTRI(5,ITRI)=RTRI(2,JTRI)
                RTRI(6,ITRI)=RTRI(3,JTRI)
                ITRI=JTRI
              END IF
C
            ELSE
C
C Point 1 in, point 2 out, point 3 out.
C
              T=-SIDE(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))/
     +           DNOM(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI),
     +                RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))
              RTRI(4,ITRI)=RTRI(1,ITRI)+(RTRI(4,ITRI)-RTRI(1,ITRI))*T
              RTRI(5,ITRI)=RTRI(2,ITRI)+(RTRI(5,ITRI)-RTRI(2,ITRI))*T
              RTRI(6,ITRI)=RTRI(3,ITRI)+(RTRI(6,ITRI)-RTRI(3,ITRI))*T
              T=-SIDE(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))/
     +           DNOM(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI),
     +                RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))
              RTRI(7,ITRI)=RTRI(1,ITRI)+(RTRI(7,ITRI)-RTRI(1,ITRI))*T
              RTRI(8,ITRI)=RTRI(2,ITRI)+(RTRI(8,ITRI)-RTRI(2,ITRI))*T
              RTRI(9,ITRI)=RTRI(3,ITRI)+(RTRI(9,ITRI)-RTRI(3,ITRI))*T
C
            END IF
C
          END IF
C
        ELSE
C
          IF (SIDE(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI)).LT.0.) THEN
C
            IF (SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI)).LT.0.) THEN
C
C Point 1 out, point 2 in, point 3 in.
C
              IF (NTRI.LT.MTRI) THEN
                JTRI=ITRI+1
                NTRI=NTRI+1
                IF (NTRI.NE.JTRI) THEN
                  RTRI(1,NTRI)=RTRI(1,JTRI)
                  RTRI(2,NTRI)=RTRI(2,JTRI)
                  RTRI(3,NTRI)=RTRI(3,JTRI)
                  RTRI(4,NTRI)=RTRI(4,JTRI)
                  RTRI(5,NTRI)=RTRI(5,JTRI)
                  RTRI(6,NTRI)=RTRI(6,JTRI)
                  RTRI(7,NTRI)=RTRI(7,JTRI)
                  RTRI(8,NTRI)=RTRI(8,JTRI)
                  RTRI(9,NTRI)=RTRI(9,JTRI)
                  RTRI(10,NTRI)=RTRI(10,JTRI)
                END IF
                T=-SIDE(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))/
     +             DNOM(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI),
     +                  RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))
                RTRI(1,JTRI)=RTRI(1,ITRI)+(RTRI(7,ITRI)-RTRI(1,ITRI))*T
                RTRI(2,JTRI)=RTRI(2,ITRI)+(RTRI(8,ITRI)-RTRI(2,ITRI))*T
                RTRI(3,JTRI)=RTRI(3,ITRI)+(RTRI(9,ITRI)-RTRI(3,ITRI))*T
                T=-SIDE(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))/
     +             DNOM(RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI),
     +                  RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))
                RTRI(4,JTRI)=RTRI(1,ITRI)+(RTRI(4,ITRI)-RTRI(1,ITRI))*T
                RTRI(5,JTRI)=RTRI(2,ITRI)+(RTRI(5,ITRI)-RTRI(2,ITRI))*T
                RTRI(6,JTRI)=RTRI(3,ITRI)+(RTRI(6,ITRI)-RTRI(3,ITRI))*T
                RTRI(7,JTRI)=RTRI(4,ITRI)
                RTRI(8,JTRI)=RTRI(5,ITRI)
                RTRI(9,JTRI)=RTRI(6,ITRI)
                RTRI(10,JTRI)=RTRI(10,ITRI)
                RTRI(1,ITRI)=RTRI(1,JTRI)
                RTRI(2,ITRI)=RTRI(2,JTRI)
                RTRI(3,ITRI)=RTRI(3,JTRI)
                ITRI=JTRI
              END IF
C
            ELSE
C
C Point 1 out, point 2 in, point 3 out.
C
              T=-SIDE(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))/
     +           DNOM(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI),
     +                RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))
              RTRI(1,ITRI)=RTRI(4,ITRI)+(RTRI(1,ITRI)-RTRI(4,ITRI))*T
              RTRI(2,ITRI)=RTRI(5,ITRI)+(RTRI(2,ITRI)-RTRI(5,ITRI))*T
              RTRI(3,ITRI)=RTRI(6,ITRI)+(RTRI(3,ITRI)-RTRI(6,ITRI))*T
              T=-SIDE(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))/
     +           DNOM(RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI),
     +                RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))
              RTRI(7,ITRI)=RTRI(4,ITRI)+(RTRI(7,ITRI)-RTRI(4,ITRI))*T
              RTRI(8,ITRI)=RTRI(5,ITRI)+(RTRI(8,ITRI)-RTRI(5,ITRI))*T
              RTRI(9,ITRI)=RTRI(6,ITRI)+(RTRI(9,ITRI)-RTRI(6,ITRI))*T
C
            END IF
C
          ELSE
C
            IF (SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI)).LT.0.) THEN
C
C Point 1 out, point 2 out, point 3 in.
C
              T=-SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))/
     +           DNOM(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI),
     +                RTRI(1,ITRI),RTRI(2,ITRI),RTRI(3,ITRI))
              RTRI(1,ITRI)=RTRI(7,ITRI)+(RTRI(1,ITRI)-RTRI(7,ITRI))*T
              RTRI(2,ITRI)=RTRI(8,ITRI)+(RTRI(2,ITRI)-RTRI(8,ITRI))*T
              RTRI(3,ITRI)=RTRI(9,ITRI)+(RTRI(3,ITRI)-RTRI(9,ITRI))*T
              T=-SIDE(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI))/
     +           DNOM(RTRI(7,ITRI),RTRI(8,ITRI),RTRI(9,ITRI),
     +                RTRI(4,ITRI),RTRI(5,ITRI),RTRI(6,ITRI))
              RTRI(4,ITRI)=RTRI(7,ITRI)+(RTRI(4,ITRI)-RTRI(7,ITRI))*T
              RTRI(5,ITRI)=RTRI(8,ITRI)+(RTRI(5,ITRI)-RTRI(8,ITRI))*T
              RTRI(6,ITRI)=RTRI(9,ITRI)+(RTRI(6,ITRI)-RTRI(9,ITRI))*T
C
            ELSE
C
C Point 1 out, point 2 out, point 3 out.
C
              IF (ITRI.NE.NTRI) THEN
                RTRI(1,ITRI)=RTRI(1,NTRI)
                RTRI(2,ITRI)=RTRI(2,NTRI)
                RTRI(3,ITRI)=RTRI(3,NTRI)
                RTRI(4,ITRI)=RTRI(4,NTRI)
                RTRI(5,ITRI)=RTRI(5,NTRI)
                RTRI(6,ITRI)=RTRI(6,NTRI)
                RTRI(7,ITRI)=RTRI(7,NTRI)
                RTRI(8,ITRI)=RTRI(8,NTRI)
                RTRI(9,ITRI)=RTRI(9,NTRI)
                RTRI(10,ITRI)=RTRI(10,NTRI)
              END IF
C
              ITRI=ITRI-1
              NTRI=NTRI-1
C
            END IF
C
          END IF
C
        END IF
C
C Done.
C
        RETURN
C
      END
