C
C $Id: tdctri.f,v 1.2 2000-07-12 16:26:31 haley Exp $
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
      SUBROUTINE TDCTRI (RTRI,MTRI,NTRI,IAXS,RCUT)
C
        DIMENSION RTRI(10,MTRI)
C
C The object of this routine is to cut into pieces all the triangles in
C a TDPACK triangle list that intersect a plane perpendicular to one of
C the axes; each piece lies entirely on one side or the other of that
C plane.  IAXS says which axis the plane is perpendicular to (IAXS = 1,
C 2, or 3 for the X, Y, or Z axis).  RCUT is the value along the axis at
C which the plane is positioned.
C
        NTIN=NTRI
C
        IF (IAXS.EQ.1) THEN
C
          DO 101 I=1,NTIN
            IF          (RTRI(1,I).LT.RCUT) THEN
              IF        (RTRI(4,I).LT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  GO TO 101                       !  1 <  4 <  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  IODD=7                          !  1 <  4 <  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  GO TO 101                       !  1 <  4 <  7 =
                END IF
              ELSE IF   (RTRI(4,I).GT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  IODD=4                          !  1 <  4 >  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  IODD=1                          !  1 <  4 >  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  IODD=7                          !  1 <  4 >  7 =
                END IF
              ELSE IF   (RTRI(4,I).EQ.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  GO TO 101                       !  1 <  4 =  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  IODD=4                          !  1 <  4 =  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  GO TO 101                       !  1 <  4 =  7 =
                END IF
              END IF
            ELSE IF     (RTRI(1,I).GT.RCUT) THEN
              IF        (RTRI(4,I).LT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  IODD=1                          !  1 >  4 <  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  IODD=4                          !  1 >  4 <  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  IODD=7                          !  1 >  4 <  7 =
                END IF
              ELSE IF   (RTRI(4,I).GT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  IODD=7                          !  1 >  4 >  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  GO TO 101                       !  1 >  4 >  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  GO TO 101                       !  1 >  4 >  7 =
                END IF
              ELSE IF   (RTRI(4,I).EQ.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  IODD=4                          !  1 >  4 =  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  GO TO 101                       !  1 >  4 =  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  GO TO 101                       !  1 >  4 =  7 =
                END IF
              END IF
            ELSE IF     (RTRI(1,I).EQ.RCUT) THEN
              IF        (RTRI(4,I).LT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  GO TO 101                       !  1 =  4 <  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  IODD=1                          !  1 =  4 <  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  GO TO 101                       !  1 =  4 <  7 =
                END IF
              ELSE IF   (RTRI(4,I).GT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  IODD=1                          !  1 =  4 >  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  GO TO 101                       !  1 =  4 >  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  GO TO 101                       !  1 =  4 >  7 =
                END IF
              ELSE IF   (RTRI(4,I).EQ.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
                  GO TO 101                       !  1 =  4 =  7 <
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
                  GO TO 101                       !  1 =  4 =  7 >
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
                  GO TO 101                       !  1 =  4 =  7 =
                END IF
              END IF
            END IF
            IOMI=MOD(IODD+5,9)+1
            IOPL=MOD(IODD+2,9)+1
            IF (RTRI(IODD,I).EQ.RCUT) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=RCUT
              RTRI(2,NTRI)=RTRI(IOMI+1,I)+
     +                     (RTRI(IOPL+1,I)-RTRI(IOMI+1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IOPL  ,I)-RTRI(IOMI  ,I)))
              RTRI(3,NTRI)=RTRI(IOMI+2,I)+
     +                     (RTRI(IOPL+2,I)-RTRI(IOMI+2,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IOPL  ,I)-RTRI(IOMI  ,I)))
              RTRI(4,NTRI)=RTRI(IODD  ,I)
              RTRI(5,NTRI)=RTRI(IODD+1,I)
              RTRI(6,NTRI)=RTRI(IODD+2,I)
              RTRI(7,NTRI)=RTRI(IOPL  ,I)
              RTRI(8,NTRI)=RTRI(IOPL+1,I)
              RTRI(9,NTRI)=RTRI(IOPL+2,I)
              RTRI(10,NTRI)=RTRI(10,I)
              RTRI(IOPL  ,I)=RTRI(1,NTRI)
              RTRI(IOPL+1,I)=RTRI(2,NTRI)
              RTRI(IOPL+2,I)=RTRI(3,NTRI)
              IF (NTRI.EQ.MTRI) RETURN
            ELSE
              NTRI=NTRI+1
              RTRI(1,NTRI)=RCUT
              RTRI(2,NTRI)=RTRI(IOMI+1,I)+
     +                     (RTRI(IODD+1,I)-RTRI(IOMI+1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOMI  ,I)))
              RTRI(3,NTRI)=RTRI(IOMI+2,I)+
     +                     (RTRI(IODD+2,I)-RTRI(IOMI+2,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOMI  ,I)))
              RTRI(4,NTRI)=RCUT
              RTRI(5,NTRI)=RTRI(IOPL+1,I)+
     +                     (RTRI(IODD+1,I)-RTRI(IOPL+1,I))*
     +                     ((         RCUT-RTRI(IOPL  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOPL  ,I)))
              RTRI(6,NTRI)=RTRI(IOPL+2,I)+
     +                     (RTRI(IODD+2,I)-RTRI(IOPL+2,I))*
     +                     ((         RCUT-RTRI(IOPL  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOPL  ,I)))
              RTRI(7,NTRI)=RTRI(IOPL  ,I)
              RTRI(8,NTRI)=RTRI(IOPL+1,I)
              RTRI(9,NTRI)=RTRI(IOPL+2,I)
              RTRI(10,NTRI)=RTRI(10,I)
              IF (NTRI.EQ.MTRI) RETURN
              NTRI=NTRI+1
              RTRI(1,NTRI)=RTRI(IOMI  ,I)
              RTRI(2,NTRI)=RTRI(IOMI+1,I)
              RTRI(3,NTRI)=RTRI(IOMI+2,I)
              RTRI(4,NTRI)=RTRI(1,NTRI-1)
              RTRI(5,NTRI)=RTRI(2,NTRI-1)
              RTRI(6,NTRI)=RTRI(3,NTRI-1)
              RTRI(7,NTRI)=RTRI(IOPL  ,I)
              RTRI(8,NTRI)=RTRI(IOPL+1,I)
              RTRI(9,NTRI)=RTRI(IOPL+2,I)
              RTRI(10,NTRI)=RTRI(10,I)
              RTRI(IOMI  ,I)=RTRI(1,NTRI-1)
              RTRI(IOMI+1,I)=RTRI(2,NTRI-1)
              RTRI(IOMI+2,I)=RTRI(3,NTRI-1)
              RTRI(IOPL  ,I)=RTRI(4,NTRI-1)
              RTRI(IOPL+1,I)=RTRI(5,NTRI-1)
              RTRI(IOPL+2,I)=RTRI(6,NTRI-1)
              IF (NTRI.EQ.MTRI) RETURN
            END IF
  101     CONTINUE
C
        ELSE IF (IAXS.EQ.2) THEN
C
          DO 102 I=1,NTIN
            IF          (RTRI(2,I).LT.RCUT) THEN
              IF        (RTRI(5,I).LT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  GO TO 102                       !  2 <  5 <  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  IODD=8                          !  2 <  5 <  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  GO TO 102                       !  2 <  5 <  8 =
                END IF
              ELSE IF   (RTRI(5,I).GT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  IODD=5                          !  2 <  5 >  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  IODD=2                          !  2 <  5 >  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  IODD=8                          !  2 <  5 >  8 =
                END IF
              ELSE IF   (RTRI(5,I).EQ.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  GO TO 102                       !  2 <  5 =  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  IODD=5                          !  2 <  5 =  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  GO TO 102                       !  2 <  5 =  8 =
                END IF
              END IF
            ELSE IF     (RTRI(2,I).GT.RCUT) THEN
              IF        (RTRI(5,I).LT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  IODD=2                          !  2 >  5 <  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  IODD=5                          !  2 >  5 <  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  IODD=8                          !  2 >  5 <  8 =
                END IF
              ELSE IF   (RTRI(5,I).GT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  IODD=8                          !  2 >  5 >  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  GO TO 102                       !  2 >  5 >  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  GO TO 102                       !  2 >  5 >  8 =
                END IF
              ELSE IF   (RTRI(5,I).EQ.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  IODD=5                          !  2 >  5 =  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  GO TO 102                       !  2 >  5 =  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  GO TO 102                       !  2 >  5 =  8 =
                END IF
              END IF
            ELSE IF     (RTRI(2,I).EQ.RCUT) THEN
              IF        (RTRI(5,I).LT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  GO TO 102                       !  2 =  5 <  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  IODD=2                          !  2 =  5 <  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  GO TO 102                       !  2 =  5 <  8 =
                END IF
              ELSE IF   (RTRI(5,I).GT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  IODD=2                          !  2 =  5 >  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  GO TO 102                       !  2 =  5 >  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  GO TO 102                       !  2 =  5 >  8 =
                END IF
              ELSE IF   (RTRI(5,I).EQ.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
                  GO TO 102                       !  2 =  5 =  8 <
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
                  GO TO 102                       !  2 =  5 =  8 >
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
                  GO TO 102                       !  2 =  5 =  8 =
                END IF
              END IF
            END IF
            IOMI=MOD(IODD+5,9)+1
            IOPL=MOD(IODD+2,9)+1
            IF (RTRI(IODD,I).EQ.RCUT) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=RTRI(IOMI-1,I)+
     +                     (RTRI(IOPL-1,I)-RTRI(IOMI-1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IOPL  ,I)-RTRI(IOMI  ,I)))
              RTRI(2,NTRI)=RCUT
              RTRI(3,NTRI)=RTRI(IOMI+1,I)+
     +                     (RTRI(IOPL+1,I)-RTRI(IOMI+1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IOPL  ,I)-RTRI(IOMI  ,I)))
              RTRI(4,NTRI)=RTRI(IODD-1,I)
              RTRI(5,NTRI)=RTRI(IODD  ,I)
              RTRI(6,NTRI)=RTRI(IODD+1,I)
              RTRI(7,NTRI)=RTRI(IOPL-1,I)
              RTRI(8,NTRI)=RTRI(IOPL  ,I)
              RTRI(9,NTRI)=RTRI(IOPL+1,I)
              RTRI(10,NTRI)=RTRI(10,I)
              RTRI(IOPL-1,I)=RTRI(1,NTRI)
              RTRI(IOPL  ,I)=RTRI(2,NTRI)
              RTRI(IOPL+1,I)=RTRI(3,NTRI)
              IF (NTRI.EQ.MTRI) RETURN
            ELSE
              NTRI=NTRI+1
              RTRI(1,NTRI)=RTRI(IOMI-1,I)+
     +                     (RTRI(IODD-1,I)-RTRI(IOMI-1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOMI  ,I)))
              RTRI(2,NTRI)=RCUT
              RTRI(3,NTRI)=RTRI(IOMI+1,I)+
     +                     (RTRI(IODD+1,I)-RTRI(IOMI+1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOMI  ,I)))
              RTRI(4,NTRI)=RTRI(IOPL-1,I)+
     +                     (RTRI(IODD-1,I)-RTRI(IOPL-1,I))*
     +                     ((         RCUT-RTRI(IOPL  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOPL  ,I)))
              RTRI(5,NTRI)=RCUT
              RTRI(6,NTRI)=RTRI(IOPL+1,I)+
     +                     (RTRI(IODD+1,I)-RTRI(IOPL+1,I))*
     +                     ((         RCUT-RTRI(IOPL  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOPL  ,I)))
              RTRI(7,NTRI)=RTRI(IOPL-1,I)
              RTRI(8,NTRI)=RTRI(IOPL  ,I)
              RTRI(9,NTRI)=RTRI(IOPL+1,I)
              RTRI(10,NTRI)=RTRI(10,I)
              IF (NTRI.EQ.MTRI) RETURN
              NTRI=NTRI+1
              RTRI(1,NTRI)=RTRI(IOMI-1,I)
              RTRI(2,NTRI)=RTRI(IOMI  ,I)
              RTRI(3,NTRI)=RTRI(IOMI+1,I)
              RTRI(4,NTRI)=RTRI(1,NTRI-1)
              RTRI(5,NTRI)=RTRI(2,NTRI-1)
              RTRI(6,NTRI)=RTRI(3,NTRI-1)
              RTRI(7,NTRI)=RTRI(IOPL-1,I)
              RTRI(8,NTRI)=RTRI(IOPL  ,I)
              RTRI(9,NTRI)=RTRI(IOPL+1,I)
              RTRI(10,NTRI)=RTRI(10,I)
              RTRI(IOMI-1,I)=RTRI(1,NTRI-1)
              RTRI(IOMI  ,I)=RTRI(2,NTRI-1)
              RTRI(IOMI+1,I)=RTRI(3,NTRI-1)
              RTRI(IOPL-1,I)=RTRI(4,NTRI-1)
              RTRI(IOPL  ,I)=RTRI(5,NTRI-1)
              RTRI(IOPL+1,I)=RTRI(6,NTRI-1)
              IF (NTRI.EQ.MTRI) RETURN
            END IF
  102     CONTINUE
C
        ELSE IF (IAXS.EQ.3) THEN
C
          DO 103 I=1,NTIN
            IF          (RTRI(3,I).LT.RCUT) THEN
              IF        (RTRI(6,I).LT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  GO TO 103                       !  3 <  6 <  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  IODD=9                          !  3 <  6 <  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  GO TO 103                       !  3 <  6 <  9 =
                END IF
              ELSE IF   (RTRI(6,I).GT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  IODD=6                          !  3 <  6 >  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  IODD=3                          !  3 <  6 >  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  IODD=9                          !  3 <  6 >  9 =
                END IF
              ELSE IF   (RTRI(6,I).EQ.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  GO TO 103                       !  3 <  6 =  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  IODD=6                          !  3 <  6 =  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  GO TO 103                       !  3 <  6 =  9 =
                END IF
              END IF
            ELSE IF     (RTRI(3,I).GT.RCUT) THEN
              IF        (RTRI(6,I).LT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  IODD=3                          !  3 >  6 <  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  IODD=6                          !  3 >  6 <  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  IODD=9                          !  3 >  6 <  9 =
                END IF
              ELSE IF   (RTRI(6,I).GT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  IODD=9                          !  3 >  6 >  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  GO TO 103                       !  3 >  6 >  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  GO TO 103                       !  3 >  6 >  9 =
                END IF
              ELSE IF   (RTRI(6,I).EQ.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  IODD=6                          !  3 >  6 =  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  GO TO 103                       !  3 >  6 =  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  GO TO 103                       !  3 >  6 =  9 =
                END IF
              END IF
            ELSE IF     (RTRI(3,I).EQ.RCUT) THEN
              IF        (RTRI(6,I).LT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  GO TO 103                       !  3 =  6 <  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  IODD=3                          !  3 =  6 <  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  GO TO 103                       !  3 =  6 <  9 =
                END IF
              ELSE IF   (RTRI(6,I).GT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  IODD=3                          !  3 =  6 >  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  GO TO 103                       !  3 =  6 >  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  GO TO 103                       !  3 =  6 >  9 =
                END IF
              ELSE IF   (RTRI(6,I).EQ.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
                  GO TO 103                       !  3 =  6 =  9 <
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
                  GO TO 103                       !  3 =  6 =  9 >
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
                  GO TO 103                       !  3 =  6 =  9 =
                END IF
              END IF
            END IF
            IOMI=MOD(IODD+5,9)+1
            IOPL=MOD(IODD+2,9)+1
            IF (RTRI(IODD,I).EQ.RCUT) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=RTRI(IOMI-2,I)+
     +                     (RTRI(IOPL-2,I)-RTRI(IOMI-2,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IOPL  ,I)-RTRI(IOMI  ,I)))
              RTRI(2,NTRI)=RTRI(IOMI-1,I)+
     +                     (RTRI(IOPL-1,I)-RTRI(IOMI-1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IOPL  ,I)-RTRI(IOMI  ,I)))
              RTRI(3,NTRI)=RCUT
              RTRI(4,NTRI)=RTRI(IODD-2,I)
              RTRI(5,NTRI)=RTRI(IODD-1,I)
              RTRI(6,NTRI)=RTRI(IODD  ,I)
              RTRI(7,NTRI)=RTRI(IOPL-2,I)
              RTRI(8,NTRI)=RTRI(IOPL-1,I)
              RTRI(9,NTRI)=RTRI(IOPL  ,I)
              RTRI(10,NTRI)=RTRI(10,I)
              RTRI(IOPL-2,I)=RTRI(1,NTRI)
              RTRI(IOPL-1,I)=RTRI(2,NTRI)
              RTRI(IOPL  ,I)=RTRI(3,NTRI)
              IF (NTRI.EQ.MTRI) RETURN
            ELSE
              NTRI=NTRI+1
              RTRI(1,NTRI)=RTRI(IOMI-2,I)+
     +                     (RTRI(IODD-2,I)-RTRI(IOMI-2,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOMI  ,I)))
              RTRI(2,NTRI)=RTRI(IOMI-1,I)+
     +                     (RTRI(IODD-1,I)-RTRI(IOMI-1,I))*
     +                     ((         RCUT-RTRI(IOMI  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOMI  ,I)))
              RTRI(3,NTRI)=RCUT
              RTRI(4,NTRI)=RTRI(IOPL-2,I)+
     +                     (RTRI(IODD-2,I)-RTRI(IOPL-2,I))*
     +                     ((         RCUT-RTRI(IOPL  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOPL  ,I)))
              RTRI(5,NTRI)=RTRI(IOPL-1,I)+
     +                     (RTRI(IODD-1,I)-RTRI(IOPL-1,I))*
     +                     ((         RCUT-RTRI(IOPL  ,I))/
     +                     (RTRI(IODD  ,I)-RTRI(IOPL  ,I)))
              RTRI(6,NTRI)=RCUT
              RTRI(7,NTRI)=RTRI(IOPL-2,I)
              RTRI(8,NTRI)=RTRI(IOPL-1,I)
              RTRI(9,NTRI)=RTRI(IOPL  ,I)
              RTRI(10,NTRI)=RTRI(10,I)
              IF (NTRI.EQ.MTRI) RETURN
              NTRI=NTRI+1
              RTRI(1,NTRI)=RTRI(IOMI-2,I)
              RTRI(2,NTRI)=RTRI(IOMI-1,I)
              RTRI(3,NTRI)=RTRI(IOMI  ,I)
              RTRI(4,NTRI)=RTRI(1,NTRI-1)
              RTRI(5,NTRI)=RTRI(2,NTRI-1)
              RTRI(6,NTRI)=RTRI(3,NTRI-1)
              RTRI(7,NTRI)=RTRI(IOPL-2,I)
              RTRI(8,NTRI)=RTRI(IOPL-1,I)
              RTRI(9,NTRI)=RTRI(IOPL  ,I)
              RTRI(10,NTRI)=RTRI(10,I)
              RTRI(IOMI-2,I)=RTRI(1,NTRI-1)
              RTRI(IOMI-1,I)=RTRI(2,NTRI-1)
              RTRI(IOMI  ,I)=RTRI(3,NTRI-1)
              RTRI(IOPL-2,I)=RTRI(4,NTRI-1)
              RTRI(IOPL-1,I)=RTRI(5,NTRI-1)
              RTRI(IOPL  ,I)=RTRI(6,NTRI-1)
              IF (NTRI.EQ.MTRI) RETURN
            END IF
  103     CONTINUE
C
        END IF
C
C Done.
C
        RETURN
C
      END
