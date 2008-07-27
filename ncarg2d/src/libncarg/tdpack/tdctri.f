C
C $Id: tdctri.f,v 1.5 2008-07-27 00:17:32 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C                 1 <  4 <  7 <
                  GO TO 101
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 <  4 <  7 >
                  IODD=7
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 <  4 <  7 =
                  GO TO 101
                END IF
              ELSE IF   (RTRI(4,I).GT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 <  4 >  7 <
                  IODD=4
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 <  4 >  7 >
                  IODD=1
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 <  4 >  7 =
                  IODD=7
                END IF
              ELSE IF   (RTRI(4,I).EQ.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 <  4 =  7 <
                  GO TO 101
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 <  4 =  7 >
                  IODD=4
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 <  4 =  7 =
                  GO TO 101
                END IF
              END IF
            ELSE IF     (RTRI(1,I).GT.RCUT) THEN
              IF        (RTRI(4,I).LT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 >  4 <  7 <
                  IODD=1
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 >  4 <  7 >
                  IODD=4
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 >  4 <  7 =
                  IODD=7
                END IF
              ELSE IF   (RTRI(4,I).GT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 >  4 >  7 <
                  IODD=7
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 >  4 >  7 >
                  GO TO 101
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 >  4 >  7 =
                  GO TO 101
                END IF
              ELSE IF   (RTRI(4,I).EQ.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 >  4 =  7 <
                  IODD=4
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 >  4 =  7 >
                  GO TO 101
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 >  4 =  7 =
                  GO TO 101
                END IF
              END IF
            ELSE IF     (RTRI(1,I).EQ.RCUT) THEN
              IF        (RTRI(4,I).LT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 =  4 <  7 <
                  GO TO 101
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 =  4 <  7 >
                  IODD=1
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 =  4 <  7 =
                  GO TO 101
                END IF
              ELSE IF   (RTRI(4,I).GT.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 =  4 >  7 <
                  IODD=1
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 =  4 >  7 >
                  GO TO 101
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 =  4 >  7 =
                  GO TO 101
                END IF
              ELSE IF   (RTRI(4,I).EQ.RCUT) THEN
                IF      (RTRI(7,I).LT.RCUT) THEN
C                 1 =  4 =  7 <
                  GO TO 101
                ELSE IF (RTRI(7,I).GT.RCUT) THEN
C                 1 =  4 =  7 >
                  GO TO 101
                ELSE IF (RTRI(7,I).EQ.RCUT) THEN
C                 1 =  4 =  7 =
                  GO TO 101
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
C                 2 <  5 <  8 <
                  GO TO 102
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 <  5 <  8 >
                  IODD=8
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 <  5 <  8 =
                  GO TO 102
                END IF
              ELSE IF   (RTRI(5,I).GT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 <  5 >  8 <
                  IODD=5
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 <  5 >  8 >
                  IODD=2
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 <  5 >  8 =
                  IODD=8
                END IF
              ELSE IF   (RTRI(5,I).EQ.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 <  5 =  8 <
                  GO TO 102
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 <  5 =  8 >
                  IODD=5
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 <  5 =  8 =
                  GO TO 102
                END IF
              END IF
            ELSE IF     (RTRI(2,I).GT.RCUT) THEN
              IF        (RTRI(5,I).LT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 >  5 <  8 <
                  IODD=2
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 >  5 <  8 >
                  IODD=5
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 >  5 <  8 =
                  IODD=8
                END IF
              ELSE IF   (RTRI(5,I).GT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 >  5 >  8 <
                  IODD=8
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 >  5 >  8 >
                  GO TO 102
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 >  5 >  8 =
                  GO TO 102
                END IF
              ELSE IF   (RTRI(5,I).EQ.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 >  5 =  8 <
                  IODD=5
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 >  5 =  8 >
                  GO TO 102
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 >  5 =  8 =
                  GO TO 102
                END IF
              END IF
            ELSE IF     (RTRI(2,I).EQ.RCUT) THEN
              IF        (RTRI(5,I).LT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 =  5 <  8 <
                  GO TO 102
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 =  5 <  8 >
                  IODD=2
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 =  5 <  8 =
                  GO TO 102
                END IF
              ELSE IF   (RTRI(5,I).GT.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 =  5 >  8 <
                  IODD=2
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 =  5 >  8 >
                  GO TO 102
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 =  5 >  8 =
                  GO TO 102
                END IF
              ELSE IF   (RTRI(5,I).EQ.RCUT) THEN
                IF      (RTRI(8,I).LT.RCUT) THEN
C                 2 =  5 =  8 <
                  GO TO 102
                ELSE IF (RTRI(8,I).GT.RCUT) THEN
C                 2 =  5 =  8 >
                  GO TO 102
                ELSE IF (RTRI(8,I).EQ.RCUT) THEN
C                 2 =  5 =  8 =
                  GO TO 102
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
C                 3 <  6 <  9 <
                  GO TO 103
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 <  6 <  9 >
                  IODD=9
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 <  6 <  9 =
                  GO TO 103
                END IF
              ELSE IF   (RTRI(6,I).GT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 <  6 >  9 <
                  IODD=6
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 <  6 >  9 >
                  IODD=3
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 <  6 >  9 =
                  IODD=9
                END IF
              ELSE IF   (RTRI(6,I).EQ.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 <  6 =  9 <
                  GO TO 103
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 <  6 =  9 >
                  IODD=6
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 <  6 =  9 =
                  GO TO 103
                END IF
              END IF
            ELSE IF     (RTRI(3,I).GT.RCUT) THEN
              IF        (RTRI(6,I).LT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 >  6 <  9 <
                  IODD=3
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 >  6 <  9 >
                  IODD=6
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 >  6 <  9 =
                  IODD=9
                END IF
              ELSE IF   (RTRI(6,I).GT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 >  6 >  9 <
                  IODD=9
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 >  6 >  9 >
                  GO TO 103
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 >  6 >  9 =
                  GO TO 103
                END IF
              ELSE IF   (RTRI(6,I).EQ.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 >  6 =  9 <
                  IODD=6
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 >  6 =  9 >
                  GO TO 103
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 >  6 =  9 =
                  GO TO 103
                END IF
              END IF
            ELSE IF     (RTRI(3,I).EQ.RCUT) THEN
              IF        (RTRI(6,I).LT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 =  6 <  9 <
                  GO TO 103
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 =  6 <  9 >
                  IODD=3
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 =  6 <  9 =
                  GO TO 103
                END IF
              ELSE IF   (RTRI(6,I).GT.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 =  6 >  9 <
                  IODD=3
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 =  6 >  9 >
                  GO TO 103
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 =  6 >  9 =
                  GO TO 103
                END IF
              ELSE IF   (RTRI(6,I).EQ.RCUT) THEN
                IF      (RTRI(9,I).LT.RCUT) THEN
C                 3 =  6 =  9 <
                  GO TO 103
                ELSE IF (RTRI(9,I).GT.RCUT) THEN
C                 3 =  6 =  9 >
                  GO TO 103
                ELSE IF (RTRI(9,I).EQ.RCUT) THEN
C                 3 =  6 =  9 =
                  GO TO 103
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
