
      PROGRAM TESTIT
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.  Use one
C of the following:
C
C       PARAMETER (IERF=6,LUNI=2,IWTY=1 ,IWID=1)  !  NCGM
C       PARAMETER (IERF=6,LUNI=2,IWTY=8 ,IWID=1)  !  X Windows
C       PARAMETER (IERF=6,LUNI=2,IWTY=20,IWID=1)  !  PostScript
C       PARAMETER (IERF=6,LUNI=2,IWTY=11,IWID=1)  !  PDF, Portrait
C       PARAMETER (IERF=6,LUNI=2,IWTY=12,IWID=1)  !  PDF, Landscape
C
        PARAMETER (IERF=6,LUNI=2,IWTYPE=1,IWTY=IWTYPE,IWID=1)
C
C This program draws three frames comparing the geodesic grids used in
C the CONPACKT examples "ctgeo2", which was generated mathematically at
C NCAR, and "ctgeo3", which was generated mathematically elsewhere and
C massaged in some fashion to make the triangles all be more nearly the
C same size; the data for "ctgeo3" were downloaded from this Web site:
C
C   http://kiwi.atmos.colostate.edu:16080/BUGS/projects/geodesic/
C
C The first frame shows: in red, the five- or six-sided polygonal
C patches of the "ctgeo3" example; in green, the triangles resulting
C from connecting the center points of adjacent patches; and, in blue,
C the mathematically-generated triangles of the "ctgeo2" example.  Use
C "idt" to zoom in on this frame and compare the triangles in different
C areas
C
C The second frame shows a histogram of the areas of the triangles of
C example "ctgeo2", to be compared to the histogram on the third frame.
C
C The third frame shows a histogram of the areas of the triangles of
C example "ctgeo3".  Note how much more tightly they are grouped.
C
C Define the number of segments into which each edge of the icosahedron
C is to be divided and the number of triangles that result.
C
        PARAMETER (NDIV=16,NTRI=20*NDIV*NDIV+1)
C
C Declare a common block with a variable we can use to make AUTOGRAPH
C use PLCHHQ.
C
        COMMON /PSSFLG/ IFLG
C
C Declare arrays into which to read the geodesic data.
C
        DIMENSION VLAT(2562),VLON(2562)
        DIMENSION XCOV(2562),YCOV(2562),ZCOV(2562)
        DIMENSION CLAT(6,2562),CLON(6,2562)
        DIMENSION IADJ(2562,6)
        DIMENSION IWRK(2562)
C
C Declare an array in which to get the prime factors of an integer.
C
        DIMENSION IFOI(32)
C
C Declare arrays in which to put information about triangles resulting
C from my elaboration of the icosahedron.
C
        DIMENSION XTRI(3,NTRI),YTRI(3,NTRI),ZTRI(3,NTRI)
        DIMENSION TLAT(3,NTRI),TLON(3,NTRI)
        DIMENSION AREA(NTRI),BREA(15372)
C
C Declare arrays needed to create a histogram of the distribution of
C triangle areas.
C
        PARAMETER (NBNS= 120,RBNS=NBNS)
C       PARAMETER (NBNS=1000,RBNS=NBNS)
        DIMENSION XCEN(NBNS),YCEN(NBNS),ZERO(NBNS)
        DIMENSION XCRD(4),YCRD(4)
C
C Declare a variable to hold a label for the histogram plots.
C
        CHARACTER*48 LABL
C
C Declare arrays to hold information describing an icosahedron.
C
        DIMENSION IFCE(3,20),XCVI(12),YCVI(12),ZCVI(12)
C
C Declare a character variable for labelling purposes.
C
        CHARACTER*6 DIGT
C
C Define the 20 faces of an icosahedron (pointers to vertices).
C
        DATA IFCE(1, 1),IFCE(2, 1),IFCE(3, 1) /  1, 3, 5 /
        DATA IFCE(1, 2),IFCE(2, 2),IFCE(3, 2) /  1, 5, 7 /
        DATA IFCE(1, 3),IFCE(2, 3),IFCE(3, 3) /  1, 7, 9 /
        DATA IFCE(1, 4),IFCE(2, 4),IFCE(3, 4) /  1, 9,11 /
        DATA IFCE(1, 5),IFCE(2, 5),IFCE(3, 5) /  1, 3,11 /
        DATA IFCE(1, 6),IFCE(2, 6),IFCE(3, 6) /  3, 8,11 /
        DATA IFCE(1, 7),IFCE(2, 7),IFCE(3, 7) /  3, 8,10 /
        DATA IFCE(1, 8),IFCE(2, 8),IFCE(3, 8) /  3, 5,10 /
        DATA IFCE(1, 9),IFCE(2, 9),IFCE(3, 9) /  5,10,12 /
        DATA IFCE(1,10),IFCE(2,10),IFCE(3,10) /  5, 7,12 /
        DATA IFCE(1,11),IFCE(2,11),IFCE(3,11) /  4, 7,12 /
        DATA IFCE(1,12),IFCE(2,12),IFCE(3,12) /  4, 7, 9 /
        DATA IFCE(1,13),IFCE(2,13),IFCE(3,13) /  4, 6, 9 /
        DATA IFCE(1,14),IFCE(2,14),IFCE(3,14) /  6, 9,11 /
        DATA IFCE(1,15),IFCE(2,15),IFCE(3,15) /  6, 8,11 /
        DATA IFCE(1,16),IFCE(2,16),IFCE(3,16) /  2, 6, 8 /
        DATA IFCE(1,17),IFCE(2,17),IFCE(3,17) /  2, 4, 6 /
        DATA IFCE(1,18),IFCE(2,18),IFCE(3,18) /  2, 4,12 /
        DATA IFCE(1,19),IFCE(2,19),IFCE(3,19) /  2,10,12 /
        DATA IFCE(1,20),IFCE(2,20),IFCE(3,20) /  2, 8,10 /
C
C Define the 12 vertices of an icosahedron (note radius less than one).
C
        DATA XCVI / .0000000000000 ,  .0000000000000 , -.8506508083520 ,
     +              .8506508083520 , -.2628655560596 ,  .2628655560596 ,
     +              .6881909602356 , -.6881909602356 ,  .6881909602356 ,
     +             -.6881909602356 , -.2628655560595 ,  .2628655560596 /
        DATA YCVI / .0000000000000 ,  .0000000000000 ,  .0000000000000 ,
     +              .0000000000000 , -.8090169943749 ,  .8090169943749 ,
     +             -.5000000000000 ,  .5000000000000 ,  .5000000000000 ,
     +             -.5000000000000 ,  .8090169943749 , -.8090169943749 /
        DATA ZCVI / .9510565162952 , -.9510565162951 ,  .4253254041760 ,
     +             -.4253254041760 ,  .4253254041760 , -.4253254041760 ,
     +              .4253254041760 , -.4253254041760 ,  .4253254041760 ,
     +             -.4253254041760 ,  .4253254041760 , -.4253254041760 /
C
C Define a label for the histogram plots.
C
        DATA LABL / 'HISTOGRAM OF TRIANGLE AREAS (ctgeox)   xxxx BINS' /
C
C Conversion factor, radians to degrees.
C
        DATA RTOD / 57.2957795130823D0 /
C
C Value of pi.
C
        DATA PI   / 3.14159265358979 /
C
C Define characters for labeling.
C
        DATA DIGT / '123456' /
C
C Define minimum and maximum values for X and Y on the histograms.
C
        DATA XMIN,XMAX / .85 , 1.25 /
        DATA YMIN,YMAX /  0. ,  .35 /
C
C Read required data from the ASCII file "ctgeo3.dat", which is actually
C associated with the CONPACKT example "ctgeo3".
C
        OPEN (11,FILE='ctgeo3.dat',STATUS='OLD',FORM='FORMATTED')
C
        READ (11,'(5E16.8)') VLAT
        READ (11,'(5E16.8)') VLON
        READ (11,'(5E16.8)') (ZDAT,I=1,2562)
        READ (11,'(5E16.8)') CLAT
        READ (11,'(5E16.8)') CLON
        READ (11,'(  10I8)') (IOLF,I=1,2562)
C
        CLOSE (11)
C
C Convert the latitudes and longitudes of the mesh points to degrees,
C compute equivalent X, Y, and Z coordinates, and eliminate duplicate
C corner points.
C
        DO 104 I=1,2562
          VLAT(I)=RTOD*VLAT(I)
          VLON(I)=RTOD*VLON(I)
          CALL LLPXYZ (VLAT(I),VLON(I),XCOV(I),YCOV(I),ZCOV(I))
          DO 101 J=1,6
            CLAT(J,I)=RTOD*CLAT(J,I)
            CLON(J,I)=RTOD*CLON(J,I)
  101     CONTINUE
          DO 103 J=1,6
            JM1=MOD(J+4,6)+1
            IF (CLAT(J,I).EQ.CLAT(JM1,I).AND.
     +          CLON(J,I).EQ.CLON(JM1,I)) THEN
              DO 102 K=J,5
                CLAT(K,I)=CLAT(K+1,I)
                CLON(K,I)=CLON(K+1,I)
  102         CONTINUE
              CLAT(6,I)=99.
              GO TO 104
            END IF
  103     CONTINUE
  104   CONTINUE
C
C Set up the adjacency index array (time-consuming brute-force method).
C This code is commented out, but I have left it here because it's a bit
C easier to read than the code that replaced it.  The idea is to examine
C each side of each of the 2562 five- or six-sided polygonal patches
C (having a center defined by VLAT(I) and VLON(I) and corners defined
C by the contents of CLAT(J,I) and CLON(J,I) for J from 1 to NOJS) and
C find a matching side in one of the other polygonal patches.  We take
C advantage of the fact that the corners are given in counterclockwise
C order.  We define the contents of an "adjacency array" so that, once
C we are done, we have the indices of the five or six neighbors of each
C of the polygonal patches.  Given this adjacency array, the triangles
C of the triangular mesh can be formed (by just connecting the center
C points of neighboring patches).
C
C       DO 108 I=1,2562
C         IF (CLAT(6,I).EQ.99.) THEN
C           IADJ(I,6)=-1
C           NOJS=5
C         ELSE
C           NOJS=6
C         END IF
C         DO 107 J=1,NOJS
C           JM1=MOD(J+NOJS-2,NOJS)+1
C           DO 106 K=1,2562
C             IF (K.NE.I) THEN
C               IF (CLAT(6,K).EQ.99.) THEN
C                 NOLS=5
C               ELSE
C                 NOLS=6
C               END IF
C               DO 105 L=1,NOLS
C                 LP1=MOD(L,NOLS)+1
C                 IF (CLAT(J  ,I).EQ.CLAT(L  ,K).AND.
C    +                CLON(J  ,I).EQ.CLON(L  ,K).AND.
C    +                CLAT(JM1,I).EQ.CLAT(LP1,K).AND.
C    +                CLON(JM1,I).EQ.CLON(LP1,K)) THEN
C                   IADJ(I,J)=K
C                   GO TO 107
C                 END IF
C 105           CONTINUE
C             END IF
C 106       CONTINUE
C 107     CONTINUE
C 108   CONTINUE
C
C Set up the adjacency index array (better method).  This is just like
C the brute-force method commented out above, but is about an order of
C magnitude faster because we order the vertices of the center points
C of the polygonal patches by latitude, making it possible to look for
C matching edges in nearby polygonal patches first.  (If we're trying
C to find a match for a side of the Ith polygonal patch (sorted order),
C we look at polygonal patches for IC = I+1, I-1, I+2, I-2, and so on,
C until we find the one we want.  If IC becomes less than 1 or greater
C than 2562, we just finish out the rest of the list by stepping through
C the remaining elements on the opposite end of it; IM implements the
C logic to manage this.
C
        CALL CTSORT (VLAT,2562,IWRK)
C
        DO 108 I=1,2562
          IF (CLAT(6,IWRK(I)).EQ.99.) THEN
            IADJ(IWRK(I),6)=-1
            NOJS=5
          ELSE
            NOJS=6
          END IF
          DO 107 J=1,NOJS
            JM1=MOD(J+NOJS-2,NOJS)+1
            IC=I
            IM=0
            DO 106 K=1,2561
              IF (IM.EQ.0) THEN
                IF (MOD(K,2).EQ.0) THEN
                  IC=IC-K
                  IF (IC.LT.1) THEN
                    IC=IC+K+1
                    IM=+1
                  END IF
                ELSE
                  IC=IC+K
                  IF (IC.GT.2562) THEN
                    IC=IC-K-1
                    IM=-1
                  END IF
                END IF
              ELSE
                IC=IC+IM
              END IF
              IF (CLAT(6,IWRK(IC)).EQ.99.) THEN
                NOLS=5
              ELSE
                NOLS=6
              END IF
              DO 105 L=1,NOLS
                LP1=MOD(L,NOLS)+1
                IF (CLAT(J  ,IWRK(I)).EQ.CLAT(L  ,IWRK(IC)).AND.
     +              CLON(J  ,IWRK(I)).EQ.CLON(L  ,IWRK(IC)).AND.
     +              CLAT(JM1,IWRK(I)).EQ.CLAT(LP1,IWRK(IC)).AND.
     +              CLON(JM1,IWRK(I)).EQ.CLON(LP1,IWRK(IC))) THEN
                  IADJ(IWRK(I),J)=IWRK(IC)
                  GO TO 107
                END IF
  105         CONTINUE
  106       CONTINUE
  107     CONTINUE
  108   CONTINUE
C
C Create a mesh using the algorithm of CONPACKT example "ctex02":
C
C First, project the vertices of the icosahedron out onto the surface
C of a sphere of radius 1.
C
        DO 109 I=1,12
          DNOM=SQRT(XCVI(I)*XCVI(I)+YCVI(I)*YCVI(I)+ZCVI(I)*ZCVI(I))
          XCVI(I)=XCVI(I)/DNOM
          YCVI(I)=YCVI(I)/DNOM
          ZCVI(I)=ZCVI(I)/DNOM
  109   CONTINUE
C
C Next, define the twenty initial triangles of the icosahedron to be
C subdivided.
C
        KTRI=0
C
        DO 110 I=1,20
          KTRI=KTRI+1
          XTRI(1,KTRI)=XCVI(IFCE(1,I))
          YTRI(1,KTRI)=YCVI(IFCE(1,I))
          ZTRI(1,KTRI)=ZCVI(IFCE(1,I))
          XTRI(2,KTRI)=XCVI(IFCE(2,I))
          YTRI(2,KTRI)=YCVI(IFCE(2,I))
          ZTRI(2,KTRI)=ZCVI(IFCE(2,I))
          XTRI(3,KTRI)=XCVI(IFCE(3,I))
          YTRI(3,KTRI)=YCVI(IFCE(3,I))
          ZTRI(3,KTRI)=ZCVI(IFCE(3,I))
  110   CONTINUE
C
C Then, get the prime factors of the number NDIV, which says how many
C segments each edge of the icosahedron will be divided into.
C
        CALL FACTOR (NDIV,IFOI,NFOI)
C
C Loop on the array of prime factors, subdividing the triangles as
C dictatate by them.
C
        DO 114 I=1,NFOI
C
          PRINT '(''PRIME FACTOR:'',I4)' , IFOI(I)
C
          DO 113 J=1,KTRI
C
            DO 112 K=0,IFOI(I)-1
C
              DO 111 L=0,2*(IFOI(I)-K-1)
C
                IF (KTRI.GE.NTRI) THEN
                  PRINT * , 'STOP - KTRI.GE.NTRI'
                  STOP
                END IF
C
                KTRI=KTRI+1
C
                IF (MOD(L,2).EQ.0) THEN
C
                  XTRI(1,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2  )/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*XTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*XTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*XTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*XTRI(3,J))
                  YTRI(1,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2  )/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*YTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*YTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*YTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*YTRI(3,J))
                  ZTRI(1,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2  )/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*ZTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*ZTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*ZTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*ZTRI(3,J))
C
                  XTRI(2,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*XTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*XTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*XTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*XTRI(3,J))
                  YTRI(2,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*YTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*YTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*YTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*YTRI(3,J))
                  ZTRI(2,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*ZTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*ZTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*ZTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*ZTRI(3,J))
C
                  IF (IFOI(I)-K-1.NE.0) THEN
C
                    XTRI(3,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*XTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*XTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*XTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*XTRI(3,J))
                    YTRI(3,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*YTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*YTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*YTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*YTRI(3,J))
                    ZTRI(3,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*ZTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*ZTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*ZTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*ZTRI(3,J))
C
                  ELSE
C
                    XTRI(3,KTRI)=XTRI(3,J)
                    YTRI(3,KTRI)=YTRI(3,J)
                    ZTRI(3,KTRI)=ZTRI(3,J)
C
                  END IF
C
                ELSE
C
                  XTRI(1,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*XTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*XTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*XTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*XTRI(3,J))
                  YTRI(1,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*YTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*YTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*YTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*YTRI(3,J))
                  ZTRI(1,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*ZTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*ZTRI(3,J))+
     +                     (REAL(          L/2  )/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*ZTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*ZTRI(3,J))
C
                  XTRI(2,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-2)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*XTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*XTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*XTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*XTRI(3,J))
                  YTRI(2,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-2)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*YTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*YTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*YTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*YTRI(3,J))
                  ZTRI(2,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-2)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*ZTRI(1,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*ZTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K-1))*
     +                    ((REAL(IFOI(I)-K-1)/REAL(IFOI(I)))*ZTRI(2,J)+
     +                     (REAL(        K+1)/REAL(IFOI(I)))*ZTRI(3,J))
C
                  XTRI(3,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*XTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*XTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*XTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*XTRI(3,J))
                  YTRI(3,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*YTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*YTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*YTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*YTRI(3,J))
                  ZTRI(3,KTRI)=
     +                     (REAL(IFOI(I)-K-L/2-1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*ZTRI(1,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*ZTRI(3,J))+
     +                     (REAL(          L/2+1)/REAL(IFOI(I)-K  ))*
     +                    ((REAL(IFOI(I)-K  )/REAL(IFOI(I)))*ZTRI(2,J)+
     +                     (REAL(        K  )/REAL(IFOI(I)))*ZTRI(3,J))
C
                END IF
C
                DNOM=SQRT(XTRI(1,KTRI)*XTRI(1,KTRI)+
     +                    YTRI(1,KTRI)*YTRI(1,KTRI)+
     +                    ZTRI(1,KTRI)*ZTRI(1,KTRI))
                XTRI(1,KTRI)=XTRI(1,KTRI)/DNOM
                YTRI(1,KTRI)=YTRI(1,KTRI)/DNOM
                ZTRI(1,KTRI)=ZTRI(1,KTRI)/DNOM
C
                DNOM=SQRT(XTRI(2,KTRI)*XTRI(2,KTRI)+
     +                    YTRI(2,KTRI)*YTRI(2,KTRI)+
     +                    ZTRI(2,KTRI)*ZTRI(2,KTRI))
                XTRI(2,KTRI)=XTRI(2,KTRI)/DNOM
                YTRI(2,KTRI)=YTRI(2,KTRI)/DNOM
                ZTRI(2,KTRI)=ZTRI(2,KTRI)/DNOM
C
                DNOM=SQRT(XTRI(3,KTRI)*XTRI(3,KTRI)+
     +                    YTRI(3,KTRI)*YTRI(3,KTRI)+
     +                    ZTRI(3,KTRI)*ZTRI(3,KTRI))
     +
                XTRI(3,KTRI)=XTRI(3,KTRI)/DNOM
                YTRI(3,KTRI)=YTRI(3,KTRI)/DNOM
                ZTRI(3,KTRI)=ZTRI(3,KTRI)/DNOM
C
  111         CONTINUE
C
  112       CONTINUE
C
            XTRI(1,J)=XTRI(1,KTRI)
            YTRI(1,J)=YTRI(1,KTRI)
            ZTRI(1,J)=ZTRI(1,KTRI)
            XTRI(2,J)=XTRI(2,KTRI)
            YTRI(2,J)=YTRI(2,KTRI)
            ZTRI(2,J)=ZTRI(2,KTRI)
            XTRI(3,J)=XTRI(3,KTRI)
            YTRI(3,J)=YTRI(3,KTRI)
            ZTRI(3,J)=ZTRI(3,KTRI)
            KTRI=KTRI-1
C
  113     CONTINUE
C
  114   CONTINUE
C
C Open GKS.
C
        CALL GOPKS (IERR,0)
        CALL GOPWK (IWID,LUNI,IWTY)
        CALL GACWK (IWID)
C
C Turn off clipping by GKS.
C
        CALL GSCLIP (0)
C
C Define colors (0 = white, background; 1 = black, foreground; 2 =
C yellow; 3 = magenta; 4 = red; 5 = cyan; 6 = green; and 7 = blue).
C
        CALL GSCR   (IWID,0,1.,1.,1.)
        CALL GSCR   (IWID,1,0.,0.,0.)
        CALL GSCR   (IWID,2,1.,1.,0.)
        CALL GSCR   (IWID,3,1.,0.,1.)
        CALL GSCR   (IWID,4,1.,0.,0.)
        CALL GSCR   (IWID,5,0.,1.,1.)
        CALL GSCR   (IWID,6,0.,1.,0.)
        CALL GSCR   (IWID,7,0.,0.,1.)
C
C Tell PLOTCHAR to use one of the filled fonts and to outline each
C character.
C
        CALL PCSETI ('FN',25)
        CALL PCSETI ('OF',1)
C
C Tell PLOTCHAR to expect a character other than a colon as the
C function-control signal character.
C
        CALL PCSETC ('FC','|')
C
C Tell EZMAP not to draw labels or a perimeter.
C
        CALL MPSETI ('LA',0)
        CALL MPSETI ('PE',0)
C
C Do two frames, one showing the entire globe and another showing a
C "zoomed in" view of its center.
C
        DO 121 IFRA=1,2
C
C Initialize EZMAP and draw a background map.
C
          IF (IFRA.EQ.1) THEN
            CALL PLCHHQ (CFUX(.50),CFUY(.96),
     +               'GLOBAL VIEW OF MESHES FROM "ctgeo2" AND "ctgeo3"',
     +                                                       .016,0.,0.)
          ELSE
            CALL PLCHHQ (CFUX(.50),CFUY(.96),
     +               'CLOSER VIEW OF MESHES FROM "ctgeo2" AND "ctgeo3"',
     +                                                       .016,0.,0.)
          END IF
C
C Tell EZMAP where to put the map on the plotter frame.
C
          CALL MAPPOS (.10,.90,.12,.92)
C
C Tell EZMAP what projection to use.
C
          CALL MAPROJ ('OR', 45.,-45.,0.)
C
C Tell EZMAP how much of the map to show (either an entire hemisphere
C or a 40-degree field of view).
C
          IF (IFRA.EQ.1) THEN
            CALL MAPSET ('MA', 0., 0., 0., 0.)
          ELSE
            CALL MAPSET ('AN',20.,20.,20.,20.)
          END IF
C
C Draw the map.
C
          CALL MAPDRW
C
C In red, draw the pentagons and hexagons specified by the user data.
C
          CALL PLOTIT (0,0,2)
          CALL GSPLCI (4)
C
          DO 117 I=1,2562
            IF (CLAT(6,I).EQ.99.) THEN
              NOJS=5
            ELSE
              NOJS=6
            END IF
            CALL MAPIT (CLAT(NOJS,I),CLON(NOJS,I),0)
            DO 115 J=1,NOJS-1
              CALL MAPIT (CLAT(J,I),CLON(J,I),1)
  115       CONTINUE
            CALL MAPIT (CLAT(NOJS,I),CLON(NOJS,I),2)
            CALL MAPIQ
            DO 116 J=1,NOJS
              CALL FPSGCR (VLAT(I),VLON(I),CLAT(J,I),CLON(J,I),.85,
     +                                                   XLAT,XLON)
              CALL MAPTRA (XLAT,XLON,XPOS,YPOS)
              IF (XPOS.NE.1.E12) THEN
                CALL PLCHMQ (XPOS,YPOS,DIGT(J:J),.001,0.,0.)
              END IF
  116       CONTINUE
  117     CONTINUE
C
C In green, draw the triangular mesh developed from the user data.
C
          CALL PLOTIT (0,0,2)
          CALL GSPLCI (6)
C
          DO 119 I=1,2562
            DO 118 J=1,6
              IF (IADJ(I,J).GT.0) THEN
                CALL MAPIT (VLAT(I),VLON(I),0)
                CALL MAPIT (VLAT(IADJ(I,J)),VLON(IADJ(I,J)),2)
              END IF
  118       CONTINUE
  119     CONTINUE
C
C Compute the latitudes and longitudes of all the vertices and draw
C the triangles of the computed mesh of example "ctgeo2"
C
          CALL PLOTIT (0,0,2)
          CALL GSPLCI (7)
C
          AMIN=1.E36
          AMAX=-1.E36
          ASUM=0.
C
          DO 120 I=1,KTRI
            TLAT(1,I)=RTOD*ASIN (ZTRI(1,I))
            TLON(1,I)=RTOD*ATAN2(YTRI(1,I),XTRI(1,I))
            TLAT(2,I)=RTOD*ASIN (ZTRI(2,I))
            TLON(2,I)=RTOD*ATAN2(YTRI(2,I),XTRI(2,I))
            TLAT(3,I)=RTOD*ASIN (ZTRI(3,I))
            TLON(3,I)=RTOD*ATAN2(YTRI(3,I),XTRI(3,I))
            CALL MAPIT (TLAT(1,I),TLON(1,I),0)
            CALL MAPIT (TLAT(2,I),TLON(2,I),2)
            CALL MAPIT (TLAT(3,I),TLON(3,I),2)
            CALL MAPIT (TLAT(1,I),TLON(1,I),2)
  120     CONTINUE
C
          CALL PLCHHQ (CFUX(.50),CFUY(.098),'The computed triangular mes
     +h from example "ctgeo2" is in blue; the polygonal patches and the'
     +,
     +                                                       .012,0.,0.)
C
          CALL PLCHHQ (CFUX(.50),CFUY(.074), 'derived triangular mesh fr
     +om the user-provided data of "ctgeo3" are in red and green, respec
     +tively.',
     +                                                       .012,0.,0.)
C
          IF (IFRA.EQ.1) THEN
            CALL PLCHHQ (CFUX(.50),CFUY(.050),'Zoom (using "idt") to see
     + details of the meshes (also, see the next frame).',.012,0.,0.)
          ELSE
            CALL PLCHHQ (CFUX(.50),CFUY(.050),'Zoom (using "idt") to see
     + details of the meshes.',.012,0.,0.)
          END IF
C
C Label visible points of the icosahedron.
C
          CALL PLOTIT (0,0,2)
          CALL GSPLCI (1)
          CALL LBLPNT (XCVI,YCVI,ZCVI)
C
C Advance the frame.
C
          CALL FRAME
C
  121   CONTINUE
C
C Initialize AUTOGRAPH.
C
          IFLG=1
          CALL AGSETI ('FRAME.',2)
          CALL AGSETR ('GRID/LE.',.1)
          CALL AGSETR ('GRID/RI.',.9)
          CALL AGSETR ('GRID/BO.',.1)
          CALL AGSETR ('GRID/TO.',.9)
          CALL AGSETR ('X/MIN.',XMIN)
          CALL AGSETR ('X/MAX.',XMAX)
          CALL AGSETR ('Y/MIN.',YMIN)
          CALL AGSETR ('Y/MAX.',YMAX)
          CALL AGSETI ('LINE/MAX.',100)
          CALL AGSETR ('LEF/MA/OU.',0.012)
          CALL AGSETR ('LEF/MA/IN.',0.000)
          CALL AGSETR ('LEF/MI/OU.',0.008)
          CALL AGSETR ('LEF/MI/IN.',0.000)
          CALL AGSETR ('RIG/MA/OU.',0.000)
          CALL AGSETR ('RIG/MA/IN.',0.000)
          CALL AGSETR ('RIG/MI/OU.',0.000)
          CALL AGSETR ('RIG/MI/IN.',0.000)
          CALL AGSETR ('BOT/MA/OU.',0.012)
          CALL AGSETR ('BOT/MA/IN.',0.000)
          CALL AGSETR ('BOT/MI/OU.',0.008)
          CALL AGSETR ('BOT/MI/IN.',0.000)
          CALL AGSETR ('TOP/MA/OU.',0.000)
          CALL AGSETR ('TOP/MA/IN.',0.000)
          CALL AGSETR ('TOP/MI/OU.',0.000)
          CALL AGSETR ('TOP/MI/IN.',0.000)
C
          CALL ANOTAT ('AREA OF TRIANGLE (AS A FRACTION OF AVERAGE)',
     +                 'FRACTION OF TRIANGLES HAVING THAT AREA',0,0,0,0)
C
C Compute the areas of all the triangles of the computed grid and find
C the minimum, maximum, and average of the areas.
C
        AMIN=1.E36
        AMAX=-1.E36
        ASUM=0.
C
        DO 122 I=1,KTRI
          A=SQRT((XTRI(1,I)-XTRI(2,I))**2+
     +           (YTRI(1,I)-YTRI(2,I))**2+
     +           (ZTRI(1,I)-ZTRI(2,I))**2)
          B=SQRT((XTRI(2,I)-XTRI(3,I))**2+
     +           (YTRI(2,I)-YTRI(3,I))**2+
     +           (ZTRI(2,I)-ZTRI(3,I))**2)
          C=SQRT((XTRI(3,I)-XTRI(1,I))**2+
     +           (YTRI(3,I)-YTRI(1,I))**2+
     +           (ZTRI(3,I)-ZTRI(1,I))**2)
          S=.5*(A+B+C)
          AREA(I)=SQRT(S*(S-A)*(S-B)*(S-C))
          AMIN=MIN(AMIN,AREA(I))
          AMAX=MAX(AMAX,AREA(I))
          ASUM=ASUM+AREA(I)
  122   CONTINUE
C
        AVRG=ASUM/REAL(KTRI)
C
C Print the minimum, maximum, and average areas.
C
        PRINT * , ' '
        PRINT '(''COMPUTED GRID OF "ctex02"'')'
        PRINT * , ' '
        PRINT '(''TRIANGLES:    '',     I8)' , KTRI
        PRINT '(''MINIMUM AREA: '',1PE16.8)' , AMIN
        PRINT '(''MAXIMUM AREA: '',1PE16.8)' , AMAX
        PRINT '(''AVERAGE AREA: '',1PE16.8)' , AVRG
        PRINT '(''EXPECTED AVG: '',1PE16.8)' , 4.*PI/REAL(KTRI)
C
C Draw a histogram showing the distribution of areas.
C
        DO 123 I=1,NBNS
          XCEN(I)=XMIN+(REAL(I-1)+.5)*(XMAX-XMIN)/RBNS
          YCEN(I)=0.
          ZERO(I)=0.
  123   CONTINUE
C
        DO 124 I=1,KTRI
          K=MAX(1,MIN(NBNS,1+INT(RBNS*(AREA(I)/AVRG-XMIN)/
     +                                                  (XMAX-XMIN))))
          YCEN(K)=YCEN(K)+1.
  124   CONTINUE
C
        DO 125 I=1,NBNS
          YCEN(I)=YCEN(I)/REAL(KTRI)
  125   CONTINUE
C
        LABL(35:35)='2'
        WRITE (LABL(40:43),'(I4)') NBNS
C
        CALL EZXY   (XCEN,ZERO,NBNS,LABL)
C
        DO 126 I=1,NBNS
          XCRD(1)=XMIN+(REAL(I-1)/RBNS)*(XMAX-XMIN)
          YCRD(1)=0.
          XCRD(2)=XMIN+(REAL(I  )/RBNS)*(XMAX-XMIN)
          YCRD(2)=0.
          XCRD(3)=XMIN+(REAL(I  )/RBNS)*(XMAX-XMIN)
          YCRD(3)=YCEN(I)
          XCRD(4)=XMIN+(REAL(I-1)/RBNS)*(XMAX-XMIN)
          YCRD(4)=YCEN(I)
          CALL GFA (4,XCRD,YCRD)
  126   CONTINUE
C
        CALL PLCHHQ (CFUX(.8),CFUY(.75),
     +                                'The triangle sizes vary widely.',
     +                                                       .012,0.,1.)
C
        CALL FRAME
C
C Compute the areas of all the triangles of the derived grid and find
C the minimum, maximum, and average of the areas.  We actually count
C each triangle three times, but that's all right ...
C
        NTDG=0
C
        BMIN=1.E36
        BMAX=-1.E36
        BSUM=0.
C
        DO 128 I=1,2562
          IF (IADJ(I,6).LT.0) THEN
            NOJS=5
          ELSE
            NOJS=6
          END IF
          DO 127 J=1,NOJS
            JM1=MOD(J+NOJS-2,NOJS)+1
            A=SQRT((XCOV(     I     )-XCOV(IADJ(I,JM1)))**2+
     +             (YCOV(     I     )-YCOV(IADJ(I,JM1)))**2+
     +             (ZCOV(     I     )-ZCOV(IADJ(I,JM1)))**2)
            B=SQRT((XCOV(     I     )-XCOV(IADJ(I,J  )))**2+
     +             (YCOV(     I     )-YCOV(IADJ(I,J  )))**2+
     +             (ZCOV(     I     )-ZCOV(IADJ(I,J  )))**2)
            C=SQRT((XCOV(IADJ(I,JM1))-XCOV(IADJ(I,J  )))**2+
     +             (YCOV(IADJ(I,JM1))-YCOV(IADJ(I,J  )))**2+
     +             (ZCOV(IADJ(I,JM1))-ZCOV(IADJ(I,J  )))**2)
            S=.5*(A+B+C)
            NTDG=NTDG+1
            BREA(NTDG)=SQRT(S*(S-A)*(S-B)*(S-C))
            BMIN=MIN(BMIN,BREA(NTDG))
            BMAX=MAX(BMAX,BREA(NTDG))
            BSUM=BSUM+BREA(NTDG)
  127     CONTINUE
  128   CONTINUE
C
        BVRG=BSUM/REAL(NTDG)
C
C Print the minimum, maximum, and average areas.
C
        PRINT * , ' '
        PRINT '(''DERIVED GRID  OF "ctex03" '')'
        PRINT * , ' '
        PRINT '(''TRIANGLES:    '',     I8)' , NTDG/3
        PRINT '(''MINIMUM AREA: '',1PE16.8)' , BMIN
        PRINT '(''MAXIMUM AREA: '',1PE16.8)' , BMAX
        PRINT '(''AVERAGE AREA: '',1PE16.8)' , BVRG
        PRINT '(''EXPECTED AVG: '',1PE16.8)' , 4.*PI/REAL(NTDG/3)
C
C Draw a histogram showing the distribution of areas.
C
        DO 129 I=1,NBNS
          XCEN(I)=XMIN+(REAL(I-1)+.5)*(XMAX-XMIN)/RBNS
          YCEN(I)=0.
          ZERO(I)=0.
  129   CONTINUE
C
        DO 130 I=1,NTDG
          K=MAX(1,MIN(NBNS,1+INT(RBNS*(BREA(I)/BVRG-XMIN)/
     +                                                  (XMAX-XMIN))))
          YCEN(K)=YCEN(K)+1.
  130   CONTINUE
C
        DO 131 I=1,NBNS
          YCEN(I)=YCEN(I)/REAL(NTDG)
  131   CONTINUE
C
        LABL(35:35)='3'
        WRITE (LABL(40:43),'(I4)') NBNS
C
        CALL EZXY   (XCEN,ZERO,NBNS,LABL)
C
        DO 132 I=1,NBNS
          XCRD(1)=XMIN+(REAL(I-1)/RBNS)*(XMAX-XMIN)
          YCRD(1)=0.
          XCRD(2)=XMIN+(REAL(I  )/RBNS)*(XMAX-XMIN)
          YCRD(2)=0.
          XCRD(3)=XMIN+(REAL(I  )/RBNS)*(XMAX-XMIN)
          YCRD(3)=YCEN(I)
          XCRD(4)=XMIN+(REAL(I-1)/RBNS)*(XMAX-XMIN)
          YCRD(4)=YCEN(I)
          CALL GFA (4,XCRD,YCRD)
  132   CONTINUE
C
        CALL PLCHHQ (CFUX(.8),CFUY(.75),
     +                  'The triangle sizes are relatively consistent.',
     +                                                       .012,0.,1.)
C
        CALL FRAME
C
C Close GKS.
C
        CALL GDAWK (IWID)
        CALL GCLWK (IWID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END


      SUBROUTINE FACTOR (ITBF,IFOI,NFOI)
C
        DIMENSION IFOI(32)
C
C Move the absolute value of the number to be factored to a local
C variable we can modify.
C
        NTBF=ABS(ITBF)
C
C Handle the values 1, 2, and 3 specially.
C
        IF (NTBF.LE.3) GO TO 103
C
C Zero the count of factors found.
C
        NFOI=0
C
C Look for prime factors.
C
        ILIM=INT(SQRT(REAL(NTBF)+.5))
C
        DO 102 I=2,ILIM
  101     IF (MOD(NTBF,I).EQ.0) THEN
            IF (NFOI.LT.32) THEN
              NFOI=NFOI+1
              IFOI(NFOI)=I
            END IF
            NTBF=NTBF/I
            IF (NTBF.EQ.1) RETURN
            GO TO 101
          END IF
  102   CONTINUE
C
C Output the remaining factor.
C
  103   IF (NFOI.LT.32) THEN
          NFOI=NFOI+1
          IFOI(NFOI)=NTBF
        END IF
C
        RETURN
C
      END


      SUBROUTINE DRSGCR (ALAT,ALON,BLAT,BLON)
C
C (DRSGCR = DRaw Shortest Great Circle Route)
C
C This routine draws the shortest great circle route joining two points
C on the globe.
C
        PARAMETER (MPTS=181,SIZE=1.)
C
        DIMENSION QLAT(MPTS),QLON(MPTS)
C
        NPTS=MAX(1,MIN(MPTS,INT(ADSGCR(ALAT,ALON,BLAT,BLON)/SIZE)))
C
        CALL MAPGCI (ALAT,ALON,BLAT,BLON,NPTS,QLAT,QLON)
C
        CALL MAPIT (ALAT,ALON,0)
C
        DO 101 I=1,NPTS
          CALL MAPIT (QLAT(I),QLON(I),1)
  101   CONTINUE
C
        CALL MAPIT (BLAT,BLON,2)
C
        CALL MAPIQ
C
        RETURN
C
      END


      FUNCTION ADSGCR (ALAT,ALON,BLAT,BLON)
C
C (ADSGCR = Angle in Degrees along Shortest Great Circle Route)
C
C This function returns the shortest great circle distance, in degrees,
C between two points, A and B, on the surface of the globe.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
        XCOA=COS(DTOR*ALAT)*COS(DTOR*ALON)
        YCOA=COS(DTOR*ALAT)*SIN(DTOR*ALON)
        ZCOA=SIN(DTOR*ALAT)
C
        XCOB=COS(DTOR*BLAT)*COS(DTOR*BLON)
        YCOB=COS(DTOR*BLAT)*SIN(DTOR*BLON)
        ZCOB=SIN(DTOR*BLAT)
C
        ADSGCR=2.*RTOD*ASIN(SQRT((XCOA-XCOB)**2+
     +                           (YCOA-YCOB)**2+
     +                           (ZCOA-ZCOB)**2)/2.)
C
        RETURN
C
      END


      SUBROUTINE LBLPNT (XCVI,YCVI,ZCVI)
        DIMENSION XCVI(12),YCVI(12),ZCVI(12)
        CHARACTER*2 CTMP
        DATA RTOD / 57.2957795130823D0 /
        DO 101 I=1,12
          RLAT=RTOD*ASIN (ZCVI(I))
          RLON=RTOD*ATAN2(YCVI(I),XCVI(I))
          CALL MAPTRA (RLAT,RLON,XCOL,YCOL)
          IF (XCOL.NE.1.E12) THEN
            WRITE (CTMP,'(I2)') I
            CALL PLCHHQ (XCOL,YCOL,CTMP(MDIFNB(CTMP):MDILNB(CTMP)),
     +                                                  .018,0.,0.)
          END IF
  101   CONTINUE
        RETURN
      END


      SUBROUTINE AGPWRT (XPOS,YPOS,CHRS,NCHS,ISIZ,IORI,ICEN)
C
        CHARACTER*(*) CHRS
C
C This subroutine replaces the default version in the NCAR Graphics
C libraries (which just calls PWRIT).
C
C If IFLG is zero, labels are drawn using calls to the SPPS routine
C PWRIT (the default behavior of AUTOGRAPH); otherwise, labels are
C drawn using calls to PLCHHQ and, in addition, the constant-spacing
C parameter of PLCHHQ is used to achieve a more consistent appearance
C for numeric labels (which are distinguished from the others by virtue
C of the fact that they are drawn with a non-zero centering flag).
C
        COMMON /PSSFLG/ IFLG
C
C Do it.
C
        IF (IFLG.EQ.0) THEN
          CALL PWRIT (XPOS,YPOS,CHRS,NCHS,ISIZ,IORI,ICEN)
        ELSE
          CALL PCGETR ('CS - CONSTANT SPACING FLAG',CSFL)
          IF (ICEN.NE.0) THEN
            CALL PCSETR ('CS - CONSTANT SPACING FLAG',1.25)
          ELSE
            CALL PCSETR ('CS - CONSTANT SPACING FLAG',0.)
          END IF
          CALL PLCHHQ (XPOS,YPOS,CHRS(1:NCHS),
     +                 .8*REAL(ISIZ),REAL(IORI),REAL(ICEN))
          CALL PCSETR ('CS - CONSTANT SPACING FLAG',CSFL)
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE FPSGCR (ALAT,ALON,BLAT,BLON,FRCT,FLAT,FLON)
C
C (FPSGCR = Find Point on Shortest Great Circle Route)
C
        REAL    ALAT,ALON,BLAT,BLON,FRCT,FLAT,FLON
C
C This routine, given the latitudes and longitudes of two points A and
C B on the surface of the globe and a fraction FRCT, between 0. and 1.,
C interpolates a point F along the shortest great circle route joining
C A to B such that the distance from A to F, divided by the distance
C from A to B, is equal to FRCT, and returns its latitude and longitude
C in FLAT and FLON.
C
C Define the constants used to convert from degrees to radians and
C vice-versa.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Compute the X, Y, and Z coordinates (on a unit sphere) of the point B.
C
        CALL LLPXYZ (BLAT,BLON,XCPB,YCPB,ZCPB)
C
C Rotating about the Z axis by the angle -ALON would carry A into the XZ
C plane.
C
        CALL ROTXYZ (3,-ALON,XCPB,YCPB,ZCPB)
C
C Then, rotating about the Y axis by the angle ALAT would carry A into
C the point on the X axis with coordinates (1,0,0).
C
        CALL ROTXYZ (2,ALAT,XCPB,YCPB,ZCPB)
C
C Then, rotating about the X axis by the angle ALPH = -ATAN(ZCPB/YCPB)
C would leave the position of A unchanged but carry B into a point in
C the XY plane.
C
        IF (ZCPB.NE.0..OR.YCPB.NE.0.) THEN
          ALPH=-RTOD*ATAN2(ZCPB,YCPB)
        ELSE
          ALPH=0.
        END IF
C
        CALL ROTXYZ (1,ALPH,XCPB,YCPB,ZCPB)
C
C The angle BETA from A to B can now be computed easily.
C
        IF (XCPB.NE.0..OR.YCPB.NE.0.) THEN
          BETA=ATAN2(YCPB,XCPB)
        ELSE
          BETA=0.
        END IF
C
C Interpolate a point at the desired position between the points A and
C B, map it back to its original position on the great circle route from
C A to B, and get its latitude and longitude to return to the caller.
C
        GAMA=FRCT*BETA
C
        XCPF=COS(GAMA)
        YCPF=SIN(GAMA)
        ZCPF=0.
C
        CALL ROTXYZ (1,-ALPH,XCPF,YCPF,ZCPF)
        CALL ROTXYZ (2,-ALAT,XCPF,YCPF,ZCPF)
        CALL ROTXYZ (3, ALON,XCPF,YCPF,ZCPF)
C
        CALL XYZLLP (XCPF,YCPF,ZCPF,FLAT,FLON)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE LLPXYZ (RLAT,RLON,RVOX,RVOY,RVOZ)
C
C (LLPXYZ = Lat/Lon Position to XYZ coordinates)
C
C Given the latitude and longitude of a point on the globe, return its
C X, Y, and Z coordinates.
C
        DATA DTOR / .017453292519943 /
C
        RVOX=COS(DTOR*RLAT)*COS(DTOR*RLON)
        RVOY=COS(DTOR*RLAT)*SIN(DTOR*RLON)
        RVOZ=SIN(DTOR*RLAT)
C
        RETURN
C
      END


      SUBROUTINE XYZLLP (RVOX,RVOY,RVOZ,RLAT,RLON)
C
C (XYZLLP = XYZ coordinates to Lat/Lon Position)
C
C Given the X, Y, and Z coordinates of a point on the globe, return its
C latitude and longitude.
C
        DATA RTOD / 57.2957795130823 /
C
        RLAT=RTOD*ASIN(RVOZ)
C
        IF (RVOX.NE.0.OR.RVOY.NE.0.) THEN
          RLON=RTOD*ATAN2(RVOY,RVOX)
        ELSE
          RLON=0.
        END IF
C
        RETURN
C
      END


      SUBROUTINE ROTXYZ (IAXS,ANGL,XCRD,YCRD,ZCRD)
C
C (ROTXYZ = ROTate a point defined by its X, Y, and Z coordinates)
C
C This is a modified version of a routine in the NCAR Graphics package,
C which is used in some of the examples.  It rotates the point having
C coordinates (XCRD,YCRD,ZCRD) by an angle ANGL about the axis specified
C by IAXS (1 for the X axis, 2 for the Y axis, 3 for the Z axis).
C
C One assumes a right-handed system with X, Y, and Z axes.  Rotating by
C an angle "a" about the X axis maps the point (x,y,z) into the point
C (x',y',z'), where
C
C       x' = x
C       y' = y cos(a) - z sin(a)
C       z' = z cos(a) + y sin(a)
C
C A positive value of "a" represents rotation in the direction from the
C Y axis to the Z axis.
C
C Similarly, rotating by an angle "a" about the Y axis maps the point
C (x,y,z) into the point (x',y',z'), where
C
C       x' = x cos(a) + z sin(a)
C       y' = y
C       z' = z cos(a) - x sin(a)
C
C A positive value of "a" represents rotation in the direction from the
C Z axis to the X axis.
C
C Rotating by an angle "a" about the Z axis maps the point (x,y,z) into
C the point (x',y',z'), where
C
C       x' = x cos(a) - y sin(a)
C       y' = y cos(a) + x sin(a)
C       y' = y
C
C A positive value of "a" represents rotation in the direction from the
C X axis to the Y axis.
C
C Define a multiplicative constant to convert from degrees to radians.
C
        DATA DTOR / .017453292519943 /
C
C Trigonometry.
C
        SINA=SIN(DTOR*ANGL)
        COSA=COS(DTOR*ANGL)
C
        XTMP=XCRD
        YTMP=YCRD
        ZTMP=ZCRD
C
        IF (IAXS.EQ.1) THEN
          YCRD=YTMP*COSA-ZTMP*SINA
          ZCRD=ZTMP*COSA+YTMP*SINA
        ELSE IF (IAXS.EQ.2) THEN
          XCRD=XTMP*COSA+ZTMP*SINA
          ZCRD=ZTMP*COSA-XTMP*SINA
        ELSE
          XCRD=XTMP*COSA-YTMP*SINA
          YCRD=YTMP*COSA+XTMP*SINA
        END IF
C
        RETURN
C
      END
