
      PROGRAM CCPPOLE
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Parameterize the number of latitudes, the number of longitudes, the
C sizes of the real and integer workspaces, the size of the area map,
C and the size of the arrays used by ARSCAM for X and Y coordinates.
C
      PARAMETER (NLON=361,NLAT=181,LRWK=5000,LIWK=5000,LAMA=600000)
      PARAMETER (NCRA=20000)
C
C Declare the data array.
C
      DIMENSION ZDAT(NLON,NLAT)
C
C Declare the area map array.
C
      DIMENSION IAMA(LAMA)
C
C Declare the real and integer workspace arrays for CONPACK.
C
      DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare arrays for ARSCAM and MAPGRM to use in calls to COLRAM and
C COLRLL, respectively.  XCRA and YCRA hold X and Y coordinates; IAIA
C and IGIA hold area identifiers and group identifiers.
C
      DIMENSION XCRA(NCRA),YCRA(NCRA),IAIA(10),IGIA(10)
C
C Declare an array in which to retrieve aspect source flags.
C
      DIMENSION IASF(13)
C
C Declare a routine to color the areas represented by the area map.
C
      EXTERNAL COLRAM
C
C Declare a routine to draw contour lines over land only.
C
      EXTERNAL COLRCL
C
C Declare a routine to draw lat/lon lines over ocean only.
C
      EXTERNAL COLRLL
C
C Define the values to be used for GKS aspect source flags.
C
      DATA IASF / 13*1 /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
      CALL GSCLIP (0)
C
C Set all aspect source flags to "individual".
C
      CALL GSASF (IASF)
C
C Force solid fill.
C
      CALL GSFAIS (1)
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.  Colors 2 and 3 are used for alternate contour
C bands over ocean.  Colors 4 through 16 are used for contour bands
C over land.
C
      CALL GSCR (IWKID, 0,0.000,0.000,0.000)
      CALL GSCR (IWKID, 1,1.000,1.000,1.000)
      CALL GSCR (IWKID, 2,0.000,1.000,1.000)
      CALL GSCR (IWKID, 3,0.000,0.850,0.850)
      CALL GSCR (IWKID, 4,0.700,0.700,0.700)
      CALL GSCR (IWKID, 5,0.750,0.500,1.000)
      CALL GSCR (IWKID, 6,0.500,0.000,1.000)
      CALL GSCR (IWKID, 7,0.000,0.000,1.000)
      CALL GSCR (IWKID, 8,0.000,0.500,1.000)
      CALL GSCR (IWKID, 9,0.000,1.000,0.600)
      CALL GSCR (IWKID,10,0.000,1.000,0.000)
      CALL GSCR (IWKID,11,0.700,1.000,0.000)
      CALL GSCR (IWKID,12,1.000,1.000,0.000)
      CALL GSCR (IWKID,13,1.000,0.750,0.000)
      CALL GSCR (IWKID,14,1.000,0.380,0.380)
      CALL GSCR (IWKID,15,1.000,0.000,0.380)
      CALL GSCR (IWKID,16,1.000,0.000,0.000)
C
C Generate an array of test data.  It is important that the data should
C represent a continous function around the globe.  What is used here
C is a simple trigonometric function of latitude and longitude.
C
      ZMIN= 1.E36
      ZMAX=-1.E36
C
      DO 102 I=1,NLON
         RLON=.017453292519943*(-180.+360.*REAL(I-1)/REAL(NLON-1))
         DO 101 J=1,NLAT
            RLAT=.017453292519943*(-90.+180.*REAL(J-1)/REAL(NLAT-1))
            ZDAT(I,J)=.5*COS(8.*RLAT)+.25*COS(RLAT)*SIN(4.*RLON)
            ZMIN=MIN(ZMIN,ZDAT(I,J))
            ZMAX=MAX(ZMAX,ZDAT(I,J))
 101     CONTINUE
 102  CONTINUE
C
C Reduce the test data to the desired range.
C
      DO 104 I=1,NLON
         DO 103 J=1,NLAT
            ZDAT(I,J)=((ZDAT(I,J)-ZMIN)/(ZMAX-ZMIN))*130.-10.
 103     CONTINUE
 104  CONTINUE
C
C Put a label at the top of the plot.
C
      CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ (.5,.975,
     +     'CONTOUR BANDS ON A STEREOGRAPHIC PROJECTION',
     +     .02,0.,0.)
C
C Initialize the area map.
C
      CALL ARINAM (IAMA,LAMA)
C
C Tell Ezmap where to put the plot on the plotter frame.
C
      CALL MAPPOS (.03,.97,.01,.95)
C
C Tell Ezmap to use a stereographic projection.
C
      CALL MAPROJ ('ST',90.,0.,0.)
C
C Tell Ezmap to use a 90-degree distance to each of the four edges
C of the map.
C
      CALL MAPSET ('AN - ANGLES',90.,90.,90.,90.)
C
C Initialize Ezmap.
C
      CALL MAPINT
C
C Put continental outlines in the area map.
C
      CALL MAPBLA (IAMA)
C
C Tell CONPACK not to do the SET call (since it's already been done)
C and to use mapping function 1 (EZMAP background).
C
      CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
      CALL CPSETI ('MAP - MAPPING FLAG',1)
C
C Tell CONPACK what ranges of X and Y coordinates to send into the
C mapping function.
C
      CALL CPSETR ('XC1 - X COORDINATE AT I=1',-180.)
      CALL CPSETR ('XCM - X COORDINATE AT I=M',+180.)
      CALL CPSETR ('YC1 - Y COORDINATE AT J=1', -90.)
      CALL CPSETR ('YCN - Y COORDINATE AT J=N', +90.)
C
C Tell CONPACK exactly what contour levels to use.
C
      CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',1)
      CALL CPSETR ('CMN - CONTOUR LEVEL MINIMUM',0.)
      CALL CPSETR ('CMX - CONTOUR LEVEL MAXIMUM',110.)
      CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',10.)
C
C Tell CONPACK what to use as the out-of-range flag.  This is the
C value returned by the Ezmap routine MAPTRA for off-map points.
C
      CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Initialize the drawing of the contour plot.
C
      CALL CPRECT (ZDAT,NLON,NLON,NLAT,RWRK,LRWK,IWRK,LIWK)
C
C Add contour lines to the area map.
C
      CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
      CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAIA,IGIA,10,COLRAM)
C
C Switch the current polyline color to black.
C
      CALL GSPLCI (0)
C
C Outline the continents and draw lines of latitude and longitude over
C the ocean only.
C
      CALL MAPGRM (IAMA,XCRA,YCRA,NCRA,IAIA,IGIA,10,COLRLL)
      CALL MAPSTC ('OU - OUTLINE DATASET','CO')
      CALL MAPLOT
C
C Draw the contour lines over land only.
C
      CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,COLRCL)
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END

      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
      DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to color an area from an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
      IAI1=-1
      IAI3=-1
C
      DO 101 I=1,NAGI
         IF (IGIA(I).EQ.1) IAI1=IAIA(I)
         IF (IGIA(I).EQ.3) IAI3=IAIA(I)
 101  CONTINUE
C
C Color-fill the area, using two slightly-different shades of blue
C over water (so that the contour bands will be minimally visible)
C and brighter colors over land.
C
      IF (IAI1.GT.0) THEN
         IF (MAPACI(IAI1).EQ.1) THEN
            CALL GSFACI (2+MOD(IAI3,2))
            CALL GFA (NCRA-1,XCRA,YCRA)
         ELSE
            IF (IAI3.GE.1.AND.IAI3.LE.13) THEN
               CALL GSFACI (IAI3+3)
               CALL GFA (NCRA-1,XCRA,YCRA)
            END IF
         END IF
      END IF
C
C Done.
C
      RETURN
C
      END

      SUBROUTINE COLRCL (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
      DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
C This routine is called to draw a portion of a contour line which is
C wholly contained in some area of an area map.  Its coordinates are
C given by the NCRA coordinates in the arrays XCRA and YCRA.  For each
C I from 1 to NAGI, IAIA(I) is the area identifier of the area relative
C to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to groups 1 and 3.
C The first of these tells us whether the area is over land or water,
C and the second tells us what contour band the area is in.
C
      IAI1=-1
      IAI3=-1
C
      DO 101 I=1,NAGI
         IF (IGIA(I).EQ.1) IAI1=IAIA(I)
         IF (IGIA(I).EQ.3) IAI3=IAIA(I)
 101  CONTINUE
C
C Draw the line only if the area it is in is over land.
C
      IF (IAI1.GT.0.AND.MAPACI(IAI1).NE.1) CALL GPL (NCRA,XCRA,YCRA)
C
C Done.
C
      RETURN
C
      END

      SUBROUTINE COLRLL (XCRA,YCRA,NCRA,IAIA,IGIA,NAGI)
C
      DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C     
C This routine is called to draw a portion of a line of latitude or
C longitude which is wholly contained in some area of an area map.  Its
C coordinates are given by the NCRA coordinates in the arrays XCRA and
C YCRA.  For each I from 1 to NAGI, IAIA(I) is the area identifier of
C the area relative to the group whose group identifier is IGIA(I).
C
C Find the area identifier for the area relative to group 1, which will
C tell us whether the area is over land or water.
C
      IAI1=-1
C
      DO 101 I=1,NAGI
         IF (IGIA(I).EQ.1.AND.IAIA(I).GT.0) IAI1=IAIA(I)
 101  CONTINUE
C
C Draw the line only if it is over water.
C
      IF (IAI1.GT.0.AND.MAPACI(IAI1).EQ.1) CALL GPL (NCRA,XCRA,YCRA)
C
C Done.
C
      RETURN
C
      END
