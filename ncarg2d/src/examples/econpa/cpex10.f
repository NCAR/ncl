
        PROGRAM CPEX10
C
C This program shows a view of the area around Boulder, Colorado, as
C seen from a satellite directly above Washington, D.C.  Within a
C circular area near Boulder, color-filled contour bands are shown;
C state boundaries are drawn in black over them.  Outside that area,
C the surface of the globe is shaded gray, state boundaries are drawn
C in white, and lines of latitude and longitude are drawn in a light
C gray.  Two-letter mnemonics are used to identify the states; each
C is written using new mapping capabilities of the package PLOTCHAR.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Define parameters specifying the lengths of the two area maps and
C the real and integer workspaces that are used.
C
        PARAMETER (LAM1=2000,LAM2=30000,LRWK=200,LIWK=100)
C
C Declare an array in which to put an area map that will be used to
C distinguish the circular area near Boulder from the rest of the globe
C and another array in which to put an area map that will be used to
C color contour bands within that area.  This program could be written
C using a single area map; it is being done this way for illustrative
C purposes.
C
        DIMENSION IAM1(LAM1),IAM2(LAM2)
C
C Declare scratch arrays required by the routines ARSCAM, MAPBLM, and
C MAPGRM.
C
        DIMENSION XCRA(1000),YCRA(1000),IAIA(10),IAGI(10)
C
C Declare arrays in which to get the latitudes and longitudes of points
C defining a circle around Boulder on the surface of the globe.
C
        DIMENSION CLAT(100),CLON(100)
C
C Declare arrays in which to get the latitudes and longitudes of points
C defining a star over Boulder.
C
        DIMENSION XLAT(6),XLON(6)
C
C Define an array in which to generate some dummy data to contour and
C workspace arrays required by CONPACK.
C
        DIMENSION ZDAT(41,41),RWRK(LRWK),IWRK(LIWK)
C
C Declare external the routines that will draw the masked grid lines
C and geographical outlines and the routine that will color the contour
C bands in the circular area around Boulder.
C
        EXTERNAL DRAWGL,DRAWGO,COLRCB
C
C Declare the common block in which the angle at which the label of a
C point on the globe is to be written and the latitude and longitude
C of the point being labelled are transmitted to the routine PCMPXY, in
C the package PLOTCHAR.
C
        COMMON /PCMP04/ PANG,PLAT,PLON
        SAVE   /PCMP04/
C
C Define arrays to hold a list of state names, two-character mnemonics
C for the states, and the latitude and longitude of a point where the
C mnemonic may be placed to label the state.
C
        CHARACTER*14 SNAM(50)
C
        CHARACTER*2 SMNE(50)
C
        DIMENSION SLAT(50),SLON(50)
C
C Define the state-labelling data.
C
        DATA SNAM( 1),SMNE( 1),SLAT( 1), SLON( 1)
     +                       / 'Alabama       ' , 'AL' , 33.0 ,  -86.5 /
        DATA SNAM( 2),SMNE( 2),SLAT( 2), SLON( 2)
     +                       / 'Alaska        ' , 'AK' , 65.0 , -152.0 /
        DATA SNAM( 3),SMNE( 3),SLAT( 3), SLON( 3)
     +                       / 'Arizona       ' , 'AZ' , 34.7 , -111.5 /
        DATA SNAM( 4),SMNE( 4),SLAT( 4), SLON( 4)
     +                       / 'Arkansas      ' , 'AR' , 35.0 ,  -92.5 /
        DATA SNAM( 5),SMNE( 5),SLAT( 5), SLON( 5)
     +                       / 'California    ' , 'CA' , 37.5 , -120.5 /
        DATA SNAM( 6),SMNE( 6),SLAT( 6), SLON( 6)
     +                       / 'Colorado      ' , 'CO' , 39.0 , -105.8 /
        DATA SNAM( 7),SMNE( 7),SLAT( 7), SLON( 7)
     +                       / 'Connecticut   ' , 'CT' , 41.6 ,  -72.6 /
        DATA SNAM( 8),SMNE( 8),SLAT( 8), SLON( 8)
     +                       / 'Delaware      ' , 'DE' , 39.0 ,  -75.5 /
        DATA SNAM( 9),SMNE( 9),SLAT( 9), SLON( 9)
     +                       / 'Florida       ' , 'FL' , 28.5 ,  -82.0 /
        DATA SNAM(10),SMNE(10),SLAT(10), SLON(10)
     +                       / 'Georgia       ' , 'GA' , 32.5 ,  -83.0 /
        DATA SNAM(11),SMNE(11),SLAT(11), SLON(11)
     +                       / 'Hawaii        ' , 'HI' , 20.0 , -157.0 /
        DATA SNAM(12),SMNE(12),SLAT(12), SLON(12)
     +                       / 'Idaho         ' , 'ID' , 43.5 , -114.0 /
        DATA SNAM(13),SMNE(13),SLAT(13), SLON(13)
     +                       / 'Illinois      ' , 'IL' , 40.2 ,  -89.2 /
        DATA SNAM(14),SMNE(14),SLAT(14), SLON(14)
     +                       / 'Indiana       ' , 'IN' , 40.0 ,  -86.0 /
        DATA SNAM(15),SMNE(15),SLAT(15), SLON(15)
     +                       / 'Iowa          ' , 'IA' , 42.0 ,  -93.2 /
        DATA SNAM(16),SMNE(16),SLAT(16), SLON(16)
     +                       / 'Kansas        ' , 'KS' , 38.5 ,  -98.2 /
        DATA SNAM(17),SMNE(17),SLAT(17), SLON(17)
     +                       / 'Kentucky      ' , 'KY' , 37.4 ,  -84.5 /
        DATA SNAM(18),SMNE(18),SLAT(18), SLON(18)
     +                       / 'Louisiana     ' , 'LA' , 31.2 ,  -92.5 /
        DATA SNAM(19),SMNE(19),SLAT(19), SLON(19)
     +                       / 'Maine         ' , 'ME' , 45.5 ,  -69.0 /
        DATA SNAM(20),SMNE(20),SLAT(20), SLON(20)
     +                       / 'Maryland      ' , 'MD' , 39.2 ,  -76.5 /
        DATA SNAM(21),SMNE(21),SLAT(21), SLON(21)
     +                       / 'Massachusetts ' , 'MA' , 42.3 ,  -72.0 /
        DATA SNAM(22),SMNE(22),SLAT(22), SLON(22)
     +                       / 'Michigan      ' , 'MI' , 44.0 ,  -85.0 /
        DATA SNAM(23),SMNE(23),SLAT(23), SLON(23)
     +                       / 'Minnesota     ' , 'MN' , 46.0 ,  -94.5 /
        DATA SNAM(24),SMNE(24),SLAT(24), SLON(24)
     +                       / 'Mississippi   ' , 'MS' , 32.5 ,  -89.5 /
        DATA SNAM(25),SMNE(25),SLAT(25), SLON(25)
     +                       / 'Missouri      ' , 'MO' , 38.5 ,  -92.5 /
        DATA SNAM(26),SMNE(26),SLAT(26), SLON(26)
     +                       / 'Montana       ' , 'MT' , 47.0 , -109.5 /
        DATA SNAM(27),SMNE(27),SLAT(27), SLON(27)
     +                       / 'Nebraska      ' , 'NE' , 41.5 ,  -99.5 /
        DATA SNAM(28),SMNE(28),SLAT(28), SLON(28)
     +                       / 'Nevada        ' , 'NV' , 39.8 , -117.0 /
        DATA SNAM(29),SMNE(29),SLAT(29), SLON(29)
     +                       / 'New Hampshire ' , 'NH' , 43.2 ,  -71.6 /
        DATA SNAM(30),SMNE(30),SLAT(30), SLON(30)
     +                       / 'New Jersey    ' , 'NJ' , 39.7 ,  -74.5 /
        DATA SNAM(31),SMNE(31),SLAT(31), SLON(31)
     +                       / 'New Mexico    ' , 'NM' , 34.7 , -106.0 /
        DATA SNAM(32),SMNE(32),SLAT(32), SLON(32)
     +                       / 'New York      ' , 'NY' , 43.0 ,  -75.0 /
        DATA SNAM(33),SMNE(33),SLAT(33), SLON(33)
     +                       / 'North Carolina' , 'NC' , 35.5 ,  -79.5 /
        DATA SNAM(34),SMNE(34),SLAT(34), SLON(34)
     +                       / 'North Dakota  ' , 'ND' , 47.5 , -100.5 /
        DATA SNAM(35),SMNE(35),SLAT(35), SLON(35)
     +                       / 'Ohio          ' , 'OH' , 40.2 ,  -82.5 /
        DATA SNAM(36),SMNE(36),SLAT(36), SLON(36)
     +                       / 'Oklahoma      ' , 'OK' , 35.6 ,  -97.5 /
        DATA SNAM(37),SMNE(37),SLAT(37), SLON(37)
     +                       / 'Oregon        ' , 'OR' , 44.0 , -120.2 /
        DATA SNAM(38),SMNE(38),SLAT(38), SLON(38)
     +                       / 'Pennsylvania  ' , 'PA' , 40.8 ,  -77.6 /
        DATA SNAM(39),SMNE(39),SLAT(39), SLON(39)
     +                       / 'Rhode Island  ' , 'RI' , 41.7 ,  -71.5 /
        DATA SNAM(40),SMNE(40),SLAT(40), SLON(40)
     +                       / 'South Carolina' , 'SC' , 34.0 ,  -80.5 /
        DATA SNAM(41),SMNE(41),SLAT(41), SLON(41)
     +                       / 'South Dakota  ' , 'SD' , 44.5 , -100.5 /
        DATA SNAM(42),SMNE(42),SLAT(42), SLON(42)
     +                       / 'Tennessee     ' , 'TN' , 36.0 ,  -86.5 /
        DATA SNAM(43),SMNE(43),SLAT(43), SLON(43)
     +                       / 'Texas         ' , 'TX' , 32.0 , -100.0 /
        DATA SNAM(44),SMNE(44),SLAT(44), SLON(44)
     +                       / 'Utah          ' , 'UT' , 39.5 , -111.5 /
        DATA SNAM(45),SMNE(45),SLAT(45), SLON(45)
     +                       / 'Vermont       ' , 'VT' , 44.2 ,  -72.5 /
        DATA SNAM(46),SMNE(46),SLAT(46), SLON(46)
     +                       / 'Virginia      ' , 'VA' , 37.6 ,  -78.6 /
        DATA SNAM(47),SMNE(47),SLAT(47), SLON(47)
     +                       / 'Washington    ' , 'WA' , 47.5 , -120.5 /
        DATA SNAM(48),SMNE(48),SLAT(48), SLON(48)
     +                       / 'West Virginia ' , 'WV' , 38.5 ,  -80.8 /
        DATA SNAM(49),SMNE(49),SLAT(49), SLON(49)
     +                       / 'Wisconsin     ' , 'WI' , 44.5 ,  -89.5 /
        DATA SNAM(50),SMNE(50),SLAT(50), SLON(50)
     +                       / 'Wyoming       ' , 'WY' , 43.0 , -107.5 /
C
C Define multiplicative constants to convert from degrees to radians
C and from radians to degrees.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Get the latitudes and longitudes of 100 points defining a circle on
C the globe centered at the point (40.0,-105.3) - the approximate
C latitude and longitude of Boulder, Colorado - and having a radius
C of 7 degrees.
C
        CALL NGGCOG (40.0,-105.3,7.0,CLAT,CLON,100)
C
C Generate some dummy data to contour later.
C
        DO 102 I=1,41
          X=REAL(I-1)/40.
          DO 101 J=1,41
            Y=REAL(J-1)/40.
            ZDAT(I,J)=X*X+Y*Y+X*Y+SIN(9.*X)*COS(9.*Y)
  101     CONTINUE
  102   CONTINUE
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off clipping.
C
        CALL GSCLIP (0)
C
C Turn on solid fill.
C
        CALL GSFAIS (1)
C
C Define some colors to use.  Color index 2 is for grid lines far from
C Boulder, color index 3 is for geographical objects near Boulder, and
C color index 4 is for geographical objects far from Boulder.  Color
C index 5 is used for that part of the earth which is not within the
C circle around Boulder.  Color index 6 is used for the star over
C Boulder.  Color indices 101 through 116 are to be used for contour
C bands in the area near Boulder; they are evenly distributed between
C pure red and pure blue.
C
        CALL GSCR   (IWKID,0,0.,0.,0.)
        CALL GSCR   (IWKID,1,1.,1.,1.)
        CALL GSCR   (IWKID,2,.6,.6,.6)
        CALL GSCR   (IWKID,3,0.,0.,0.)
        CALL GSCR   (IWKID,4,1.,1.,1.)
        CALL GSCR   (IWKID,5,.4,.4,.4)
        CALL GSCR   (IWKID,6,1.,1.,0.)
C
        DO 103 I=101,116
          CALL GSCR (IWKID,I,REAL(116-I)/15.,0.,REAL(I-101)/15.)
  103   CONTINUE
C
C Put a label at the top of the plot.
C
        CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
        CALL PLCHHQ (.5,.975,
     +         ':F25:SATELLITE VIEW OF CONTOUR BANDS IN A LIMITED AREA',
     +                                                       .018,0.,0.)
C
C Tell EZMAP where to put the map on the plotter frame.
C
        CALL MAPPOS (.05,.95,.05,.95)
C
C Tell EZMAP to use the view from a satellite above Washington, D.C.
C The basic satellite-view projection is rotated clockwise by 75
C degrees so that the direction a little north of due west is toward
C the top of the projection.
C
        CALL MAPROJ ('SV',38.,-76.,75.)
C
C Tell EZMAP how far the satellite is from the center of earth and
C make it look in a direction about 7/8 of the way between looking
C straight down and looking directly at the horizon.  We end up
C looking roughly in the direction of Boulder.
C
        DFCE=1.3
C
        CALL MPSETR ('SA',DFCE)
C
        CALL MPSETR ('S1',7.*RTOD*ASIN(1./DFCE)/8.)
C
C Set the parameter 'S2' so that the line of sight is displaced toward
C the top of the basic satellite view - that is to say, in the direction
C a little north of due west that the setting of ROTA implies - by the
C angle specified by 'S1'.
C
        CALL MPSETR ('S2',90.)
C
C Tell EZMAP the satellite has a total field of view of 40 degrees.
C
        CALL MAPSET ('AN',20.,20.,20.,20.)
C
C Tell EZMAP to use the outline with political and state boundaries.
C
        CALL MPSETC ('OU','PS')
C
C Tell EZMAP to use a one-degree grid.
C
        CALL MPSETI ('GR',1)
C
C Initialize EZMAP.
C
        CALL MAPINT
C
C Tell CONPACK not to call SET (because MAPINT has already done it).
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Tell CONPACK to map the contour lines using EZMAP.
C
        CALL CPSETI ('MAP - MAPPING FLAG.',1)
C
C Tell CONPACK what longitudes the minimum and maximum values of the
C first array index correspond to.
C
        CALL CPSETR ('XC1 - X COORDINATE AT I=1',-115.)
        CALL CPSETR ('XCM - X COORDINATE AT I=M', -95.)
C
C Tell CONPACK what latitudes the minimum and maximum values of the
C second array index correspond to.
C
        CALL CPSETR ('YC1 - Y COORDINATE AT J=1',32.)
        CALL CPSETR ('YCN - Y COORDINATE AT J=N',48.)
C
C Tell CONPACK to use exactly 15 contour levels, splitting the range
C from the minimum value to the maximum value into 16 equal bands.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',-15)
C
C Tell CONPACK where the data to be contoured are, where the real and
C integer workspaces are, and how big each array is.
C
        CALL CPRECT (ZDAT,41,41,41,RWRK,LRWK,IWRK,LIWK)
C
C Initialize the first area-map array.
C
        CALL ARINAM (IAM1,LAM1)
C
C Put the projection of the circle around Boulder into the area map.
C The information goes into edge group 1.  The area inside the projected
C circle is characterized as area 1 and the area outside the circle as
C area 2.
C
        CALL MAPITA (CLAT(1),CLON(1),0,IAM1,1,1,2)
C
        DO 104 I=2,100
          CALL MAPITA (CLAT(I),CLON(I),1,IAM1,1,1,2)
  104   CONTINUE
C
        CALL MAPIQA (IAM1,1,1,2)
C
C Copy the information from the first area-map array to the second one.
C Note that the routine we use to do this, despite the "move" implied
C by its name, can actually be used in this way.
C
        CALL ARMVAM (IAM1,IAM2,LAM2)
C
C Add to the second area map the limb line and the perimeter for the
C satellite-view projection.  This is done by temporarily using no
C outline dataset, so that MAPBLA will put only the lines we want into
C the area map.  The edges will go in edge group 1.
C
        CALL MPSETC ('OU','NO')
        CALL MAPBLA (IAM2)
        CALL MPSETC ('OU','PS')
C
C Add to the second area map the contour lines for the area near
C Boulder.  They will go in edge group 3.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAM2)
C
C Scan the second area map to color the contour bands near Boulder.
C
        CALL ARSCAM (IAM2,XCRA,YCRA,1000,IAIA,IAGI,10,COLRCB)
C
C Double the line width.
C
        CALL GSLWSC (2.)
C
C Draw a masked latitude/longitude grid and masked outlines.
C
        CALL MAPGRM (IAM1,XCRA,YCRA,1000,IAIA,IAGI,10,DRAWGL)
        CALL MAPBLM (IAM1,XCRA,YCRA,1000,IAIA,IAGI,10,DRAWGO)
C
C Set the current polyline color index to draw a black line.
C
        CALL GSPLCI (0)
C
C Draw the circle around Boulder.
C
        CALL MAPIT (CLAT(1),CLON(1),0)
C
        DO 105 I=2,100
          CALL MAPIT (CLAT(I),CLON(I),1)
  105   CONTINUE
C
        CALL MAPIQ
C
C Set the current polyline color index to draw a yellow line.
C
        CALL GSPLCI (6)
C
C Put a star at the position of Boulder.
C
        CALL NGGSOG (40.,-105.,.25,XLAT,XLON)
C
        CALL MAPIT (XLAT(1),XLON(1),0)
C
        DO 106 I=2,6
          CALL MAPIT (XLAT(I),XLON(I),1)
  106   CONTINUE
C
        CALL MAPIQ
C
C Label the states using two-character mnemonics for them.
C
        CALL PCSETI ('MAP',4)
        CALL PCSETR ('ORV',1.E12)
        PANG=45.
C
        DO 107 I=1,50
          PLAT=SLAT(I)
          PLON=SLON(I)
          CALL PLCHHQ (0.,0.,SMNE(I),.5,0.,0.)
  107   CONTINUE
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
C Compute or get information about the amount of space used in each
C area-map array and in the real and integer workspace arrays and
C print it.
C
        CALL CPGETI ('RWU - INTEGER WORKSPACE USED',IRWS)
        CALL CPGETI ('IWU - INTEGER WORKSPACE USED',IIWS)
C
        PRINT * , 'NUMBER OF WORDS USED IN REAL WORKSPACE:    ',IRWS
        PRINT * , 'NUMBER OF WORDS USED IN INTEGER WORKSPACE: ',IIWS
        PRINT * , 'NUMBER OF WORDS USED IN AREA-MAP ARRAY 1:  ',
     +                                          LAM1-(IAM1(6)-IAM1(5)-1)
        PRINT * , 'NUMBER OF WORDS USED IN AREA-MAP ARRAY 2:  ',
     +                                          LAM2-(IAM2(6)-IAM2(5)-1)
C
C Done.
C
        STOP
C
      END



      SUBROUTINE DRAWGL (XCRA,YCRA,NCRA,IAAI,IAGI,NOGI)
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C The routine DRAWGL draws the polyline defined by the points
C ((XCRA(I),YCRA(I)),I=1,NCRA) using color index 2 if the area
C identifier relative to edge group 1 is a 2, implying that the
C polyline is outside the circle around Boulder, Colorado; otherwise,
C it does not draw the polyline at all.
C
C Find the area identifier relative to edge group 1.
C
        IAI1=-1
C
        DO 101 I=1,NOGI
          IF (IAGI(I).EQ.1) IAI1=IAAI(I)
  101   CONTINUE
C
C If the polyline is outside the circle, draw it.
C
        IF (IAI1.EQ.2) THEN
          CALL GSPLCI (2)
          CALL CURVE (XCRA,YCRA,NCRA)
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE DRAWGO (XCRA,YCRA,NCRA,IAAI,IAGI,NOGI)
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C The routine DRAWGO draws the polyline defined by the points
C ((XCRA(I),YCRA(I)),I=1,NCRA) using color index 3 if the area
C identifier relative to edge group 1 is a 1, implying that the
C polyline is inside the circle around Boulder, Colorado, and
C using color index 4, otherwise.
C
C Find the area identifier relative to edge group 1.
C
        IAI1=-1
C
        DO 101 I=1,NOGI
          IF (IAGI(I).EQ.1) IAI1=IAAI(I)
  101   CONTINUE
C
C Draw the polyline if the area identifier is a 1 or a 2, but not
C otherwise.
C
        IF (IAI1.EQ.1) THEN
          CALL GSPLCI (3)
        ELSE IF (IAI1.EQ.2) THEN
          CALL GSPLCI (4)
        END IF
C
        CALL CURVE (XCRA,YCRA,NCRA)
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE COLRCB (XCRA,YCRA,NCRA,IAAI,IAGI,NOGI)
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C The routine COLRCB colors the polygon defined by the points
C ((XCRA(I),YCRA(I)),I=1,NCRA) if and only if it is inside the
C circle around Boulder and it is a portion of one of the contour
C bands defined by the dummy data array.
C
C Find the area identifiers for the polygon relative to edge groups 1
C and 3.
C
        IAI1=-1
        IAI3=-1
C
        DO 101 I=1,NOGI
          IF (IAGI(I).EQ.1) IAI1=IAAI(I)
          IF (IAGI(I).EQ.3) IAI3=IAAI(I)
  101   CONTINUE
C
C Fill the polygon using a color implied by the contour level if it
C is inside the circle around Boulder and is part of a contour band.
C If it is outside the circle around Boulder, but is still on the
C globe, use color index 5 for it.
C
        IF (IAI1.EQ.1.AND.IAI3.GE.1.AND.IAI3.LE.16) THEN
          CALL GSFACI (100+IAI3)
          CALL GFA (NCRA-1,XCRA,YCRA)
        ELSE IF (IAI1.EQ.2) THEN
          CALL GSFACI (5)
          CALL GFA (NCRA-1,XCRA,YCRA)
        END IF
C
C Done.
C
        RETURN
C
      END
