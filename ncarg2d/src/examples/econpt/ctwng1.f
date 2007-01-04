

      PROGRAM CTWNG1
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
C The object of this program is to produce a set of plots illustrating
C the use of a triangular mesh derived from a SEAM grid on the surface
C of the globe (obtained from Houjun Wang):  Each face of a cube is
C divided into a 4-by-4 grid of squares (for a total of 6 x 4 x 4 = 96
C squares).  Each of those squares is further subdivided into an 7-by-7
C grid of rectangles, some of which are square (for a total of 96 x 7 x
C 7 = 4704 rectangles).  The rectangles are projected outward onto the
C surface of the globe to form a network of quadrilaterals, each of
C which is split into two triangles to form the triangular mesh.
C Routines developed for use with the geodesic mesh (method 2) are also
C used for this mesh.
C
C Selected frames are drawn for each of four different viewpoints.  If
C (CLAT,CLON) is the approximate position of the "center point" of the
C mesh (which is defined somewhat arbitrarily for certain meshes), the
C four viewpoints used are as follows: (CLAT+45,CLON), (CLAT-45,CLON),
C (CLAT+45,CLON+180), and (CLAT-45,CLON+180).
C
C At each of the four viewpoints, up to five different frames are drawn:
C
C   1) A frame showing the original rectangular grid on the globe.
C      This frame is drawn only if the parameter IMSH = 1 or 3.
C
C   2) A frame showing the triangular mesh on the globe.  This frame
C      is drawn only if the parameter IMSH = 2 or 3.
C
C   3) A frame showing simple contours on the globe.  This frame is
C      drawn only if the parameter ICON is non-zero.
C
C   4) A frame showing color-filled contour bands on the globe, drawn
C      using filled areas.  This frame is drawn only if the parameter
C      ICOL is non-zero.
C
C   5) A frame showing color-filled contour bands on the globe, drawn
C      using a cell array.  This frame is drawn only if the parameter
C      ICAP is non-zero.
C
C Define the parameter that says which frames showing the rectangular
C grid and/or the triangular mesh are to be drawn (frames 1 and 2):
C
C       PARAMETER (IMSH=0)  !  neither grid nor mesh
C       PARAMETER (IMSH=1)  !  simple rectangular grid only
C       PARAMETER (IMSH=2)  !  triangular mesh only
C       PARAMETER (IMSH=3)  !  both
C
        PARAMETER (IMSH=3)
C
C Define the parameter that says whether or not to draw simple contours
C (frame 3):
C
C       PARAMETER (ICON=0)  !  contours not drawn
C       PARAMETER (ICON=1)  !  contours drawn
C
        PARAMETER (ICON=1)
C
C Define the parameter that says whether or not to draw color-filled
C contours (frame 4):
C
C       PARAMETER (ICOL=0)  !  no color fill done
C       PARAMETER (ICOL=1)  !  color fill done
C
        PARAMETER (ICOL=1)
C
C Define the parameter that says whether or not to draw a cell array
C plot (frame 5):
C
C       PARAMETER (ICAP=0)  !  cell array plot not drawn
C       PARAMETER (ICAP=1)  !  cell array plot drawn
C
        PARAMETER (ICAP=1)
C
C To represent the triangular mesh, we use three singly-dimensioned
C arrays: RPNT holds points, IEDG holds edges, and ITRI holds triangles.
C The elements of each array form "nodes" having lengths as follows:
C
        PARAMETER (LOPN=5)
C
C The five elements of a point node are
C
C   1. the X coordinate of the point;
C   2. the Y coordinate of the point;
C   3. the Z coordinate of the point;
C   4. the field value at the point;
C   5. any additional value desired by the user.
C
        PARAMETER (LOEN=5)
C
C The five elements of an edge node are
C
C   1. the base index, in RPNT, of point 1 of the edge;
C   2. the base index, in RPNT, of point 2 of the edge;
C   3. the index, in ITRI, of the pointer to the edge in the triangle to
C      the left of the edge (-1 if there is no triangle to the left);
C   4. the index, in ITRI, of the pointer to the edge in the triangle to
C      the right of the edge (-1 if there is no triangle to the right);
C   5. a utility flag for use by algorithms that scan the structure.
C
C The "left" and "right" sides of an edge are defined as they would be
C by an observer standing on the globe at point 1 of the edge, looking
C toward point 2 of the edge.  It is possible, if there are "holes" in
C the mesh, that there will be no triangle to the left or to the right
C of an edge, but there must be a triangle on one side or the other.
C
        PARAMETER (LOTN=4)
C
C The four elements of a triangle node are
C
C   1. the base index, in IEDG, of edge 1 of the triangle;
C   2. the base index, in IEDG, of edge 2 of the triangle;
C   3. the base index, in IEDG, of edge 3 of the triangle;
C   4. a flag set non-zero to block use of the triangle, effectively
C      removing it from the mesh.
C
C The "base index" of a point node, an edge node, or a triangle node is
C always a multiple of the length of the node, to which can be added an
C offset to get the index of a particular element of the node.  For
C example, if I is the base index of a triangle of interest, ITRI(I+1)
C is its first element (the base index of its first edge).  Similarly,
C IEDG(ITRI(I+1)+2) is the base index of the second point of the first
C edge of the triangle with base index I, and RPNT(IEDG(ITRI(I+1)+2)+3)
C is the third (Z) coordinate of the second point of the first edge of
C the triangle with base index I.
C
C It is the pointers from the edge nodes back to the triangle nodes that
C allow CONPACKT to navigate the mesh, moving from triangle to triangle
C as it follows a contour line, but these pointers are tricky to define:
C if IPTE is the base index of an edge node and IEDG(IPTE+3) is zero or
C more, saying that there is a triangle to the left of the edge, then
C IEDG(IPTE+3) is the actual index of that element of the triangle node
C that points to the edge node; i.e., ITRI(IEDG(IPTE+3))=IPTE.  The base
C index of the triangle node defining that triangle is IPTT, where
C IPTT=LOTN*((IEDG(IPTE+3)-1)/LOTN), and the index of the pointer to
C the edge within the triangle node is IPTI=IEDG(IPTE+3)-IPTT, so that
C ITRI(IPTT+IPTI)=IPTE.  Similar comments apply to element 4 of an edge
C node, which points into the triangle node defining the triangle to the
C right of the edge.
C
C The numbers of points, edges, and triangles in the triangular mesh are
C as follows (obtained from a run in which the numbers were set larger
C than this):
C
        PARAMETER (MNOP= 4706)
        PARAMETER (MNOE=14112)
        PARAMETER (MNOT= 9408)
C
C Once we know how many points, edges, and triangles we're going to use
C (at most), we can set parameters defining the space reserved for the
C triangular mesh:
C
        PARAMETER (MPNT=MNOP*LOPN)
        PARAMETER (MEDG=MNOE*LOEN)
        PARAMETER (MTRI=MNOT*LOTN)
C
C Declare the arrays to hold the point nodes, edge nodes, and triangle
C nodes defining the triangular mesh.
C
        DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C Declare sort arrays to be used in GTWNG1 to keep track of where points
C and edges were put in the structure defining the triangular mesh.
C
        DIMENSION IPPP(2,MNOP),IPPE(2,MNOE)
C
C Declare real and integer workspaces needed by CONPACKT.
C
        PARAMETER (LRWK=10000,LIWK=1000)
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare the area map array needed to do solid fill.
C
        PARAMETER (LAMA=400000)
C
        DIMENSION IAMA(LAMA)
C
C Declare workspace arrays to be used in calls to ARSCAM.
C
        PARAMETER (NCRA=LAMA/10,NGPS=2)
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C Declare arrays in which to generate a cell array picture of the
C data on the triangular mesh.
C
C       PARAMETER (ICAM=1024,ICAN=1024)
        PARAMETER (ICAM= 512,ICAN= 512)
C
        DIMENSION ICRA(ICAM,ICAN)
C
C Declare arrays to hold color components for colors to be used at the
C four corners of an illustrative drawing of the rectangular grid.
C
        DIMENSION CCLL(3),CCLR(3),CCUL(3),CCUR(3)
C
C Declare external the routine that draws masked contour lines.
C
        EXTERNAL DRWMCL
C
C Declare external the routine that does color fill of contour bands.
C
        EXTERNAL DCFOCB
C
C Define a common block in which to keep track of the maximum space used
C in the arrays XCRA and YCRA.
C
        COMMON /COMONA/ MAXN
C
C Define the out-of-range flag.
C
        DATA OORV / 1.E12 /
C
C Define the tension on the splines to be used in smoothing contours.
C
C       DATA T2DS / 0.0 /  !  smoothing off
C       DATA T2DS / 2.5 /  !  smoothing on
C
        DATA T2DS / 0.0 /
C
C Define the distance between points on smoothed contour lines.
C
        DATA RSSL / .002 /
C
C Define the amount of real workspace to be used in drawing contours.
C
        DATA IRWC / 500 /
C
C Define the label-positioning flag.
C
C       DATA ILLP / 0 /  !  no labels
C       DATA ILLP / 1 /  !  dash-package writes labels
C       DATA ILLP / 2 /  !  regular scheme
C       DATA ILLP / 3 /  !  penalty scheme
C
        DATA ILLP / 2 /
C
C Define the high/low search radius.
C
        DATA HLSR / .075 /
C
C Define the high/low label overlap flag.
C
        DATA IHLO / 11 /
C
C Define the hachuring flag, hachure length, and hachure spacing.
C
C       DATA IHCF,HCHL,HCHS /  0 , +.004 , .010 /  !  off
C       DATA IHCF,HCHL,HCHS / +1 , -.004 , .020 /  !  on, all, uphill
C
        DATA IHCF,HCHL,HCHS /  0 , +.004 , .010 /
C
C Define the colors to be used in the lower left, lower right, upper
C left, and upper right corners of the rectangular grid when a drawing
C of it is made for the purpose of illustrating how it is wrapped around
C the globe.
C
        DATA CCLL / 0.2 , 0.2 , 0.2 /
        DATA CCLR / 1.0 , 0.0 , 1.0 /
        DATA CCUL / 0.0 , 1.0 , 1.0 /
        DATA CCUR / 1.0 , 1.0 , 0.0 /
C
C Define a constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823 /
C
C Read data and generate the required triangular mesh.
C
        PRINT * , ' '
        PRINT * , 'CREATING TRIANGULAR MESH:'
C
        CALL GTWNG1 (RPNT,MPNT,NPNT,LOPN,
     +               IEDG,MEDG,NEDG,LOEN,
     +               ITRI,MTRI,NTRI,LOTN,
     +               IPPP,IPPE,CLAT,CLON)
C
C Print the number of points, edges, and triangles.
C
        PRINT * , '  NUMBER OF POINTS:    ',NPNT/LOPN
        PRINT * , '  NUMBER OF EDGES:     ',NEDG/LOEN
        PRINT * , '  NUMBER OF TRIANGLES: ',NTRI/LOTN
C
C Write the contents of the point list, the edge list, and the triangle
C list to "fort.11" in a readable form.
C
c       WRITE (11,'(''P'',I8,5F10.4)')
c    +        (I,RPNT(I+1),RPNT(I+2),RPNT(I+3),RPNT(I+4),RPNT(I+5),
c    +                                               I=0,NPNT-LOPN,LOPN)
c       WRITE (11,'(''E'',I8,5I10)')
c    +        (I,IEDG(I+1),IEDG(I+2),IEDG(I+3),IEDG(I+4),IEDG(I+5),
c    +                                               I=0,NEDG-LOEN,LOEN)
c       WRITE (11,'(''T'',I8,4I10)')
c    +        (I,ITRI(I+1),ITRI(I+2),ITRI(I+3),ITRI(I+4),
c    +                                               I=0,NTRI-LOTN,LOTN)
C
C Open GKS.
C
        PRINT * , 'OPENING AND INITIALIZING GKS'
C
        CALL GOPKS (IERF,0)
        CALL GOPWK (IWID,LUNI,IWTY)
        CALL GACWK (IWID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Define a basic set of colors (0 = white, background; 1 = black,
C foreground; 2 = yellow; 3 = magenta; 4 = red; 5 = cyan; 6 = green;
C 7 = blue; 8 = darker light gray; 9 = lighter light gray; 10 = dark
C yellow; 11 = dark gray; 12 = medium gray; 13 = light red, for
C geographic lines; 14 = light blue, for lat/lon lines).
C
        CALL GSCR   (IWID, 0,1.,1.,1.)
        CALL GSCR   (IWID, 1,0.,0.,0.)
        CALL GSCR   (IWID, 2,1.,1.,0.)
        CALL GSCR   (IWID, 3,1.,0.,1.)
        CALL GSCR   (IWID, 4,1.,0.,0.)
        CALL GSCR   (IWID, 5,0.,1.,1.)
        CALL GSCR   (IWID, 6,0.,1.,0.)
        CALL GSCR   (IWID, 7,0.,0.,1.)
        CALL GSCR   (IWID, 8,.5,.5,.5)
        CALL GSCR   (IWID, 9,.8,.8,.8)
        CALL GSCR   (IWID,10,.3,.3,0.)
        CALL GSCR   (IWID,11,.3,.3,.3)
        CALL GSCR   (IWID,12,.5,.5,.5)
        CALL GSCR   (IWID,13,.8,.5,.5)
        CALL GSCR   (IWID,14,.5,.5,.8)
C
C Define 100 colors, associated with color indices 151 through 250, to
C be used for color-filled contour bands and in cell arrays, ranging
C from blue to red.
C
        CALL DFCLRS (IWID,151,250,0.,0.,1.,1.,0.,0.)
C
C Set parameters in the utilities.
C
        PRINT * , 'SETTING PARAMETERS IN CONPACKT, EZMAP, AND PLOTCHAR'
C
C Set the mapping flag.
C
        CALL CTSETI ('MAP - MAPPING FLAG',1)
C
C Set the out-of-range flag value.
C
        CALL CTSETR ('ORV - OUT-OF-RANGE VALUE',OORV)
C
C Turn on the drawing of the mesh edge and set the area identifier for
C areas outside the mesh.
C
        CALL CTSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CTSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CTSETI ('AIA - AREA IDENTIFIER FOR AREA',1001)
C
C Set the area identifier for areas in "out-of-range" areas.
C
C       CALL CTSETI ('PAI',-2)
C       CALL CTSETI ('AIA - AREA IDENTIFIER FOR AREA',1002)
C
C Set the 2D smoother flag.
C
        CALL CTSETR ('T2D - TENSION ON 2D SPLINES',T2DS)
C
C Set the distance between points on smoothed lines.
C
        CALL CTSETR ('SSL - SMOOTHED SEGMENT LENGTH',RSSL)
C
C Set the amount of real workspace to be used in drawing contours.
C
        CALL CTSETI ('RWC - REAL WORKSPACE FOR CONTOURS',IRWC)
C
C Set the label-positioning flag.
C
        CALL CTSETI ('LLP - LINE LABEL POSITIONING FLAG',ILLP)
C
C Set the high/low search radius.
C
        CALL CTSETR ('HLR - HIGH/LOW SEARCH RADIUS',HLSR)
C
C Set the high/low label overlap flag.
C
        CALL CTSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',IHLO)
C
C Set the hachuring flag, hachure length, and hachure spacing.
C
        CALL CTSETI ('HCF - HACHURING FLAG',IHCF)
        CALL CTSETR ('HCL - HACHURE LENGTH',HCHL)
        CALL CTSETR ('HCS - HACHURE SPACING',HCHS)
C
C Set the cell array flag.
C
        CALL CTSETI ('CAF - CELL ARRAY FLAG',-1)
C
C Tell CONPACKT not to do its own call to SET, since EZMAP will have
C done it.
C
        CALL CTSETI ('SET - DO-SET-CALL FLAG', 0)
C
C Move the informational label up a little.
C
        CALL CTSETR ('ILY - INFORMATIONAL LABEL Y POSITION',-.005)
C
C Tell EZMAP not to draw the perimeter.
C
        CALL MPSETI ('PE',0)
C
C Tell EZMAP to use solid lat/lon lines.
C
        CALL MPSETI ('DA',65535)
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
C Loop through four different viewing angles.
C
        DO 104 IDIR=1,4
C
          PRINT * , ' '
          PRINT * , 'VIEW FROM DIRECTION NUMBER: ',IDIR
C
C Tell EZMAP what projection to use and what its limits are.
C
          IF      (IDIR.EQ.1) THEN
            CALL MAPROJ ('OR',CLAT+45.,CLON,      0.)
          ELSE IF (IDIR.EQ.2) THEN
            CALL MAPROJ ('OR',CLAT-45.,CLON,      0.)
          ELSE IF (IDIR.EQ.3) THEN
            CALL MAPROJ ('OR',CLAT+45.,CLON+180., 0.)
          ELSE IF (IDIR.EQ.4) THEN
            CALL MAPROJ ('OR',CLAT-45.,CLON+180., 0.)
          END IF
C
          CALL MAPSET ('MA',0.,0.,0.,0.)
C
C Initialize EZMAP.
C
          CALL MAPINT
C
C If the rectangular grid and/or the triangular mesh are to be drawn,
C do it.
C
          IF (IMSH.NE.0) THEN
C
            PRINT * , 'DRAWING MESHES'
C
C Simple rectangular grid:
C
            IF (MOD(IMSH,2).NE.0) THEN
C
              PRINT * , 'DRAWING SIMPLE RECTANGULAR GRID'
C
              CALL DSWNG1
C
C Label the first frame.
C
              CALL PLCHHQ (CFUX(.03),CFUY(.946),'SEAM GRID',
     +                                                      .024,0.,-1.)
              CALL PLCHHQ (CFUX(.03),CFUY(.906),'(EXAMPLE 1)',
     +                                                      .016,0.,-1.)
C
              IF (     IDIR.EQ.1) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 1',
     +                                                      .012,0.,-1.)
              ELSE IF (IDIR.EQ.2) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 2',
     +                                                      .012,0.,-1.)
              ELSE IF (IDIR.EQ.3) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 3',
     +                                                      .012,0.,-1.)
              ELSE IF (IDIR.EQ.4) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 4',
     +                                                      .012,0.,-1.)
              END IF
C
              CALL PLCHHQ (CFUX(.97),CFUY(.950),'ORIGINAL RECTANGULAR GR
     +ID',.012,0.,1.)
C
              CALL PLCHHQ (CFUX(.97),CFUY(.928),
     +                     'This grid is developed from a cube.',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.908),
     +                     'The numbers label 96 elements,',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.888),
     +                     'each of which derives from',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.868),
     +                     '1/16 of a cube face and',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.848),
     +                     'is subdivided into',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.828),
     +                     'a 7x7 array of',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.808),
     +                     'rectangular',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.788),
     +                     'patches.',
     +                                                       .010,0.,1.)
C
              CALL PLCHHQ (CFUX(.03),CFUY(.084),'Grid is gray.',
     +                                                      .012,0.,-1.)
              CALL PLCHHQ (CFUX(.03),CFUY(.060),'Shorelines are blue.',
     +                                                      .012,0.,-1.)
              CALL PLCHHQ (CFUX(.03),CFUY(.036),'Parallels/meridians are
     + red.',.012,0.,-1.)
C
C Advance the frame.
C
              CALL FRAME
C
            END IF
C
C Triangular mesh (with edges between blocked triangles somewhat fainter
C than the rest):
C
            IF (MOD(IMSH/2,2).NE.0) THEN
C
              PRINT * , 'DRAWING TRIANGULAR MESH'
C
              DO 101 IPTE=0,NEDG-LOEN,LOEN
C
                IFLL=0
C
                IF (IEDG(IPTE+3).GE.0) THEN
                  IF (ITRI(LOTN*((IEDG(IPTE+3)-1)/LOTN)+4).EQ.0) IFLL=1
                END IF
C
                IFLR=0
C
                IF (IEDG(IPTE+4).GE.0) THEN
                  IF (ITRI(LOTN*((IEDG(IPTE+4)-1)/LOTN)+4).EQ.0) IFLR=1
                END IF
C
                IF (IFLL.NE.0.OR.IFLR.NE.0) THEN
                  CALL PLOTIT (0,0,2)
                  CALL GSPLCI (8)
                ELSE
                  CALL PLOTIT (0,0,2)
                  CALL GSPLCI (9)
                END IF
C
                ALAT=RTOD*ASIN(RPNT(IEDG(IPTE+1)+3))
C
                IF (RPNT(IEDG(IPTE+1)+1).EQ.0..AND.
     +              RPNT(IEDG(IPTE+1)+2).EQ.0.) THEN
                  ALON=0.
                ELSE
                  ALON=RTOD*ATAN2(RPNT(IEDG(IPTE+1)+2),
     +                            RPNT(IEDG(IPTE+1)+1))
                END IF
C
                BLAT=RTOD*ASIN(RPNT(IEDG(IPTE+2)+3))
C
                IF (RPNT(IEDG(IPTE+2)+1).EQ.0..AND.
     +              RPNT(IEDG(IPTE+2)+2).EQ.0.) THEN
                  BLON=0.
                ELSE
                  BLON=RTOD*ATAN2(RPNT(IEDG(IPTE+2)+2),
     +                            RPNT(IEDG(IPTE+2)+1))
                END IF
C
                CALL DRSGCR (ALAT,ALON,BLAT,BLON)
C
  101         CONTINUE
C
              CALL PLOTIT (0,0,2)
              CALL GSPLCI (13)
              CALL MAPGRD
              CALL PLOTIT (0,0,2)
              CALL GSPLCI (14)
              CALL MAPLOT
C
C Label the second frame.
C
              CALL PLCHHQ (CFUX(.03),CFUY(.946),'SEAM GRID',
     +                                                      .024,0.,-1.)
              CALL PLCHHQ (CFUX(.03),CFUY(.906),'(EXAMPLE 1)',
     +                                                      .016,0.,-1.)
C
              IF (     IDIR.EQ.1) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 1',
     +                                                      .012,0.,-1.)
              ELSE IF (IDIR.EQ.2) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 2',
     +                                                      .012,0.,-1.)
              ELSE IF (IDIR.EQ.3) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 3',
     +                                                      .012,0.,-1.)
              ELSE IF (IDIR.EQ.4) THEN
                CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 4',
     +                                                      .012,0.,-1.)
              END IF
C
              CALL PLCHHQ (CFUX(.97),CFUY(.950),'DERIVED TRIANGULAR MESH
     +',.012,0.,1.)
C
              CALL PLCHHQ (CFUX(.97),CFUY(.928),
     +                     'Each quadrilateral of the grid',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.908),
     +                     'is split along a diagonal',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.888),
     +                     'to get two triangles',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.868),
     +                     'of the mesh.',
     +                                                       .010,0.,1.)
C
              CALL PLCHHQ (CFUX(.03),CFUY(.084),'Mesh is gray.',
     +                                                      .012,0.,-1.)
              CALL PLCHHQ (CFUX(.03),CFUY(.060),'Shorelines are blue.',
     +                                                      .012,0.,-1.)
              CALL PLCHHQ (CFUX(.03),CFUY(.036),'Parallels/meridians are
     + red.',.012,0.,-1.)
C
C Advance the frame.
C
              CALL FRAME
C
            END IF
C
          END IF
C
C If a frame showing simple contours is to be drawn, do it next (adding
C an overlay of lat/lon lines and continental outlines in light gray).
C
          IF (ICON.NE.0) THEN
C
            PRINT * , 'DRAWING PLOT SHOWING SIMPLE CONTOURS'
C
C Initialize CONPACKT.
C
            PRINT * , 'CALLING CTMESH'
C
            CALL CTMESH (RPNT,NPNT,LOPN,
     +                   IEDG,NEDG,LOEN,
     +                   ITRI,NTRI,LOTN,
     +                   RWRK,LRWK,
     +                   IWRK,LIWK)
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (1)
C
C Proceed as implied by the setting of the label-positioning flag.
C
            IF (ABS(ILLP).EQ.1) THEN
C
C Draw the contour lines with labels generated by the dash package.
C
              PRINT * , 'CALLING CTCLDR'
              CALL CTCLDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
C Add the informational and high/low labels.
C
              PRINT * , 'CALLING CTLBDR'
              CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
            ELSE IF (ABS(ILLP).GT.1) THEN
C
C Create an area map for masking of labels.
C
              MAXN=0
C
              PRINT * , 'CALLING ARINAM'
              CALL ARINAM (IAMA,LAMA)
C
              PRINT * , 'CALLING CTLBAM'
              CALL CTLBAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
C Draw the contour lines masked by the area map.
C
              PRINT * , 'CALLING CTCLDM'
              CALL CTCLDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,DRWMCL)
C
              PRINT * , 'AREA MAP SPACE REQUIRED:         ',
     +                                         IAMA(1)-IAMA(6)+IAMA(5)+1
C
              PRINT * , 'NUMBER OF POINTS IN LONGEST LINE:',MAXN
C
C Draw all the labels.
C
              PRINT * , 'CALLING CTLBDR'
              CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
            END IF
C
            CALL CTGETI ('IWU - INTEGER WORKSPACE USED',IIWU)
            PRINT * , 'INTEGER WORKSPACE REQUIRED:      ',IIWU
C
            CALL CTGETI ('RWU -    REAL WORKSPACE USED',IRWU)
            PRINT * , 'REAL WORKSPACE REQUIRED:         ',IRWU
C
C Add lat/lon lines and continental outlines.
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (13)
            CALL MAPGRD
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (14)
            CALL MAPLOT
C
C Label the third frame.
C
            CALL PLCHHQ (CFUX(.03),CFUY(.946),'SEAM GRID',.024,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.906),'(EXAMPLE 1)',.016,0.,-1.)
C
            IF (     IDIR.EQ.1) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 1',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.2) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 2',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.3) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 3',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.4) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 4',
     +                                                      .012,0.,-1.)
            END IF
C
            CALL PLCHHQ (CFUX(.97),CFUY(.950),'SIMPLE CONTOURS ON',
     +                                                       .012,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.926),'TRIANGULAR MESH',
     +                                                       .012,0.,1.)
C
            CALL PLCHHQ (CFUX(.97),CFUY(.904),'Data values come from a',
     +                                                       .010,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.884),'land/water mask.',
     +                                                       .010,0.,1.)
C
            CALL PLCHHQ (CFUX(.03),CFUY(.060),'Shorelines are blue.',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.036),'Parallels/meridians are r
     +ed.',.012,0.,-1.)
C
C Advance the frame.
C
            CALL FRAME
C
          END IF
C
C If a frame showing color-filled contours is to be drawn, do it next.
C
          IF (ICOL.NE.0) THEN
C
            PRINT * , 'DRAWING PLOT SHOWING COLOR-FILLED CONTOURS'
C
            PRINT * , 'CALLING CTMESH'
C
            CALL CTMESH (RPNT,NPNT,LOPN,
     +                   IEDG,NEDG,LOEN,
     +                   ITRI,NTRI,LOTN,
     +                   RWRK,LRWK,
     +                   IWRK,LIWK)
C
            MAXN=0
C
            PRINT * , 'CALLING CTPKCL'
            CALL CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
C
            PRINT * , 'CALLING ARINAM'
            CALL ARINAM (IAMA,LAMA)
C
            PRINT * , 'CALLING CTCLAM'
            CALL CTCLAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
C           PRINT * , 'CALLING CTLBAM'
C           CALL CTLBAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
            PRINT * , 'CALLING ARSCAM'
            CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,DCFOCB)
C
            PRINT * , 'SPACE REQUIRED IN AREA MAP:      ',
     +                                         IAMA(1)-IAMA(6)+IAMA(5)+1
C
            PRINT * , 'NUMBER OF POINTS IN LARGEST AREA:',MAXN
C
            CALL CTGETI ('IWU - INTEGER WORKSPACE USED',IIWU)
            PRINT * , 'INTEGER WORKSPACE REQUIRED:      ',IIWU
C
            CALL CTGETI ('RWU -    REAL WORKSPACE USED',IRWU)
            PRINT * , 'REAL WORKSPACE REQUIRED:         ',IRWU
C
            CALL GSFACI (1)
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (13)
            CALL MAPGRD
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (14)
            CALL MAPLOT
C
C Label the fourth frame.
C
            CALL PLCHHQ (CFUX(.03),CFUY(.946),'SEAM GRID',.024,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.906),'(EXAMPLE 1)',.016,0.,-1.)
C
            IF (     IDIR.EQ.1) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 1',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.2) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 2',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.3) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 3',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.4) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 4',
     +                                                      .012,0.,-1.)
            END IF
C
            CALL PLCHHQ (CFUX(.97),CFUY(.950),'COLORED CONTOUR BANDS ON'
     +,.012,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.926),'TRIANGULAR MESH',
     +                                                       .012,0.,1.)
C
            CALL PLCHHQ (CFUX(.97),CFUY(.904),'Data values come from a',
     +                                                       .010,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.884),'land/water mask.',
     +                                                       .010,0.,1.)
C
            CALL PLCHHQ (CFUX(.03),CFUY(.132),'Off-mesh',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.108),'areas of the',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.084),'globe are yellow.',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.060),'Shorelines are blue.',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.036),'Parallels/meridians are r
     +ed.',.012,0.,-1.)
C
C Advance the frame.
C
            CALL FRAME
C
          END IF
C
C If the flag for it is set, do a cell array plot.
C
          IF (ICAP.NE.0) THEN
C
            PRINT * , 'DRAWING CELL-ARRAY PLOT OF DATA VALUES'
C
            PRINT * , 'CALLING CTMESH'
C
            CALL CTMESH (RPNT,NPNT,LOPN,
     +                   IEDG,NEDG,LOEN,
     +                   ITRI,NTRI,LOTN,
     +                   RWRK,LRWK,
     +                   IWRK,LIWK)
C
            CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
            PRINT * , 'CALLING CTCICA'
            CALL CTCICA (RPNT,IEDG,ITRI,RWRK,IWRK,ICRA,ICAM,ICAM,ICAN,
     +                                            XVPL,YVPB,XVPR,YVPT)
C
            PRINT * , 'CALLING GCA'
            CALL GCA (XWDL,YWDB,XWDR,YWDT,ICAM,ICAN,1,1,ICAM,ICAN,ICRA)
C
            CALL CTGETI ('IWU - INTEGER WORKSPACE USED',IIWU)
            PRINT * , 'INTEGER WORKSPACE REQUIRED:      ',IIWU
C
            CALL CTGETI ('RWU -    REAL WORKSPACE USED',IRWU)
            PRINT * , 'REAL WORKSPACE REQUIRED:         ',IRWU
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (13)
            CALL MAPGRD
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (14)
            CALL MAPLOT
C
C Label the fifth frame.
C
            CALL PLCHHQ (CFUX(.03),CFUY(.946),'SEAM GRID',.024,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.906),'(EXAMPLE 1)',.016,0.,-1.)
C
            IF (     IDIR.EQ.1) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 1',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.2) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 2',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.3) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 3',
     +                                                      .012,0.,-1.)
            ELSE IF (IDIR.EQ.4) THEN
              CALL PLCHHQ (CFUX(.03),CFUY(.874),'VIEWPOINT 4',
     +                                                      .012,0.,-1.)
            END IF
C
            CALL PLCHHQ (CFUX(.97),CFUY(.950),'CELL ARRAY DERIVED FROM',
     +.012,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.926),'TRIANGULAR MESH',
     +                                                       .012,0.,1.)
C
            CALL PLCHHQ (CFUX(.97),CFUY(.904),'Data values come from a',
     +                                                       .010,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.884),'land/water mask.',
     +                                                       .010,0.,1.)
C
            CALL PLCHHQ (CFUX(.03),CFUY(.132),'Off-mesh',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.108),'areas of the',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.084),'globe are yellow.',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.060),'Shorelines are blue.',
     +                                                      .012,0.,-1.)
            CALL PLCHHQ (CFUX(.03),CFUY(.036),'Parallels/meridians are r
     +ed.',.012,0.,-1.)
C
C Advance the frame.
C
            CALL FRAME
C
          END IF
C
  104   CONTINUE
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


      SUBROUTINE DFCLRS (IWID,IOFC,IOLC,REDF,GRNF,BLUF,REDL,GRNL,BLUL)
C
C This routine defines color indices IOFC through IOLC on workstation
C IWID by interpolating values from REDF/GRNF/BLUF to REDL/GRNL/BLUL.
C
        DO 101 I=IOFC,IOLC
          P=REAL(IOLC-I)/REAL(IOLC-IOFC)
          Q=REAL(I-IOFC)/REAL(IOLC-IOFC)
          CALL GSCR (IWID,I,P*REDF+Q*REDL,P*GRNF+Q*GRNL,P*BLUF+Q*BLUL)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE DRWMCL (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C This routine draws the curve defined by the points (XCRA(I),YCRA(I)),
C for I = 1 to NCRA, if and only if none of the area identifiers for the
C area containing the polyline are negative.  It calls either CURVE or
C CURVED to do the drawing, depending on the value of the internal
C parameter 'DPU'.
C
C It keeps track of the maximum value used for NCRA in a common block.
C
        COMMON /COMONA/ MAXN
C
        MAXN=MAX(MAXN,NCRA)
C
C Retrieve the value of the internal parameter 'DPU'.
C
        CALL CTGETI ('DPU - DASH PACKAGE USED',IDUF)
C
C Turn on drawing.
C
        IDRW=1
C
C If any area identifier is negative, turn off drawing.
C
        DO 101 I=1,NGPS
          IF (IAAI(I).LT.0) IDRW=0
  101   CONTINUE
C
C If drawing is turned on, draw the polyline.
C
        IF (IDRW.NE.0) THEN
          IF (IDUF.EQ.0) THEN
            CALL CURVE  (XCRA,YCRA,NCRA)
          ELSE IF (IDUF.LT.0) THEN
            CALL DPCURV (XCRA,YCRA,NCRA)
          ELSE
            CALL CURVED (XCRA,YCRA,NCRA)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE DCFOCB (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
C This routine fills the area defined by the points (XCRA(I),YCRA(I)),
C for I = 1 to NCRA, if and only if none of the area identifiers for
C the area are negative.  The color used is determined from the area
C identifier of the area relative to group 3; it is assumed that 100
C colors are defined having color indices 151 through 250.
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C It keeps track of the maximum value used for NCRA in a common block.
C
        COMMON /COMONA/ MAXN
C
        MAXN=MAX(MAXN,NCRA)
C
C Retrieve the number of contour levels being used.
C
        CALL CTGETI ('NCL - NUMBER OF CONTOUR LEVELS',NOCL)
C
C If the number of contour levels is non-zero and the area has more
C than two points, fill it.
C
        IF (NOCL.NE.0.AND.NCRA.GT.2) THEN
C
          IAI3=-1
C
          DO 101 I=1,NGPS
            IF (IAGI(I).EQ.3) IAI3=IAAI(I)
  101     CONTINUE
C
          IF (IAI3.GE.1.AND.IAI3.LE.NOCL+1) THEN
            CALL GSFACI (151+INT(((REAL(IAI3)-.5)/REAL(NOCL+1))*100.))
            CALL GFA    (NCRA,XCRA,YCRA)
          ELSE IF (IAI3.EQ.1001) THEN
            CALL GSFACI (2)
            CALL GFA    (NCRA,XCRA,YCRA)
          ELSE IF (IAI3.EQ.1002) THEN
            CALL GSFACI (3)
            CALL GFA    (NCRA,XCRA,YCRA)
          END IF
C
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE GTWNG1 (RPNT,MPNT,NPNT,LOPN,
     +                   IEDG,MEDG,NEDG,LOEN,
     +                   ITRI,MTRI,NTRI,LOTN,
     +                   IPPP,IPPE,CLAT,CLON)
C
        DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI),IPPP(2,*),IPPE(2,*)
C
C Construct a triangular mesh using SEAM data from Houjun Wang,
C using a routine (CTTMTL) that makes it easy to create an arbitrary
C triangular mesh (first used above in GTGEO2).  The "tree-sorts"
C used by this method are very inefficient for objects that are
C partially ordered, so, as the triangles of the mesh are generated,
C they are stored in a triangle buffer from which they can be dumped
C in random order by calls to the routine CTTMTL.
C
C Declare the array to use as the buffer for the triangles, so that the
C order in which they are processed can be somewhat randomized.  Up to
C a point, making this buffer larger will result in more randomization
C of the triangles and speed up the process.  MBUF is the number of
C triangles that will fit in the buffer at once, and KBUF is the number
C of those that are dumped whenever the buffer is found to be full; it
C turns out that it's better to dump more than 1 at a time (I suppose
C because the call and loop set-up time for CTTMTL are non-trivial, so
C it's better to dump a few triangles while you're there).  So ... use
C MBUF > KBUF > 1.
C
        PARAMETER (MBUF=5021,KBUF=173,MBM1=MBUF-1,DTOR=.017453292519943)
C
        DIMENSION TBUF(12,MBUF)
C
C Include definitions that "NetCDF" needs (now commented out, because
C the data are being read from an ASCII file, instead).
C
C       include 'netcdf.inc'
C
C Declare arrays to receive lat/lon coordinate data from Houjun's
C NetCDF data file.
C
        COMMON /CMWNG1/ TLAT(8,8,96),TLON(8,8,96),ELAT(96),ELON(96)
C
C Declare arrays to receive other data from Houjun's NetCDF data file.
C
        DIMENSION LAND(8,8,96)
C
C The data for this example were originally read from a user's "NetCDF"
C file.  The code used for the purpose follows, but has been commented
C out, as the data are now read from an ASCII file.  This gets around
C certain procedural problems in running the example from "ncargex".
C
C Open the "NetCDF" file.
C
C       ISTA=NF_OPEN('ctwng1.nc',0,NCID)
C
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_OPEN: ',ISTA
C         STOP
C       END IF
C
C Read the array of latitudes of cell centers.
C
C       ISTA=NF_INQ_VARID(NCID,'LATIXY',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C       ISTA=NF_GET_VAR_REAL(NCID,IVID,TLAT)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_REAL: ',ISTA
C         STOP
C       END IF
C
C Read the array of longitudes of cell centers.
C
C       ISTA=NF_INQ_VARID(NCID,'LONGXY',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C       ISTA=NF_GET_VAR_REAL(NCID,IVID,TLON)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_REAL: ',ISTA
C         STOP
C       END IF
C
C Read the "land/ocean mask" array.
C
C       ISTA=NF_INQ_VARID(NCID,'LANDMASK',IVID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_INQ_VARID: ',ISTA
C         STOP
C       END IF
C       ISTA=NF_GET_VAR_INT(NCID,IVID,LAND)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_GET_VAR_INT: ',ISTA
C         STOP
C       END IF
C
C Close the "NetCDF" file.
C
C       ISTA=NF_CLOSE(NCID)
C       IF (ISTA.NE.NF_NOERR) THEN
C         PRINT * , 'ERROR RETURN FROM NF_CLOSE: ',ISTA
C         STOP
C       END IF
C
C Write the data to unit 11.  (These statements are commented out; they
C were used to create a "fort.11" which was then renamed "ctwng1.dat".)
C
C       WRITE (11,'(5E16.8)') TLAT
C       WRITE (11,'(5E16.8)') TLON
C       WRITE (11,  '(20I4)') LAND
C
C Read the required data from the ASCII file "ctwng1.dat".
C
        OPEN (11,FILE='ctwng1.dat',STATUS='OLD',FORM='FORMATTED')
C
        READ (11,'(5E16.8)') TLAT
        READ (11,'(5E16.8)') TLON
        READ (11,  '(20I4)') LAND
C
        CLOSE (11)
C
C Find the center points of the elements of Wang's grid.
C
        DO 101 K=1,96
          CALL IPIQOS (TLAT(4,4,K),TLON(4,4,K),TLAT(5,4,K),TLON(5,4,K),
     +                 TLAT(4,5,K),TLON(4,5,K),TLAT(5,5,K),TLON(5,5,K),
     +                                           .5,.5,ELAT(K),ELON(K))
  101   CONTINUE
C
C Build structures forming the triangular mesh.  First, initialize the
C variables used to keep track of items in the sort arrays for points
C and edges, in the triangle buffer, and in the triangle list.
C
        MPPP=MPNT/LOPN
        NPPP=0
C
        MPPE=MEDG/LOEN
        NPPE=0
C
        NBUF=0
C
        NTRI=0
C
C Loop through the triangles derived from Houjun's SEAM grid.  K indexes
C the elements of the rectangular mesh, each of which is a 7-by-7 array
C of rectangles.  IROW and ICOL are the row and column numbers of the
C element within a square face of the cube; they are used to determine
C the "parity" of the rectangle, determining which diagonal of it to use
C to split it into two triangles.
C
        DO 107 K=1,96
C
          IROW=MOD(K-1,16)/4+1
          ICOL=MOD(K-1,4)+1
C
C I and J index the rectangular pieces of each square element.
C
          DO 106 I=1,7
            DO 105 J=1,7
C
C If the triangle buffer is almost full, dump a few randomly-chosen
C triangles from it to make room for two new ones.
C
              IF (NBUF.GE.MBM1) THEN
                CALL CTTMTL (KBUF,TBUF,MBUF,NBUF,
     +                       IPPP,MPPP,NPPP,
     +                       IPPE,MPPE,NPPE,
     +                       RPNT,MPNT,NPNT,LOPN,
     +                       IEDG,MEDG,NEDG,LOEN,
     +                       ITRI,MTRI,NTRI,LOTN)
              END IF
C
C Put two new triangles in the triangle buffer.
C
              IF (MOD(I+J+IROW+ICOL,2).EQ.0) THEN
C
                NBUF=NBUF+1
C
                TBUF( 1,NBUF)=COS(DTOR*TLAT(I  ,J  ,K))*
     +                        COS(DTOR*TLON(I  ,J  ,K))
                TBUF( 2,NBUF)=COS(DTOR*TLAT(I  ,J  ,K))*
     +                        SIN(DTOR*TLON(I  ,J  ,K))
                TBUF( 3,NBUF)=SIN(DTOR*TLAT(I  ,J  ,K))
                TBUF( 4,NBUF)=REAL(  LAND(I  ,J  ,K))
C
                TBUF( 5,NBUF)=COS(DTOR*TLAT(I+1,J  ,K))*
     +                        COS(DTOR*TLON(I+1,J  ,K))
                TBUF( 6,NBUF)=COS(DTOR*TLAT(I+1,J  ,K))*
     +                        SIN(DTOR*TLON(I+1,J  ,K))
                TBUF( 7,NBUF)=SIN(DTOR*TLAT(I+1,J  ,K))
                TBUF( 8,NBUF)=REAL(    LAND(I+1,J  ,K))
C
                TBUF( 9,NBUF)=COS(DTOR*TLAT(I+1,J+1,K))*
     +                        COS(DTOR*TLON(I+1,J+1,K))
                TBUF(10,NBUF)=COS(DTOR*TLAT(I+1,J+1,K))*
     +                        SIN(DTOR*TLON(I+1,J+1,K))
                TBUF(11,NBUF)=SIN(DTOR*TLAT(I+1,J+1,K))
                TBUF(12,NBUF)=REAL(    LAND(I+1,J+1,K))
C
                NBUF=NBUF+1
C
                TBUF( 1,NBUF)=COS(DTOR*TLAT(I  ,J  ,K))*
     +                        COS(DTOR*TLON(I  ,J  ,K))
                TBUF( 2,NBUF)=COS(DTOR*TLAT(I  ,J  ,K))*
     +                        SIN(DTOR*TLON(I  ,J  ,K))
                TBUF( 3,NBUF)=SIN(DTOR*TLAT(I  ,J  ,K))
                TBUF( 4,NBUF)=REAL(    LAND(I  ,J  ,K))
C
                TBUF( 5,NBUF)=COS(DTOR*TLAT(I+1,J+1,K))*
     +                        COS(DTOR*TLON(I+1,J+1,K))
                TBUF( 6,NBUF)=COS(DTOR*TLAT(I+1,J+1,K))*
     +                        SIN(DTOR*TLON(I+1,J+1,K))
                TBUF( 7,NBUF)=SIN(DTOR*TLAT(I+1,J+1,K))
                TBUF( 8,NBUF)=REAL(    LAND(I+1,J+1,K))
C
                TBUF( 9,NBUF)=COS(DTOR*TLAT(I  ,J+1,K))*
     +                        COS(DTOR*TLON(I  ,J+1,K))
                TBUF(10,NBUF)=COS(DTOR*TLAT(I  ,J+1,K))*
     +                        SIN(DTOR*TLON(I  ,J+1,K))
                TBUF(11,NBUF)=SIN(DTOR*TLAT(I  ,J+1,K))
                TBUF(12,NBUF)=REAL(    LAND(I  ,J+1,K))
C
              ELSE
C
                NBUF=NBUF+1
C
                TBUF( 1,NBUF)=COS(DTOR*TLAT(I  ,J  ,K))*
     +                        COS(DTOR*TLON(I  ,J  ,K))
                TBUF( 2,NBUF)=COS(DTOR*TLAT(I  ,J  ,K))*
     +                        SIN(DTOR*TLON(I  ,J  ,K))
                TBUF( 3,NBUF)=SIN(DTOR*TLAT(I  ,J  ,K))
                TBUF( 4,NBUF)=REAL(    LAND(I  ,J  ,K))
C
                TBUF( 5,NBUF)=COS(DTOR*TLAT(I+1,J  ,K))*
     +                        COS(DTOR*TLON(I+1,J  ,K))
                TBUF( 6,NBUF)=COS(DTOR*TLAT(I+1,J  ,K))*
     +                        SIN(DTOR*TLON(I+1,J  ,K))
                TBUF( 7,NBUF)=SIN(DTOR*TLAT(I+1,J  ,K))
                TBUF( 8,NBUF)=REAL(    LAND(I+1,J  ,K))
C
                TBUF( 9,NBUF)=COS(DTOR*TLAT(I  ,J+1,K))*
     +                        COS(DTOR*TLON(I  ,J+1,K))
                TBUF(10,NBUF)=COS(DTOR*TLAT(I  ,J+1,K))*
     +                        SIN(DTOR*TLON(I  ,J+1,K))
                TBUF(11,NBUF)=SIN(DTOR*TLAT(I  ,J+1,K))
                TBUF(12,NBUF)=REAL(    LAND(I  ,J+1,K))
C
                NBUF=NBUF+1
C
                TBUF( 1,NBUF)=COS(DTOR*TLAT(I+1,J  ,K))*
     +                        COS(DTOR*TLON(I+1,J  ,K))
                TBUF( 2,NBUF)=COS(DTOR*TLAT(I+1,J  ,K))*
     +                        SIN(DTOR*TLON(I+1,J  ,K))
                TBUF( 3,NBUF)=SIN(DTOR*TLAT(I+1,J  ,K))
                TBUF( 4,NBUF)=REAL(    LAND(I+1,J  ,K))
C
                TBUF( 5,NBUF)=COS(DTOR*TLAT(I+1,J+1,K))*
     +                        COS(DTOR*TLON(I+1,J+1,K))
                TBUF( 6,NBUF)=COS(DTOR*TLAT(I+1,J+1,K))*
     +                        SIN(DTOR*TLON(I+1,J+1,K))
                TBUF( 7,NBUF)=SIN(DTOR*TLAT(I+1,J+1,K))
                TBUF( 8,NBUF)=REAL(    LAND(I+1,J+1,K))
C
                TBUF( 9,NBUF)=COS(DTOR*TLAT(I  ,J+1,K))*
     +                        COS(DTOR*TLON(I  ,J+1,K))
                TBUF(10,NBUF)=COS(DTOR*TLAT(I  ,J+1,K))*
     +                        SIN(DTOR*TLON(I  ,J+1,K))
                TBUF(11,NBUF)=SIN(DTOR*TLAT(I  ,J+1,K))
                TBUF(12,NBUF)=REAL(    LAND(I  ,J+1,K))
C
              END IF
C
  105       CONTINUE
  106     CONTINUE
  107   CONTINUE
C
C Output all triangles remaining in the buffer.
C
        IF (NBUF.NE.0) THEN
          CALL CTTMTL (NBUF,TBUF,MBUF,NBUF,
     +                 IPPP,MPPP,NPPP,
     +                 IPPE,MPPE,NPPE,
     +                 RPNT,MPNT,NPNT,LOPN,
     +                 IEDG,MEDG,NEDG,LOEN,
     +                 ITRI,MTRI,NTRI,LOTN)
        END IF
C
C Set the pointers that tell the caller how many points and edges were
C created.
C
        NPNT=NPPP*LOPN
        NEDG=NPPE*LOEN
C
C Initialize the dummy-data generator and get data values for all the
C points of the mesh.
C
c       CALL TDGDIN (-1.,1.,-1.,1.,-1.,1.,21,21)
C
c       DO 108 I=0,NPNT-LOPN,LOPN
c         RPNT(I+4)=TDGDVA(RPNT(I+1),RPNT(I+2),RPNT(I+3))
c 108   CONTINUE
C
C Return the latitude and longitude of the approximate center point of
C the mesh on the globe.
C
        CLAT=0.
        CLON=0.
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE FNDBST (SLAT,SLON,IBST,JBST,KBST)
C
C Find that point in Houjun Wang's grid that is closest to the point
C with latitude SLAT and longitude SLON.
C
        COMMON /CMWNG1/ TLAT(8,8,96),TLON(8,8,96),ELAT(96),ELON(96)
C
C Initialize.
C
        IBST=0
        JBST=0
        KBST=0
        DBST=181.
C
C Find the index of the element whose center point is closest.
C
        DO 101 K=1,96
          DIST=ADSGCR(SLAT,SLON,ELAT(K),ELON(K))
          IF (DIST.LT.DBST) THEN
            KBST=K
            DBST=DIST
          END IF
  101   CONTINUE
C
C Now find the indices of the point of that element that is closest.
C
        DO 103 I=1,8
          DO 102 J=1,8
            DIST=ADSGCR(SLAT,SLON,TLAT(I,J,KBST),TLON(I,J,KBST))
            IF (DIST.LT.DBST) THEN
              IBST=I
              JBST=J
              DBST=DIST
            END IF
  102     CONTINUE
  103   CONTINUE
C
        RETURN
C
      END


      SUBROUTINE DSWNG1
C
C Draw the 96 simple rectangular grids that comprise the SEAM grid,
C using data passed in a common block.
C
        COMMON /CMWNG1/ TLAT(8,8,96),TLON(8,8,96),ELAT(96),ELON(96)
C
C Declare a character temporary in which to form labels.
C
        CHARACTER*2 CTMP
C
C Draw the grid.
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (8)
        CALL GSLWSC (2.)
C
        DO 105 K=1,96
C
          DO 102 I=1,7
            DO 101 J=1,7
              CALL DRSGCR (TLAT(I  ,J  ,K),TLON(I  ,J  ,K),
     +                     TLAT(I  ,J+1,K),TLON(I  ,J+1,K))
  101       CONTINUE
  102     CONTINUE
C
          DO 104 J=1,7
            DO 103 I=1,7
              CALL DRSGCR (TLAT(I  ,J  ,K),TLON(I  ,J  ,K),
     +                     TLAT(I+1,J  ,K),TLON(I+1,J  ,K))
  103       CONTINUE
  104     CONTINUE
C
C Label each element with its index (from 1 to 96, inclusive).
C
          CALL MAPTRN (ELAT(K),ELON(K),XTMP,YTMP)
C
          IF (XTMP.NE.1.E12) THEN
            WRITE (CTMP,'(I2)') K
            IF (CTMP(1:1).EQ.' ') CTMP(1:1)='0'
            CALL PCSETI ('CC',1)
            CALL PLCHHQ (XTMP,YTMP,CTMP,.015,0.,0.)
            CALL PCSETI ('CC',-1)
          END IF
C
  105   CONTINUE
C
C Draw the continental outlines and lat/lon grid.
C
        CALL PLOTIT (0,0,2)
        CALL GSLWSC (1.)
        CALL GSPLCI (13)
        CALL MAPGRD
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (14)
        CALL MAPLOT
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE DRSGCR (ALAT,ALON,BLAT,BLON)
C
C (DRSGCR = DRaw Shortest Great Circle Route)
C
C This routine draws the shortest great circle route joining two points
C on the globe.  Note that MPTS = INT(180./SIZE) + 1.
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


      SUBROUTINE IPIQOS (ALAT,ALON,BLAT,BLON,CLAT,CLON,DLAT,DLON,
     +                                       XFRA,YFRA,ELAT,ELON)
C
C (IPIQOS = Interpolate Point In Quadrilateral on Sphere)
C
C This routine, given the latitudes and longitudes of four points (A, B,
C C, and D) forming a "quadrilateral" on the globe and two interpolation
C fractions (XFRA and YFRA, each between 0 and 1, inclusive), finds the
C point E defined by the following diagram and returns its latitude and
C longitude.
C
C                              C------Q----D
C                              |      |    |
C                              |      E    |
C                              |      |    |
C                              |      |    |
C                              A------P----B
C
C P and Q are positioned such that AP/AB = CQ/CD = XFRA and then E is
C positioned such that PE/PQ = YFRA (where "XY" is interpreted to mean
C "the shortest great circle distance from X to Y").
C
C It is assumed that the "quadrilateral" ABDC is "convex" (a working
C definition of which might be that none of the four great circles
C defined by its edges - the ones through A and B, B and D, D and C,
C and C and A - cross it anywhere.  However, this is not verified.
C
C The code is easy:
C
        CALL FPSGCR (ALAT,ALON,BLAT,BLON,XFRA,PLAT,PLON)
        CALL FPSGCR (CLAT,CLON,DLAT,DLON,XFRA,QLAT,QLON)
        CALL FPSGCR (PLAT,PLON,QLAT,QLON,YFRA,ELAT,ELON)
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


      SUBROUTINE CTSCAE (ICRA,ICA1,ICAM,ICAN,XCPF,YCPF,XCQF,YCQF,
     +                                       IND1,IND2,ICAF,IAID)
        DIMENSION ICRA(ICA1,*)
C
C This routine is called by CTCICA when the internal parameter 'CAF' is
C given a negative value.  Each call is intended to create a particular
C element in the user's cell array.  The arguments are as follows:
C
C ICRA is the user's cell array.
C
C ICA1 is the first dimension of the FORTRAN array ICRA.
C
C ICAM and ICAN are the first and second dimensions of the cell array
C stored in ICRA.
C
C (XCPF,YCPF) is the point at that corner of the rectangular area
C into which the cell array maps that corresponds to the cell (1,1).
C The coordinates are given in the fractional coordinate system (unlike
C what is required in a call to GCA, in which the coordinates of the
C point P are in the world coordinate system).
C
C (XCQF,YCQF) is the point at that corner of the rectangular area into
C which the cell array maps that corresponds to the cell (ICAM,ICAN).
C The coordinates are given in the fractional coordinate system (unlike
C what is required in a call to GCA, in which the coordinates of the
C point Q are in the world coordinate system).
C
C IND1 is the 1st index of the cell that is to be updated.
C
C IND2 is the 2nd index of the cell that is to be updated.
C
C ICAF is the current value of the internal parameter 'CAF'.  This
C value will always be an integer which is less than zero (because
C when 'CAF' is zero or greater, this routine is not called).
C
C IAID is the area identifier associated with the cell.  It will have
C been given one of the values from the internal parameter array 'AIA'
C (the one for 'PAI' = -2 if the cell lies in an out-of-range area, the
C one for 'PAI' = -1 if the cell lies off the data grid, or the one for
C some value of 'PAI' between 1 and 'NCL' if the cell lies on the data
C grid).  The value zero may occur if the cell falls in an out-of-range
C area and the value of 'AIA' for 'PAI' = -2 is 0 or if the cell lies
C off the data grid and the value of 'AIA' for 'PAI' = -1 is 0, or if
C the cell falls on the data grid, but no contour level below the cell
C has a non-zero 'AIA' and no contour level above the cell has a
C non-zero 'AIB'.  Note that, if the values of 'AIA' for 'PAI' = -1
C and -2 are given non-zero values, IAID can only be given a zero
C value in one way.
C
C The default behavior of CTSCAE is as follows:  If the area identifier
C is non-negative, it is treated as a color index, to be stored in the
C appropriate cell in the cell array; but if the area identifier is
C negative, a zero is stored for the color index.  The user may supply
C a version of CTSCAE that does something different; it may simply map
C the area identifiers into color indices or it may somehow modify the
C existing cell array element to incorporate the information provided
C by the area identifier.
C
C       ICRA(IND1,IND2)=MAX(0,IAID)
C
C What follows is not the default behavior; instead, it is the behavior
C expected by many of the example programs for CONPACKT.
C
        CALL CTGETI ('NCL - NUMBER OF CONTOUR LEVELS',NOCL)
C
        IF (IAID.GE.1.AND.IAID.LE.NOCL+1) THEN
          ICRA(IND1,IND2)=151+INT(((REAL(IAID)-.5)/REAL(NOCL+1))*100.)
        ELSE IF (IAID.EQ.1001) THEN
          ICRA(IND1,IND2)=2
        ELSE IF (IAID.EQ.1002) THEN
          ICRA(IND1,IND2)=3
        END IF
C
        RETURN
C
      END
