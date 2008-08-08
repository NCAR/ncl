

      PROGRAM CTISC2
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
C the use of a triangular mesh to represent an ISCCP grid on the surface
C of the globe.  The original rectangular grid is of a different nature
C than others: each of 72 latitude bands of equal width is cut into just
C enough pieces so that each of the pieces will have about the same area
C on the globe.  The resulting 6596 little pieces, each of which is a
C rectangle in lat/lon space, form a sort of tiling of the sphere.  The
C triangular mesh is created by using the center points of the pieces as
C vertices and connecting adjacent vertices to form edges which form
C triangles.
C
C Selected frames are drawn for each of four different viewpoints.  If
C (CLAT,CLON) is the approximate position of the "center point" of the
C mesh (which is defined somewhat arbitrarily for this mesh), the four
C viewpoints used are as follows: (CLAT+45,CLON), (CLAT-45,CLON),
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
C      removing it from the mesh.  Play with the setting of the
C      parameter ISCP (which see, below) to get examples of the use
C      of this feature.
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
C The numbers of points, edges, and triangles in the triangular mesh
C are as follows (obtained from the print output of a run in which the
C numbers were set larger than this):
C
        PARAMETER (MNOP=6596)
        PARAMETER (MNOE=19782)
        PARAMETER (MNOT=13188)
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
C Declare sort arrays to be used to keep track of where points and
C edges were put in the structure defining the triangular mesh.
C
        DIMENSION IPPP(2,MNOP),IPPE(2,MNOE)
C
C Define a parameter specifying what data to use on the ISCCP grid.  A
C triangle is blocked if and only if more than "n" of its vertices are
C of the type blocked.  For example, when ISCP is 12, "n" is 0, so that
C a triangle is blocked if one or more of its vertices are over land.
C
C       PARAMETER (ISCP=  1) ! ISCCP, topo data, no blocking
C       PARAMETER (ISCP= 21) ! ISCCP, topo data, block ocean, n=0
C       PARAMETER (ISCP=121) ! ISCCP, topo data, block ocean, n=1
C       PARAMETER (ISCP=221) ! ISCCP, topo data, block ocean, n=2
C       PARAMETER (ISCP=  2) ! ISCCP, fractal data, no blocking
C       PARAMETER (ISCP= 12) ! ISCCP, fractal data, block land, n=0 *
C       PARAMETER (ISCP=112) ! ISCCP, fractal data, block land, n=1
C       PARAMETER (ISCP=212) ! ISCCP, fractal data, block land, n=2
C       PARAMETER (ISCP= 22) ! ISCCP, fractal data, block ocean, n=0
C       PARAMETER (ISCP=122) ! ISCCP, fractal data, block ocean, n=1
C       PARAMETER (ISCP=222) ! ISCCP, fractal data, block ocean, n=2 *
C
        PARAMETER (ISCP= 12)
C
C Declare real and integer workspaces needed by CONPACKT.
C
        PARAMETER (LRWK=10000,LIWK=1000)
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare the area map array needed to do solid fill.
C
        PARAMETER (LAMA=200000)
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
C Define a constant to convert from radians to degrees.
C
        DATA RTOD / 57.2957795130823 /
C
C Read data and generate the required triangular mesh.
C
        PRINT * , ' '
        PRINT * , 'CREATING TRIANGULAR MESH:'
C
        CALL GTISC2 (RPNT,MPNT,NPNT,LOPN,
     +               IEDG,MEDG,NEDG,LOEN,
     +               ITRI,MTRI,NTRI,LOTN,
     +               IPPP,IPPE,ISCP,CLAT,CLON)
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
              PRINT * , '  DRAWING SIMPLE RECTANGULAR GRID'
C
              CALL DSISC2
C
C Label the first frame.
C
              CALL PLCHHQ (CFUX(.03),CFUY(.946),'ISCCP GRID',
     +                                                      .024,0.,-1.)
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
     +                     'Each of the 72 2.5-degree-wide strips',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.908),
     +                     'is cut into just enough pieces so',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.888),
     +                     'as to make all of the pieces',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.868),
     +                     'have approximately the',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.848),
     +                     'same surface area',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.828),
     +                     'on the globe.',
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
              PRINT * , '  DRAWING TRIANGULAR MESH'
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
              CALL PLCHHQ (CFUX(.03),CFUY(.946),'ISCCP GRID',
     +                                                      .024,0.,-1.)
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
     +                     'Vertices are at centers of rectangles.',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.908),
     +                     'Land portions of the mesh are',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.888),
     +                     '"blocked" and are shown',
     +                                                       .010,0.,1.)
              CALL PLCHHQ (CFUX(.97),CFUY(.868),
     +                     'in very light gray.',
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
              PRINT * , '  AREA MAP SPACE REQUIRED:         ',
     +                                         IAMA(1)-IAMA(6)+IAMA(5)+1
C
              PRINT * , '  NUMBER OF POINTS IN LONGEST LINE:',MAXN
C
C Draw all the labels.
C
              PRINT * , 'CALLING CTLBDR'
              CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
            END IF
C
            CALL CTGETI ('IWU - INTEGER WORKSPACE USED',IIWU)
            PRINT * , '  INTEGER WORKSPACE REQUIRED:      ',IIWU
C
            CALL CTGETI ('RWU -    REAL WORKSPACE USED',IRWU)
            PRINT * , '  REAL WORKSPACE REQUIRED:         ',IRWU
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
            CALL PLCHHQ (CFUX(.03),CFUY(.946),'ISCCP GRID',.024,0.,-1.)
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
            CALL PLCHHQ (CFUX(.97),CFUY(.904),'Dummy data are used.',
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
            PRINT * , '  SPACE REQUIRED IN AREA MAP:      ',
     +                                         IAMA(1)-IAMA(6)+IAMA(5)+1
C
            PRINT * , '  NUMBER OF POINTS IN LARGEST AREA:',MAXN
C
            CALL CTGETI ('IWU - INTEGER WORKSPACE USED',IIWU)
            PRINT * , '  INTEGER WORKSPACE REQUIRED:      ',IIWU
C
            CALL CTGETI ('RWU -    REAL WORKSPACE USED',IRWU)
            PRINT * , '  REAL WORKSPACE REQUIRED:         ',IRWU
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
            CALL PLCHHQ (CFUX(.03),CFUY(.946),'ISCCP GRID',.024,0.,-1.)
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
            CALL PLCHHQ (CFUX(.97),CFUY(.904),'Dummy data are used.',
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
            PRINT * , '  INTEGER WORKSPACE REQUIRED:      ',IIWU
C
            CALL CTGETI ('RWU -    REAL WORKSPACE USED',IRWU)
            PRINT * , '  REAL WORKSPACE REQUIRED:         ',IRWU
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
            CALL PLCHHQ (CFUX(.03),CFUY(.946),'ISCCP GRID',.024,0.,-1.)
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
     +                                                       .012,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.926),'TRIANGULAR MESH',
     +                                                       .012,0.,1.)
            CALL PLCHHQ (CFUX(.97),CFUY(.904),'Dummy data are used.',
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


      SUBROUTINE GTISC2 (RPNT,MPNT,NPNT,LOPN,
     +                   IEDG,MEDG,NEDG,LOEN,
     +                   ITRI,MTRI,NTRI,LOTN,
     +                   IPPP,IPPE,IDAT,XLAT,XLON)
C
        DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI),IPPP(2,*),IPPE(2,*)
C
C The ISCCP grid is an array of rectangles covering the cylindrical
C equidistant projection of the earth, at each center point of which
C we have latitude and longitude.  At each of the center points, we
C have available a topographic elevation, in meters above sea level.
C We can either use that data or generate dummy data at each of the
C specified center points.
C
C Declare variables to be used to read the ISCCP data.  The variable
C LINE just receives each of 6596 lines of data from the input file.
C
        CHARACTER*64 LINE
C
C Each line of data contains information about one of 6596 rectangular
C boxes on a cylindrical equidistant projection of the globe.  For each
C I from 1 to 6596, ILAT(I) is an integer between 1 and 72, specifying
C which of 72 rows box I is a part of and ILON(I) is an integer greater
C than 1 saying which of the boxes on that row box I is.  VLAT(I) and
C VLON(I) are the latitude and longitude, in degrees, of the center
C point of the box.  ZDAT(I) is a field value that we generate to use
C for test purposes.  ITOP(I) is the topographic height of the box, in
C meters.  For each IROW from 1 to 72, IIOR(IROW) is the index (between
C 1 and 6596) of the first box of the row IROW; NBIR(IROW) is the number
C of boxes on that row.
C
        COMMON /CISCCP/ ILAT(6596),ILON(6596),VLAT(6596),VLON(6596),
     +                  ZDAT(6596),ITOP(6596),IIOR(72),NBIR(72)
C
C For each I from 1 to 6596, XCOV(I), YCOV(I), and ZCOV(I) are the
C computed Cartesian coordinates of vertex I.
C
        DIMENSION XCOV(6596),YCOV(6596),ZCOV(6596)
C
C For each I from 1 to 6596 and for each J from to 1 to NOCP(I),
C CLAT(J,I) and CLON(J,I) are the computed latitude and longitude of
C one of the corner points of the polygonal cell centered at the vertex
C I and IADJ(I,J) is the index of one of the adjoining polygonal cells.
C ISRT is an index array allowing us to order the vertices in such a
C way as to permit efficient searches for adjoining cells.
C
        PARAMETER (MNCP=7)
C
        DIMENSION NOCP(6596),CLAT(MNCP,6596),CLON(MNCP,6596)
        DIMENSION ISRT(6596),IADJ(6596,MNCP)
C
C We make use of a routine (CTTMTL) that allows one to easily create an
C arbitrary triangular mesh.  The "tree-sorts" used by the method are
C very inefficient for objects that are partially ordered, so, as the
C triangles of the mesh are generated, they are stored in a triangle
C buffer from which they can be dumped in random order by calls to the
C routine CTTMTL.  The following declarations create an array to use
C as the buffer for the triangles.  Up to a point, making this buffer
C larger will result in more randomization of the triangles and speed
C up the process.  MBUF is the number of triangles that will fit in the
C buffer at once, and KBUF is the number of those to be dumped whenever
C the buffer is found to be full.  Because the call and loop set-up
C time for CTTMTL are non-trivial, it's better to dump a significant
C number of triangles while you're there, but I'm not sure what value
C might work best.  Use MBUF > KBUF > 1 and play with the values a bit
C to see what causes improvement.  (I tend to use numbers that are
C relatively prime, but I'm not sure if that makes any difference.)
C
        PARAMETER (MBUF=5021,KBUF=173)
C
        DIMENSION TBUF(12,MBUF)
C
C Define needed conversion constants.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Set a tolerance value used in the code that generates the polygonal
C patches.
C
        DATA EPSI / 1.E-2 /
C
C Initialize the arrays that keep track of the initial index and the
C number of boxes in each of the rows of the ISCCP data.
C
        DO 101 I=1,72
          IIOR(I)=0
          NBIR(I)=0
  101   CONTINUE
C
C Read the data from the ASCII file "ctiscp.dat", adding field data
C values to be associated with the center points of the rectangles.
C
        OPEN (11,FILE='ctiscp.dat',STATUS='OLD',FORM='FORMATTED')
C
C Initialize the dummy-data generator.
C
        IF (MOD(IDAT,10).NE.1) CALL GGDINI (-1.,1.,1234,.4)
C
C Skip the first two lines of the file.
C
        PRINT * , '  READING DATA FILE'
C
        READ (11,'(A64)') LINE
        READ (11,'(A64)') LINE
C
C Read through the rest of the file, saving certain pieces of data.
C For reference, the data look like this:
C
C  GRD LAT LON SQ1 SQ2     LAT      LON      BOX    %   TOPOG  VEG
C  BOX IND IND IND IND   CENTER   CENTER    AREA  LAND HEIGHT TYPE
C    1   1   1   1  48     1.25    60.00   80949   100   2895   8
C    2   1   2  49  96     1.25   180.00   80949   100   2865   8
C    3   1   3  97 144     1.25   300.00   80949   100   2621   8
C    4   2   1   1  16     3.75    20.00   80862   100   2834   8
C    .   .   .   .   .        .        .       .     .      .   .
C    .   .   .   .   .        .        .       .     .      .   .
C    .   .   .   .   .        .        .       .     .      .   .
C
C 6593  71   9 129 144   176.25   340.00   80862     0      0   0
C 6594  72   1   1  48   178.75    60.00   80949     0      0   0
C 6595  72   2  49  96   178.75   180.00   80949     0      0   0
C 6596  72   3  97 144   178.75   300.00   80949     0      0   0
C
        DO 102 I=1,6596
          READ (11,'(A64)') LINE
          READ (LINE,'(3I4,8X,2E9.0,14X,I7)')
     +                         J,ILAT(I),ILON(I),VLAT(I),VLON(I),ITOP(I)
          VLAT(I)=VLAT(I)-90.
          XCOV(I)=COS(DTOR*VLAT(I))*COS(DTOR*VLON(I))
          YCOV(I)=COS(DTOR*VLAT(I))*SIN(DTOR*VLON(I))
          ZCOV(I)=SIN(DTOR*VLAT(I))
          IF (J.NE.I) THEN
            PRINT * , '  GTISC2 - STOP - DATA FILE CORRUPTED?'
            STOP
          END IF
          IF (MOD(IDAT,10).EQ.1) THEN
            ZDAT(I)=REAL(ITOP(I))
          ELSE
            ZDAT(I)=GGDPNT(VLAT(I),VLON(I))
          END IF
          IF (IIOR(ILAT(I)).EQ.0) IIOR(ILAT(I))=I
          NBIR(ILAT(I))=NBIR(ILAT(I))+1
  102   CONTINUE
C
        CLOSE (11)
C
C Recompute the longitudes of the box centers.
C
        PRINT * , '  RECOMPUTING LONGITUDES'
C
        DO 103 I=1,6596
          VLON(I)=360.*(REAL(ILON(I))-.5)/REAL(NBIR(ILAT(I)))
  103   CONTINUE
C
C Define the polygonal patches surrounding each of the vertices.
C
        PRINT * , '  DEFINING POLYGONAL PATCHES'
C
        DO 106 I=1,6596
          SLCR=VLON(I)-180./REAL(NBIR(ILAT(I)))
          ELCR=VLON(I)+180./REAL(NBIR(ILAT(I)))
          NOCP(I)=0
          IF (ILAT(I).GT.1) THEN
            IF (NOCP(I).GE.MNCP) GO TO 901
            NOCP(I)=NOCP(I)+1
            CLAT(NOCP(I),I)=VLAT(I)-1.25
            CLON(NOCP(I),I)=SLCR
            DO 104 J=IIOR(ILAT(I)-1),
     +               IIOR(ILAT(I)-1)+NBIR(ILAT(I)-1)-1,
     +               1
              SLLR=VLON(J)-180./REAL(NBIR(ILAT(I)-1))
              ELLR=VLON(J)+180./REAL(NBIR(ILAT(I)-1))
              IF (SLLR.GT.SLCR-EPSI.AND.SLLR.LT.ELCR-EPSI) THEN
                IF (NOCP(I).GE.MNCP) GO TO 901
                NOCP(I)=NOCP(I)+1
                CLAT(NOCP(I),I)=VLAT(I)-1.25
                CLON(NOCP(I),I)=MAX(SLCR,MIN(ELCR,SLLR))
              END IF
  104       CONTINUE
          END IF
          IF (NOCP(I).GE.MNCP) GO TO 901
          NOCP(I)=NOCP(I)+1
          CLAT(NOCP(I),I)=VLAT(I)-1.25
          CLON(NOCP(I),I)=ELCR
          IF (ABS(CLAT(NOCP(I),I)+ 90.).LT.EPSI) CLON(NOCP(I),I)=0.
          IF (ABS(CLON(NOCP(I),I)-360.).LT.EPSI) CLON(NOCP(I),I)=0.
          IF (NOCP(I).GE.MNCP) GO TO 901
          NOCP(I)=NOCP(I)+1
          CLAT(NOCP(I),I)=VLAT(I)+1.25
          CLON(NOCP(I),I)=ELCR
          IF (ABS(CLAT(NOCP(I),I)- 90.).LT.EPSI) CLON(NOCP(I),I)=0.
          IF (ABS(CLON(NOCP(I),I)-360.).LT.EPSI) CLON(NOCP(I),I)=0.
          IF (ILAT(I).LT.72) THEN
            DO 105 J=IIOR(ILAT(I)+1)+NBIR(ILAT(I)+1)-1,
     +               IIOR(ILAT(I)+1),
     +               -1
              SLNR=VLON(J)-180./REAL(NBIR(ILAT(I)+1))
              ELNR=VLON(J)+180./REAL(NBIR(ILAT(I)+1))
              IF (ELNR.GT.SLCR+EPSI.AND.ELNR.LT.ELCR+EPSI) THEN
                IF (NOCP(I).GE.MNCP) GO TO 901
                NOCP(I)=NOCP(I)+1
                CLAT(NOCP(I),I)=VLAT(I)+1.25
                CLON(NOCP(I),I)=MAX(SLCR,MIN(ELCR,ELNR))
                IF (CLON(NOCP(I),I).GT.360.-EPSI) CLON(NOCP(I),I)=0.
              END IF
  105       CONTINUE
            IF (NOCP(I).GE.MNCP) GO TO 901
            NOCP(I)=NOCP(I)+1
            CLAT(NOCP(I),I)=VLAT(I)+1.25
            CLON(NOCP(I),I)=SLCR
          END IF
  106   CONTINUE
C
C Write some debug stuff for "Debug1.f" to read.
C
c       WRITE (11,*) NOCP
c       WRITE (11,*) CLAT
c       WRITE (11,*) CLON
C
c       PRINT * , '  DEBUG INFO WRITTEN'
C
C Set up the adjacency index array.  We order the polygonal patches by
C latitude, making it possible to look for matching edges in nearby
C polygonal patches first.  (If we're trying to find a match for a
C side of the Ith polygonal patch (sorted order), we look at polygonal
C patches for IC = I+1, I-1, I+2, I-2, and so on, until we find the one
C we want.  If IC becomes less than 1 or greater than 6596, we just
C finish out the rest of the list by stepping through the remaining
C elements on the opposite end of it; IM implements the logic to manage
C this.
C
        PRINT * , '  GETTING SORTED INDEX ARRAY'
C
        CALL CTSORT (VLAT,6596,ISRT)
C
        PRINT * , '  SETTING UP ADJACENCY ARRAY'
C
        NOZP=0
C
        DO 110 I=1,6596
          DO 109 J=1,NOCP(ISRT(I))
            JM1=MOD(J+NOCP(ISRT(I))-2,NOCP(ISRT(I)))+1
            IC=I
            IM=0
            DO 108 K=1,6595
              IF (IM.EQ.0) THEN
                IF (MOD(K,2).EQ.0) THEN
                  IC=IC-K
                  IF (IC.LT.1) THEN
                    IC=IC+K+1
                    IM=+1
                  END IF
                ELSE
                  IC=IC+K
                  IF (IC.GT.6596) THEN
                    IC=IC-K-1
                    IM=-1
                  END IF
                END IF
              ELSE
                IC=IC+IM
              END IF
              DO 107 L=1,NOCP(ISRT(IC))
                LP1=MOD(L,NOCP(ISRT(IC)))+1
                IF (ABS(CLAT(J  ,ISRT(I ))-
     +                  CLAT(L  ,ISRT(IC))).LT.EPSI.AND.
     +              ABS(CLON(J  ,ISRT(I ))-
     +                  CLON(L  ,ISRT(IC))).LT.EPSI.AND.
     +              ABS(CLAT(JM1,ISRT(I ))-
     +                  CLAT(LP1,ISRT(IC))).LT.EPSI.AND.
     +              ABS(CLON(JM1,ISRT(I ))-
     +                  CLON(LP1,ISRT(IC))).LT.EPSI) THEN
                  IADJ(ISRT(I),J)=ISRT(IC)
                  GO TO 109
                END IF
  107         CONTINUE
  108       CONTINUE
            IADJ(ISRT(I),J)=0
            NOZP=NOZP+1
  109     CONTINUE
  110   CONTINUE
C
        IF (NOZP.NE.0) PRINT * , '  NUMBER OF ZERO POINTERS IS ',NOZP
C
C Write some debug stuff for "Debug2.f" to read.
C
c       WRITE (11,*) NOCP
c       WRITE (11,*) VLAT
c       WRITE (11,*) VLON
c       WRITE (11,*) IADJ
C
c       PRINT * , '  DEBUG INFO WRITTEN'
C
C Write some debug stuff for "Debug3.f" to read.
C
c       WRITE (11,*) VLAT
c       WRITE (11,*) VLON
c       WRITE (11,*) NOCP
c       WRITE (11,*) CLAT
c       WRITE (11,*) CLON
c       WRITE (11,*) IADJ
C
c       PRINT * , '  DEBUG INFO WRITTEN'
C
C Build structures forming the triangular mesh.  First, initialize the
C variables used to keep track of items in the sort arrays for points
C and edges, in the triangle buffer, and in the triangle list.
C
        PRINT * , '  BUILDING THE TRIANGULAR MESH'
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
C Loop through the vertices of the grid.
C
        DO 112 I=1,6596
C
C Set the index of vertex 1 of the triangle.
C
          IOV1=I
C
C Examine all of the triangles meeting at vertex IOV1, taking into
C account that the vertices are given in counterclockwise order.
C
          DO 111 J=1,NOCP(I)
C
            JP1=MOD(J,NOCP(I))+1
C
C Set the indices of vertices 2 and 3 of the triangle.
C
            IOV2=IADJ(I,J)
            IOV3=IADJ(I,JP1)
            if (iov2.eq.0.or.iov3.eq.0) go to 111
C
C Process a triangle only if the index of the 1st vertex is less than
C the indices of the other two vertices.  This will ensure that each
C triangle is processed only once, even though we see it three times.
C
            IF (IOV1.LT.IOV2.AND.IOV1.LT.IOV3) THEN
C
C If the triangle buffer is full, dump a few randomly-chosen triangles
C from it to make room for the new one.
C
              IF (NBUF.EQ.MBUF) THEN
                CALL CTTMTL (KBUF,TBUF,MBUF,NBUF,
     +                       IPPP,MPPP,NPPP,
     +                       IPPE,MPPE,NPPE,
     +                       RPNT,MPNT,NPNT,LOPN,
     +                       IEDG,MEDG,NEDG,LOEN,
     +                       ITRI,MTRI,NTRI,LOTN)
              END IF
C
C Put the new triangle in the triangle buffer.  For the "data", use the
C real equivalent of an index that will later allow us to fill in the
C appropriate data values and determine which triangles are blocked.
C
              NBUF=NBUF+1
C
              TBUF( 1,NBUF)=XCOV(IOV1)
              TBUF( 2,NBUF)=YCOV(IOV1)
              TBUF( 3,NBUF)=ZCOV(IOV1)
              TBUF( 4,NBUF)=REAL(IOV1)
C
              TBUF( 5,NBUF)=XCOV(IOV2)
              TBUF( 6,NBUF)=YCOV(IOV2)
              TBUF( 7,NBUF)=ZCOV(IOV2)
              TBUF( 8,NBUF)=REAL(IOV2)
C
              TBUF( 9,NBUF)=XCOV(IOV3)
              TBUF(10,NBUF)=YCOV(IOV3)
              TBUF(11,NBUF)=ZCOV(IOV3)
              TBUF(12,NBUF)=REAL(IOV3)
C
            END IF
C
  111     CONTINUE
C
  112   CONTINUE
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
C Determine which triangles should be blocked and block them.
C
        DO 113 I=0,NTRI-LOTN,LOTN
C
C IOP1 is the base index of the point common to edges 1 and 2.
C
          IF (IEDG(ITRI(I+1)+1).EQ.IEDG(ITRI(I+2)+1).OR.
     +        IEDG(ITRI(I+1)+1).EQ.IEDG(ITRI(I+2)+2)) THEN
            IOP1=IEDG(ITRI(I+1)+1)
          ELSE
            IOP1=IEDG(ITRI(I+1)+2)
          END IF
C
C IOP1 is the base index of the point common to edges 2 and 3.
C
          IF (IEDG(ITRI(I+2)+1).EQ.IEDG(ITRI(I+3)+1).OR.
     +        IEDG(ITRI(I+2)+1).EQ.IEDG(ITRI(I+3)+2)) THEN
            IOP2=IEDG(ITRI(I+2)+1)
          ELSE
            IOP2=IEDG(ITRI(I+2)+2)
          END IF
C
C IOP3 is the base index of the point common to edges 3 and 1.
C
          IF (IEDG(ITRI(I+3)+1).EQ.IEDG(ITRI(I+1)+1).OR.
     +        IEDG(ITRI(I+3)+1).EQ.IEDG(ITRI(I+1)+2)) THEN
            IOP3=IEDG(ITRI(I+3)+1)
          ELSE
            IOP3=IEDG(ITRI(I+3)+2)
          END IF
C
C Block the triangle if so directed by the value of IDAT.
C
          IF      (MOD(IDAT/10,10).EQ.1) THEN
C
C Land is blocked.
C
            NOBV=0
            IF (ITOP(INT(RPNT(IOP1+4))).NE.0) NOBV=NOBV+1
            IF (ITOP(INT(RPNT(IOP2+4))).NE.0) NOBV=NOBV+1
            IF (ITOP(INT(RPNT(IOP3+4))).NE.0) NOBV=NOBV+1
            IF (NOBV.GT.IDAT/100) ITRI(I+4)=1
C
          ELSE IF (MOD(IDAT/10,10).EQ.2) THEN
C
C Ocean is blocked.
C
            NOBV=0
            IF (ITOP(INT(RPNT(IOP1+4))).EQ.0) NOBV=NOBV+1
            IF (ITOP(INT(RPNT(IOP2+4))).EQ.0) NOBV=NOBV+1
            IF (ITOP(INT(RPNT(IOP3+4))).EQ.0) NOBV=NOBV+1
            IF (NOBV.GT.IDAT/100) ITRI(I+4)=1
C
          END IF
C
  113   CONTINUE
C
C Now that we don't need the pointers that were stored as "data", swap
C in the proper data values.  Save each pointer in the extra cell in
C the point definition.
C
        DO 114 I=0,NPNT-LOPN,LOPN
          RPNT(I+5)=RPNT(I+4)
          RPNT(I+4)=ZDAT(INT(RPNT(I+4)))
  114   CONTINUE
C
C Return the latitude and longitude of the approximate center point of
C the mesh on the globe.
C
        XLAT=0.
        XLON=0.
C
C Done.
C
        RETURN
C
C Error exit.
C
  901   PRINT * , '  GTISC2 - STOP - TOO MANY ADJOINING PATCHES FOUND'
        STOP
C
      END


      SUBROUTINE DSISC2
C
C Draw the original ISCCP grid, using data passed in a common block.
C
        COMMON /CISCCP/ ILAT(6596),ILON(6596),VLAT(6596),VLON(6596),
     +                  ZDAT(6596),ITOP(6596),IIOR(72),NBIR(72)
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (8)
C
        DO 101 I=1,6596
          SLAT=VLAT(I)-1.25
          ELAT=VLAT(I)+1.25
          SLON=VLON(I)-180./REAL(NBIR(ILAT(I)))
          ELON=VLON(I)+180./REAL(NBIR(ILAT(I)))
          IF (ILAT(I).NE.1) CALL DLOCLA (SLAT,SLON,SLAT,ELON)
          CALL DLOCLO (SLAT,SLON,ELAT,SLON)
  101   CONTINUE
C
C Draw the continental outlines and lat/lon grid.
C
        CALL PLOTIT (0,0,2)
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


      SUBROUTINE DLOCLA (ALAT,ALON,BLAT,BLON)
C
C (DLOCLA = Draw Line Of Constant LAtitude)
C
C This routine draws a line of constant latitude joining two points on
C the globe.
C
        PARAMETER (MPTS=181,SIZE=1.,DTOR=.017453292519943)
C
        NPTS=MAX(0,MIN(MPTS,INT(COS(DTOR*ALAT)*ABS(BLON-ALON)/SIZE)))
C
        CALL MAPIT (ALAT,ALON,0)
C
        DO 101 I=1,NPTS
          CALL MAPIT (ALAT,ALON+(REAL(I)/REAL(NPTS+1))*(BLON-ALON),1)
  101   CONTINUE
C
        CALL MAPIT (BLAT,BLON,2)
C
        CALL MAPIQ
C
        RETURN
C
      END


      SUBROUTINE DLOCLO (ALAT,ALON,BLAT,BLON)
C
C (DLOCLO = Draw Line Of Constant LOngitude)
C
C This routine draws a line of constant longitude joining two points on
C the globe.
C
        PARAMETER (MPTS=181,SIZE=1.)
C
        NPTS=MAX(0,MIN(MPTS,INT(ABS(BLAT-ALAT)/SIZE)))
C
        CALL MAPIT (ALAT,ALON,0)
C
        DO 101 I=1,NPTS
          CALL MAPIT (ALAT+(REAL(I)/REAL(NPTS+1))*(BLAT-ALAT),ALON,1)
  101   CONTINUE
C
        CALL MAPIT (BLAT,BLON,2)
C
        CALL MAPIQ
C
        RETURN
C
      END


      SUBROUTINE GGDINI (DLOW,DHGH,NRND,FRCT)
C
C The routines GGDINI and GGDPNT are used to generate test data on the
C globe, using a method which is essentially fractal and is based on
C successive elaborations of an icosahedron.
C
C To initialize the generation of a particular data field, execute the
C statement
C
C      CALL GGDINI (DLOW,DHGH,NRND,FRCT)
C
C DLOW is the desired minimum value and DHGH the desired maximum value.
C NRND is the number of random numbers (from the function FRAN) to be
C discarded before beginning to generate the field.  FRCT is a fraction
C between -1. and 1. which affects the "roughness" of the generated
C data; values near 0. will give smoother values than values further
C from 0.
C
C After a call to GGDINI, the function GGDPNT returns the value of the
C generated data field at the point (RLAT,RLON); the angles RLAT and
C RLON are given in radians.
C
C Parameters determine how many elaboration steps will be used (NELA)
C and therefore how many faces, edges, and vertices will be used (MFCE,
C MEDG, and MVTX, respectively).  Choose values from the table which
C follows:
C
C     NELA      MFCE      MEDG      MVTX
C     ----      ----      ----      ----
C        0        20        30        12
C        1       100       150        42
C        2       420       630       162
C        3      1700      2550       642
C        4      6820     10230      2562
C        5     27300     40950     10242
C        6    109220    163830     40962
C
C       PARAMETER(NELA=5,MFCE= 27300,MEDG= 40950,MVTX=10242,NELP=NELA+1)
        PARAMETER(NELA=6,MFCE=109220,MEDG=163830,MVTX=40962,NELP=NELA+1)
C
C NOTE:  A COPY OF THE ABOVE PARAMETER STATEMENT MUST BE PLACED IN THE
C FUNCTION GGDPNT, AS WELL.
C
C Information about the structure of the elaborated icosahedron is
C stored in variables in the labelled common block GGDCMN.  Each of
C NFCE entries in IFCE defines a triangular face; it consists of three
C pointers to elements of IEDG plus another element which, if non-zero,
C points to the faces into which that face was broken by the next
C elaboration step.  Each of NEDG entries in IEDG defines an edge;
C the first two elements are pointers to elements of VRTX, and the
C second two elements, if non-zero, are pointers back into IEDG, to
C the two edges into which the edge was broken by the next elaboration
C step.  Each of NVTX entries in VRTX defines a vertex, including the
C position and the data value associated with it.
C
C The arrays LFCE, LEDG, and LVTX point to the beginning and ending of
C the faces, edges, and vertices, respectively, added by the step whose
C number is given by the second index.
C
C RLOW and RHGH are just copies of the user's DLOW and DHGH; RMIN and
C RMAX are the actual minimum and maximum value of the data values
C stored in the array VRTX.
C
        COMMON /GGDCMN/ NFCE,IFCE(4,MFCE),NEDG,IEDG(4,MEDG),
     +                  NVTX,VRTX(4,MVTX),
     +                  LFCE(2,NELP),LEDG(2,NELP),LVTX(2,NELP),
     +                  RLOW,RHGH,RMIN,RMAX
        SAVE   /GGDCMN/
C
C Declare local arrays which are used for initialization.
C
        DIMENSION JFCE(3,20),JEDG(2,30),XCVI(12),YCVI(12),ZCVI(12)
C
C Define the twenty faces of an icosahedron (pointers to edges).
C
        DATA JFCE(1, 1),JFCE(2, 1),JFCE(3, 1) /  1,11, 2 /
        DATA JFCE(1, 2),JFCE(2, 2),JFCE(3, 2) /  2,19, 3 /
        DATA JFCE(1, 3),JFCE(2, 3),JFCE(3, 3) /  3,25, 4 /
        DATA JFCE(1, 4),JFCE(2, 4),JFCE(3, 4) /  4,29, 5 /
        DATA JFCE(1, 5),JFCE(2, 5),JFCE(3, 5) /  1,14, 5 /
        DATA JFCE(1, 6),JFCE(2, 6),JFCE(3, 6) / 12,28,14 /
        DATA JFCE(1, 7),JFCE(2, 7),JFCE(3, 7) / 12,27,13 /
        DATA JFCE(1, 8),JFCE(2, 8),JFCE(3, 8) / 11,20,13 /
        DATA JFCE(1, 9),JFCE(2, 9),JFCE(3, 9) / 20,30,21 /
        DATA JFCE(1,10),JFCE(2,10),JFCE(3,10) / 19,26,21 /
        DATA JFCE(1,11),JFCE(2,11),JFCE(3,11) / 16,18,26 /
        DATA JFCE(1,12),JFCE(2,12),JFCE(3,12) / 16,25,17 /
        DATA JFCE(1,13),JFCE(2,13),JFCE(3,13) / 15,23,17 /
        DATA JFCE(1,14),JFCE(2,14),JFCE(3,14) / 23,29,24 /
        DATA JFCE(1,15),JFCE(2,15),JFCE(3,15) / 22,28,24 /
        DATA JFCE(1,16),JFCE(2,16),JFCE(3,16) /  7, 8,22 /
        DATA JFCE(1,17),JFCE(2,17),JFCE(3,17) /  6,15, 7 /
        DATA JFCE(1,18),JFCE(2,18),JFCE(3,18) /  6,18,10 /
        DATA JFCE(1,19),JFCE(2,19),JFCE(3,19) /  9,30,10 /
        DATA JFCE(1,20),JFCE(2,20),JFCE(3,20) /  8,27, 9 /
C
C Define the thirty edges of the icosahedron (pointers to vertices).
C
        DATA JEDG(1, 1),JEDG(2, 1) /  1, 3 /
        DATA JEDG(1, 2),JEDG(2, 2) /  1, 5 /
        DATA JEDG(1, 3),JEDG(2, 3) /  1, 7 /
        DATA JEDG(1, 4),JEDG(2, 4) /  1, 9 /
        DATA JEDG(1, 5),JEDG(2, 5) /  1,11 /
        DATA JEDG(1, 6),JEDG(2, 6) /  2, 4 /
        DATA JEDG(1, 7),JEDG(2, 7) /  2, 6 /
        DATA JEDG(1, 8),JEDG(2, 8) /  2, 8 /
        DATA JEDG(1, 9),JEDG(2, 9) /  2,10 /
        DATA JEDG(1,10),JEDG(2,10) /  2,12 /
        DATA JEDG(1,11),JEDG(2,11) /  3, 5 /
        DATA JEDG(1,12),JEDG(2,12) /  3, 8 /
        DATA JEDG(1,13),JEDG(2,13) /  3,10 /
        DATA JEDG(1,14),JEDG(2,14) /  3,11 /
        DATA JEDG(1,15),JEDG(2,15) /  4, 6 /
        DATA JEDG(1,16),JEDG(2,16) /  4, 7 /
        DATA JEDG(1,17),JEDG(2,17) /  4, 9 /
        DATA JEDG(1,18),JEDG(2,18) /  4,12 /
        DATA JEDG(1,19),JEDG(2,19) /  5, 7 /
        DATA JEDG(1,20),JEDG(2,20) /  5,10 /
        DATA JEDG(1,21),JEDG(2,21) /  5,12 /
        DATA JEDG(1,22),JEDG(2,22) /  6, 8 /
        DATA JEDG(1,23),JEDG(2,23) /  6, 9 /
        DATA JEDG(1,24),JEDG(2,24) /  6,11 /
        DATA JEDG(1,25),JEDG(2,25) /  7, 9 /
        DATA JEDG(1,26),JEDG(2,26) /  7,12 /
        DATA JEDG(1,27),JEDG(2,27) /  8,10 /
        DATA JEDG(1,28),JEDG(2,28) /  8,11 /
        DATA JEDG(1,29),JEDG(2,29) /  9,11 /
        DATA JEDG(1,30),JEDG(2,30) / 10,12 /
C
C Define the vertices of the icosahedron (note radius less than one).
C
        DATA XCVI / .9510565162952 , -.9510565162951 ,  .4253254041760 ,
     +             -.4253254041760 ,  .4253254041760 , -.4253254041760 ,
     +              .4253254041760 , -.4253254041760 ,  .4253254041760 ,
     +             -.4253254041760 ,  .4253254041760 , -.4253254041760 /
        DATA YCVI / .0000000000000 ,  .0000000000000 ,  .8506508083520 ,
     +             -.8506508083520 ,  .2628655560596 , -.2628655560596 ,
     +             -.6881909602356 ,  .6881909602356 , -.6881909602356 ,
     +              .6881909602356 ,  .2628655560595 , -.2628655560596 /
        DATA ZCVI / .0000000000000 ,  .0000000000000 ,  .0000000000000 ,
     +              .0000000000000 ,  .8090169943749 , -.8090169943749 ,
     +              .5000000000000 , -.5000000000000 , -.5000000000000 ,
     +              .5000000000000 , -.8090169943749 ,  .8090169943749 /
C
C Define constants for converting from degrees to radians and back.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Spin up the random number generator.
C
        DO 101 I=1,NRND
          TEMP=FRAN()
  101   CONTINUE
C
C Initialize the list of faces.
C
        NFCE=20
C
        DO 102 I=1,NFCE
          IFCE(1,I)=JFCE(1,I)
          IFCE(2,I)=JFCE(2,I)
          IFCE(3,I)=JFCE(3,I)
          IFCE(4,I)=0
  102   CONTINUE
C
C Initialize the list of edges.
C
        NEDG=30
C
        DO 103 I=1,NEDG
          IEDG(1,I)=JEDG(1,I)
          IEDG(2,I)=JEDG(2,I)
          IEDG(3,I)=0
          IEDG(4,I)=0
  103   CONTINUE
C
C Initialize the list of vertices.
C
        NVTX=12
C
        DO 104 I=1,NVTX
          TEMP=SQRT(XCVI(I)**2+YCVI(I)**2+ZCVI(I)**2)
          VRTX(1,I)=XCVI(I)/TEMP
          VRTX(2,I)=YCVI(I)/TEMP
          VRTX(3,I)=ZCVI(I)/TEMP
          VRTX(4,I)=FRAN()
  104   CONTINUE
C
C Define pointers to the existing faces, edges, and vertices.
C
        LFCE(1,1)=1
        LFCE(2,1)=NFCE
C
        LEDG(1,1)=1
        LEDG(2,1)=NEDG
C
        LVTX(1,1)=1
        LVTX(2,1)=NVTX
C
C Define the initial multiplier for the random number generator.
C
        RMUL=1.
C
C Do a number of successive elaboration steps.
C
        DO 108 IELA=2,NELP
C
C Update pointers which will tell us what faces, edges, and vertices
C were added during this step.
C
          LFCE(1,IELA)=NFCE+1
          LEDG(1,IELA)=NEDG+1
          LVTX(1,IELA)=NVTX+1
C
C Change the multiplier for the random number generator.
C
          RMUL=FRCT*RMUL
C
C Add a vertex at the midpoint of each edge added during the last step.
C
          DO 105 I=LEDG(1,IELA-1),LEDG(2,IELA-1)
            NVTX=NVTX+1
            IF (NVTX.GT.MVTX) GO TO 901
            VRTX(1,NVTX)=.5*(VRTX(1,IEDG(1,I))+VRTX(1,IEDG(2,I)))
            VRTX(2,NVTX)=.5*(VRTX(2,IEDG(1,I))+VRTX(2,IEDG(2,I)))
            VRTX(3,NVTX)=.5*(VRTX(3,IEDG(1,I))+VRTX(3,IEDG(2,I)))
            VRTX(4,NVTX)=.5*(VRTX(4,IEDG(1,I))+VRTX(4,IEDG(2,I)))
            TEMP=SQRT(VRTX(1,NVTX)**2+VRTX(2,NVTX)**2+VRTX(3,NVTX)**2)
            VRTX(1,NVTX)=VRTX(1,NVTX)/TEMP
            VRTX(2,NVTX)=VRTX(2,NVTX)/TEMP
            VRTX(3,NVTX)=VRTX(3,NVTX)/TEMP
            NEDG=NEDG+2
            IF (NEDG.GT.MEDG) GO TO 902
            IEDG(3,I)=NEDG-1
            IEDG(4,I)=NEDG
            IEDG(1,NEDG-1)=IEDG(1,I)
            IEDG(2,NEDG-1)=NVTX
            IEDG(3,NEDG-1)=0
            IEDG(4,NEDG-1)=0
            IEDG(1,NEDG  )=NVTX
            IEDG(2,NEDG  )=IEDG(2,I)
            IEDG(3,NEDG  )=0
            IEDG(4,NEDG  )=0
  105     CONTINUE
C
C Modify the data values at each of the vertices of the new object.
C
          DO 106 I=1,NVTX
            VRTX(4,I)=VRTX(4,I)+RMUL*FRAN()
  106     CONTINUE
C
C Break each face from the last step into four new ones and add them
C back into the list.
C
          DO 107 I=LFCE(1,IELA-1),LFCE(2,IELA-1)
            IFCE(4,I)=NFCE+1
            NEDG=NEDG+3
            IF (NEDG.GT.MEDG) GO TO 902
            IEDG(1,NEDG-2)=IEDG(2,IEDG(3,IFCE(1,I)))
            IEDG(2,NEDG-2)=IEDG(2,IEDG(3,IFCE(2,I)))
            IEDG(3,NEDG-2)=0
            IEDG(4,NEDG-2)=0
            IEDG(1,NEDG-1)=IEDG(2,IEDG(3,IFCE(2,I)))
            IEDG(2,NEDG-1)=IEDG(2,IEDG(3,IFCE(3,I)))
            IEDG(3,NEDG-1)=0
            IEDG(4,NEDG-1)=0
            IEDG(1,NEDG  )=IEDG(2,IEDG(3,IFCE(3,I)))
            IEDG(2,NEDG  )=IEDG(2,IEDG(3,IFCE(1,I)))
            IEDG(3,NEDG  )=0
            IEDG(4,NEDG  )=0
            NFCE=NFCE+4
            IF (NFCE.GT.MFCE) GO TO 903
            IF (IEDG(1,IFCE(1,I)).EQ.IEDG(1,IFCE(2,I))) THEN
              IF (IEDG(2,IFCE(1,I)).EQ.IEDG(1,IFCE(3,I))) THEN
                IFCE(1,NFCE-3)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-3)=IEDG(3,IFCE(2,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(4,IFCE(2,I))
                IFCE(2,NFCE-2)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(3,IFCE(3,I))
                IFCE(2,NFCE-1)=IEDG(4,IFCE(1,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              ELSE
                IFCE(1,NFCE-3)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-3)=IEDG(3,IFCE(2,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(3,IFCE(3,I))
                IFCE(2,NFCE-2)=IEDG(4,IFCE(2,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(4,IFCE(1,I))
                IFCE(2,NFCE-1)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              END IF
            ELSE IF (IEDG(1,IFCE(1,I)).EQ.IEDG(2,IFCE(2,I))) THEN
              IF (IEDG(2,IFCE(1,I)).EQ.IEDG(1,IFCE(3,I))) THEN
                IFCE(1,NFCE-3)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-3)=IEDG(4,IFCE(2,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(3,IFCE(2,I))
                IFCE(2,NFCE-2)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(3,IFCE(3,I))
                IFCE(2,NFCE-1)=IEDG(4,IFCE(1,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              ELSE
                IFCE(1,NFCE-3)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-3)=IEDG(4,IFCE(2,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(3,IFCE(2,I))
                IFCE(2,NFCE-2)=IEDG(3,IFCE(3,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(4,IFCE(1,I))
                IFCE(2,NFCE-1)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              END IF
            ELSE IF (IEDG(1,IFCE(1,I)).EQ.IEDG(1,IFCE(3,I))) THEN
              IF (IEDG(2,IFCE(1,I)).EQ.IEDG(1,IFCE(2,I))) THEN
                IFCE(1,NFCE-3)=IEDG(3,IFCE(2,I))
                IFCE(2,NFCE-3)=IEDG(4,IFCE(1,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(4,IFCE(2,I))
                IFCE(2,NFCE-2)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-1)=IEDG(3,IFCE(3,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              ELSE
                IFCE(1,NFCE-3)=IEDG(4,IFCE(1,I))
                IFCE(2,NFCE-3)=IEDG(4,IFCE(2,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(3,IFCE(2,I))
                IFCE(2,NFCE-2)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-1)=IEDG(3,IFCE(3,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              END IF
            ELSE
              IF (IEDG(2,IFCE(1,I)).EQ.IEDG(1,IFCE(2,I))) THEN
                IFCE(1,NFCE-3)=IEDG(3,IFCE(2,I))
                IFCE(2,NFCE-3)=IEDG(4,IFCE(1,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(3,IFCE(3,I))
                IFCE(2,NFCE-2)=IEDG(4,IFCE(2,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-1)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              ELSE
                IFCE(1,NFCE-3)=IEDG(4,IFCE(1,I))
                IFCE(2,NFCE-3)=IEDG(4,IFCE(2,I))
                IFCE(3,NFCE-3)=NEDG-2
                IFCE(4,NFCE-3)=0
                IFCE(1,NFCE-2)=IEDG(3,IFCE(2,I))
                IFCE(2,NFCE-2)=IEDG(3,IFCE(3,I))
                IFCE(3,NFCE-2)=NEDG-1
                IFCE(4,NFCE-2)=0
                IFCE(1,NFCE-1)=IEDG(3,IFCE(1,I))
                IFCE(2,NFCE-1)=IEDG(4,IFCE(3,I))
                IFCE(3,NFCE-1)=NEDG
                IFCE(4,NFCE-1)=0
              END IF
            END IF
            IFCE(1,NFCE  )=NEDG-2
            IFCE(2,NFCE  )=NEDG-1
            IFCE(3,NFCE  )=NEDG
            IFCE(4,NFCE  )=0
  107     CONTINUE
C
C Update pointers which will tell us what faces, edges, and vertices
C were added during this step.
C
          LFCE(2,IELA)=NFCE
          LEDG(2,IELA)=NEDG
          LVTX(2,IELA)=NVTX
C
C End of elaboration step.
C
  108   CONTINUE
C
C Copy the desired lowest and highest values to COMMON.
C
        RLOW=DLOW
        RHGH=DHGH
C
C Compute the minimum and maximum value in the data.
C
        RMIN=+1.E36
        RMAX=-1.E36
C
        DO 109 I=1,NVTX
          RMIN=MIN(RMIN,VRTX(4,I))
          RMAX=MAX(RMAX,VRTX(4,I))
  109   CONTINUE
C
C Done.
C
        RETURN
C
C Error exits.
C
  901   PRINT * , 'STOP - GGDINI - TOO MANY VERTICES'
        STOP
C
  902   PRINT * , 'STOP - GGDINI - TOO MANY EDGES'
        STOP
C
  903   PRINT * , 'STOP - GGDINI - TOO MANY FACES'
        STOP
C
      END


      FUNCTION GGDPNT (RLAT,RLON)
C
C Following is a copy of the PARAMETER statement from GGDINI.
C
        PARAMETER(NELA=6,MFCE=109220,MEDG=163830,MVTX=40962,NELP=NELA+1)
C
C Define the common block for the package.
C
        COMMON /GGDCMN/ NFCE,IFCE(4,MFCE),NEDG,IEDG(4,MEDG),
     +                  NVTX,VRTX(4,MVTX),
     +                  LFCE(2,NELP),LEDG(2,NELP),LVTX(2,NELP),
     +                  RLOW,RHGH,RMIN,RMAX
        SAVE   /GGDCMN/
C
C Define constants for converting from degrees to radians and back.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Compute the position of the point on the surface of the unit sphere.
C
        X0=COS(DTOR*RLAT)*COS(DTOR*RLON)
        Y0=COS(DTOR*RLAT)*SIN(DTOR*RLON)
        Z0=SIN(DTOR*RLAT)
C
C Find out what face that point falls above.
C
        IFST=1
        NUMB=20
        ITRY=1
C
  101   CONTINUE
          IPOS=IFST
  102     CONTINUE
            X1=VRTX(1,IEDG(1,IFCE(1,IPOS)))
            Y1=VRTX(2,IEDG(1,IFCE(1,IPOS)))
            Z1=VRTX(3,IEDG(1,IFCE(1,IPOS)))
            W1=VRTX(4,IEDG(1,IFCE(1,IPOS)))
            X2=VRTX(1,IEDG(2,IFCE(1,IPOS)))
            Y2=VRTX(2,IEDG(2,IFCE(1,IPOS)))
            Z2=VRTX(3,IEDG(2,IFCE(1,IPOS)))
            W2=VRTX(4,IEDG(2,IFCE(1,IPOS)))
            IF (IEDG(1,IFCE(2,IPOS)).NE.IEDG(1,IFCE(1,IPOS)).AND.
     +          IEDG(1,IFCE(2,IPOS)).NE.IEDG(2,IFCE(1,IPOS))) THEN
              X3=VRTX(1,IEDG(1,IFCE(2,IPOS)))
              Y3=VRTX(2,IEDG(1,IFCE(2,IPOS)))
              Z3=VRTX(3,IEDG(1,IFCE(2,IPOS)))
              W3=VRTX(4,IEDG(1,IFCE(2,IPOS)))
            ELSE
              X3=VRTX(1,IEDG(2,IFCE(2,IPOS)))
              Y3=VRTX(2,IEDG(2,IFCE(2,IPOS)))
              Z3=VRTX(3,IEDG(2,IFCE(2,IPOS)))
              W3=VRTX(4,IEDG(2,IFCE(2,IPOS)))
            END IF
            A1=Y3*Z2-Y2*Z3
            B1=X2*Z3-X3*Z2
            C1=X3*Y2-X2*Y3
            A2=Y1*Z3-Y3*Z1
            B2=X3*Z1-X1*Z3
            C2=X1*Y3-X3*Y1
            A3=Y2*Z1-Y1*Z2
            B3=X1*Z2-X2*Z1
            C3=X2*Y1-X1*Y2
            IF (ITRY.EQ.1) THEN
              XM=(X1+X2+X3)/3.
              YM=(Y1+Y2+Y3)/3.
              ZM=(Z1+Z2+Z3)/3.
              S1=SIGN(1.,A1*XM+B1*YM+C1*ZM)
              S2=SIGN(1.,A2*XM+B2*YM+C2*ZM)
              S3=SIGN(1.,A3*XM+B3*YM+C3*ZM)
              T1=SIGN(1.,A1*X0+B1*Y0+C1*Z0)
              T2=SIGN(1.,A2*X0+B2*Y0+C2*Z0)
              T3=SIGN(1.,A3*X0+B3*Y0+C3*Z0)
              IF (S1.EQ.T1.AND.S2.EQ.T2.AND.S3.EQ.T3) GO TO 103
            ELSE
              T1=2.*((X0-X2)*(X3-X2)+(Y0-Y2)*(Y3-Y2)+(Z0-Z2)*(Z3-Z2))/
     +              ((X3-X2)*(X3-X2)+(Y3-Y2)*(Y3-Y2)+(Z3-Z2)*(Z3-Z2))
              T1=MAX(0.,MIN(1.,T1))
              D1=(X2-X0+(X3-X2)*T1)**2+(Y2-Y0+(Y3-Y2)*T1)**2+
     +                                 (Z2-Z0+(Z3-Z2)*T1)**2
              T2=2.*((X0-X3)*(X1-X3)+(Y0-Y3)*(Y1-Y3)+(Z0-Z3)*(Z1-Z3))/
     +              ((X1-X3)*(X1-X3)+(Y1-Y3)*(Y1-Y3)+(Z1-Z3)*(Z1-Z3))
              T2=MAX(0.,MIN(1.,T2))
              D2=(X3-X0+(X1-X3)*T2)**2+(Y3-Y0+(Y1-Y3)*T2)**2+
     +                                 (Z3-Z0+(Z1-Z3)*T2)**2
              T3=2.*((X0-X1)*(X2-X1)+(Y0-Y1)*(Y2-Y1)+(Z0-Z1)*(Z2-Z1))/
     +              ((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
              T3=MAX(0.,MIN(1.,T3))
              D3=(X1-X0+(X2-X1)*T3)**2+(Y1-Y0+(Y2-Y1)*T3)**2+
     +                                 (Z1-Z0+(Z2-Z1)*T3)**2
              DIST=MIN(D1,D2,D3)
              IF (IBST.EQ.0.OR.DIST.LT.DBST) THEN
                IBST=IPOS
                DBST=DIST
              END IF
            END IF
            IF (IPOS.EQ.IFST+NUMB-1) THEN
              IF (ITRY.EQ.1) THEN
                IPOS=IFST-1
                ITRY=2
                IBST=0
                DBST=0.
              ELSE
                IPOS=IBST
                X1=VRTX(1,IEDG(1,IFCE(1,IPOS)))
                Y1=VRTX(2,IEDG(1,IFCE(1,IPOS)))
                Z1=VRTX(3,IEDG(1,IFCE(1,IPOS)))
                W1=VRTX(4,IEDG(1,IFCE(1,IPOS)))
                X2=VRTX(1,IEDG(2,IFCE(1,IPOS)))
                Y2=VRTX(2,IEDG(2,IFCE(1,IPOS)))
                Z2=VRTX(3,IEDG(2,IFCE(1,IPOS)))
                W2=VRTX(4,IEDG(2,IFCE(1,IPOS)))
                IF (IEDG(1,IFCE(2,IPOS)).NE.IEDG(1,IFCE(1,IPOS)).AND.
     +              IEDG(1,IFCE(2,IPOS)).NE.IEDG(2,IFCE(1,IPOS))) THEN
                  X3=VRTX(1,IEDG(1,IFCE(2,IPOS)))
                  Y3=VRTX(2,IEDG(1,IFCE(2,IPOS)))
                  Z3=VRTX(3,IEDG(1,IFCE(2,IPOS)))
                  W3=VRTX(4,IEDG(1,IFCE(2,IPOS)))
                ELSE
                  X3=VRTX(1,IEDG(2,IFCE(2,IPOS)))
                  Y3=VRTX(2,IEDG(2,IFCE(2,IPOS)))
                  Z3=VRTX(3,IEDG(2,IFCE(2,IPOS)))
                  W3=VRTX(4,IEDG(2,IFCE(2,IPOS)))
                END IF
                GO TO 103
              END IF
            END IF
            IPOS=IPOS+1
          GO TO 102
  103     CONTINUE
          IFST=IFCE(4,IPOS)
          NUMB=4
          ITRY=1
        IF (IFST.NE.0) GO TO 101
C
C Compute a data value at the point.
C
        IF ((X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2.LT..000001) THEN
          RVAL=W1
        ELSE
          AP=Y1*(Z3-Z2)+Y2*(Z1-Z3)+Y3*(Z2-Z1)
          BP=X1*(Z2-Z3)+X2*(Z3-Z1)+X3*(Z1-Z2)
          CP=X1*(Y3-Y2)+X2*(Y1-Y3)+X3*(Y2-Y1)
          DP=X1*Y2*Z3-X1*Y3*Z2-X2*Y1*Z3+X3*Y1*Z2+X2*Y3*Z1-X3*Y2*Z1
          TP=-DP/(AP*X0+BP*Y0+CP*Z0)
          X4=TP*X0
          Y4=TP*Y0
          Z4=TP*Z0
          A1=Y3*Z2-Y2*Z3
          B1=X2*Z3-X3*Z2
          C1=X3*Y2-X2*Y3
          TP=-(A1*X1+B1*Y1+C1*Z1)/(A1*(X4-X1)+B1*(Y4-Y1)+C1*(Z4-Z1))
          X5=X1+TP*(X4-X1)
          Y5=Y1+TP*(Y4-Y1)
          Z5=Z1+TP*(Z4-Z1)
          W5=W2+(W3-W2)*SQRT(((X5-X2)**2+(Y5-Y2)**2+(Z5-Z2)**2)/
     +                       ((X3-X2)**2+(Y3-Y2)**2+(Z3-Z2)**2))
          RVAL=W1+(W5-W1)*SQRT(((X4-X1)**2+(Y4-Y1)**2+(Z4-Z1)**2)/
     +                         ((X5-X1)**2+(Y5-Y1)**2+(Z5-Z1)**2))
        END IF
C
C Adjust the value to the desired range.
C
        GGDPNT=RLOW+(RHGH-RLOW)*(RVAL-RMIN)/(RMAX-RMIN)
C
C Done.
C
        RETURN
C
      END


      FUNCTION FRAN ()
C
C Pseudo-random-number generator.
C
        DOUBLE PRECISION X
        SAVE X
C
        DATA X / 2.718281828459045D0 /
C
        X=MOD(9821.D0*X+.211327D0,1.D0)
        FRAN=REAL(X)
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
