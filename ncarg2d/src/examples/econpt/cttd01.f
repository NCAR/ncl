

      PROGRAM CTTD01
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
C the use of a simple, closed, grid wrapped around an arbitrary blob.
C The triangular mesh is created from a rectangular grid by calling
C the CONPACKT routine CTTMRG, which returns a triangular mesh on the
C surface of the unit sphere, and then putting the triangular mesh
C through a couple of steps that deform it.  Dummy data are supplied
C for its vertices, and the data are contoured using a mapping supplied
C by the 3D package TDPACK.
C
C Various frames can be drawn or not drawn:
C
C   1) The triangular mesh on the globe.  This is drawn only if the
C      parameter IMSH is non-zero.
C
C   2) Simple contours on the globe.  This is drawn only if the
C      parameter ICON is non-zero.
C
C   3) Color-filled contour bands on the globe, done using an area
C      map.  This is drawn only if the parameter ICOL is non-zero.
C
C   4) Color-filled contour bands on the globe, done using a cell
C      array.  This is drawn only if the parameter ICAP is non-zero.
C
C Define parameters that say whether or not the triangular mesh is to be
C drawn on the various frames
C
C       PARAMETER (IMSH=0)  !  frame 1 (mesh only) not drawn
C       PARAMETER (IMSH=1)  !  frame 1 (mesh only) drawn
C
        PARAMETER (IMSH=1)
C
C Define the parameter that says whether or not parts of the triangular
C mesh that were diagonals of the rectangular mesh are to be drawn.
C
C       PARAMETER (IDIA=0)  !  diagonals not drawn
C       PARAMETER (IDIA=1)  !  diagonals drawn
C
        PARAMETER (IDIA=0)
C
C Define the parameter that says whether or not simple contours are to
C be drawn:
C
C       PARAMETER (ICON=0)  !  contours not drawn
C       PARAMETER (ICON=1)  !  contours drawn
C
        PARAMETER (ICON=1)
C
C       PARAMETER (IMSN=0)  !  mesh not drawn on frame
C       PARAMETER (IMSN=1)  !  mesh drawn on frame
C
        PARAMETER (IMSN=1)
C
C Define the parameter that says whether or not color-filled contours
C (drawn using an area map) are to be drawn:
C
C       PARAMETER (ICOL=0)  !  no color fill done
C       PARAMETER (ICOL=1)  !  color fill done
C
        PARAMETER (ICOL=1)
C
C       PARAMETER (IMSL=0)  !  mesh not drawn on frame
C       PARAMETER (IMSL=1)  !  mesh drawn on frame
C
        PARAMETER (IMSL=0)
C
C       PARAMETER (ICSL=0)  !  contours not drawn on frame
C       PARAMETER (ICSL=1)  !  contours drawn on frame
C
        PARAMETER (ICSL=1)
C
C Define the parameter that says whether or not color-filled contours
C (drawn using a cell array) are to be drawn:
C
C       PARAMETER (ICAP=0)  !  no color fill done
C       PARAMETER (ICAP=1)  !  color fill done
C
        PARAMETER (ICAP=1)
C
C       PARAMETER (IMSP=0)  !  mesh not drawn on frame
C       PARAMETER (IMSP=1)  !  mesh drawn on frame
C
        PARAMETER (IMSP=0)
C
C       PARAMETER (ICOP=0)  !  contours not drawn on frame
C       PARAMETER (ICOP=1)  !  contours drawn on frame
C
        PARAMETER (ICOP=1)
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
        PARAMETER (LOEN=6)
C
C The six elements of an edge node are
C
C   1. the base index, in RPNT, of point 1 of the edge;
C   2. the base index, in RPNT, of point 2 of the edge;
C   3. the index, in ITRI, of the pointer to the edge in the triangle to
C      the left of the edge (-1 if there is no triangle to the left);
C   4. the index, in ITRI, of the pointer to the edge in the triangle to
C      the right of the edge (-1 if there is no triangle to the right);
C   5. a utility flag for use by algorithms that scan the structure.
C   6. a flag used to distinguish certain edges from others (allowing
C      one to suppress the drawing of edges that were diagonals of the
C      rectangular grid).
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
C      removing it from the mesh.  Different bits of this flag are
C      used for different purposes, as discussed below just prior to
C      the call to the routine CTTDBF.
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
C The maximum numbers of points, edges, and triangles in a triangular
C mesh can be computed in the following manner:  Since the triangular
C mesh is created from a rectangular one by splitting each rectangular
C cell in half, we can declare the largest values we expect to use for
C the dimensions of that mesh:
C
C       PARAMETER (IDIM=361,JDIM=181,IDM1=IDIM-1,JDM1=JDIM-1)
C       PARAMETER (IDIM=181,JDIM= 91,IDM1=IDIM-1,JDM1=JDIM-1)
C       PARAMETER (IDIM=121,JDIM= 61,IDM1=IDIM-1,JDM1=JDIM-1)
        PARAMETER (IDIM= 81,JDIM= 41,IDM1=IDIM-1,JDM1=JDIM-1)
C       PARAMETER (IDIM= 41,JDIM= 21,IDM1=IDIM-1,JDM1=JDIM-1)
C
C and then compute from those values the maximum number of points,
C edges, and triangles that the triangular mesh arrays will need to
C hold.  The computed values will be exactly as required if no points
C or edges of the rectangular grid are repeated in it; if there are
C repeating points or edges, space for slightly fewer points and edges
C will be needed:
C
        PARAMETER (MNOP=IDIM*JDIM)
        PARAMETER (MNOE=3*IDM1*JDM1+IDM1+JDM1)
        PARAMETER (MNOT=2*IDM1*JDM1)
C
C A simple lat/lon grid is a rectangular array, at each point of which
C we have latitude, longitude, and a data value.  From this rectangular
C grid, we construct a triangular mesh to test the contouring algorithm.
C XLAT and XLON are the arrays of latitudes and longitudes and ZDAT is
C the array of field values.
C
        DIMENSION XLAT(IDIM,JDIM),XLON(IDIM,JDIM),ZDAT(IDIM,JDIM)
C
C Each cell of the rectangular grid for which data is available at all
C four corners is split in half along one of its diagonals; resulting
C triangles form the triangular mesh.  As we create the triangular mesh,
C we have to keep track of where the points and edges of the rectangular
C grid were put (so as to avoid the problem of duplicating points and
C edges in the structure).  The array ISCR is used for this.
C
        DIMENSION ISCR(4,IDIM,JDIM)
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
C Declare real and integer workspaces needed by CONPACKT.
C
        PARAMETER (LRWK=10000,LIWK=5000)
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare the area map array needed to do solid fill.
C
        PARAMETER (LAMA=2000000)
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
        PARAMETER (ICAM= 512,ICAN= 512)
C       PARAMETER (ICAM=1024,ICAN=1024)
C       PARAMETER (ICAM=2048,ICAN=2048)
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
C DEBUG: CHARACTER TEMPORARY FOR LABELLING PURPOSES.
C
C       CHARACTER*5 CTMP
C
C Set the desired minimum and maximum values of X, Y, and Z.
C
        DATA XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX / -1.,-1.,-1.,1.,1.,1. /
C
C Set the desired values of parameters determining the eye position.
C ANG1 is a bearing angle, ANG2 is an elevation angle, and RMUL is a
C multiplier of the length of the diagonal of the data box, specifying
C the distance from the center of the box to the eye.
C
        DATA ANG1,ANG2,RMUL / -35.,55.,2.9 /
C
C ISTE is a flag that says whether to do a simple image (ISTE=0),
C a one-frame stereo image (ISTE=-1), or a two-frame stereo image
C (ISTE=+1).
C
C       DATA ISTE /  0 /  !  simple image
C       DATA ISTE / -1 /  !  one-frame stereo image
C       DATA ISTE / +1 /  !  two-frame stereo image
C
        DATA ISTE / -1 /
C
C ASTE is the desired angle (in degrees) between the lines of sight for
C a pair of stereo views.
C
        DATA ASTE / 2. /
C
C WOSW is the width of the stereo windows to be used in one-frame stereo
C images; the width is stated as a fraction of the width of the plotter
C frame.  (The windows are centered vertically; horizontally, they are
C placed as far apart as possible in the plotter frame.)  The value used
C must be positive and non-zero; it may be slightly greater than .5, if
C it is desired that the stereo windows should overlap slightly.
C
        DATA WOSW / .5 /
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
C Define conversion constants to convert from degrees to radians and
C vice-versa.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Define a constant to be used in the code that blocks triangles too
C nearly parallel to the line of sight.
C
        DATA ATOL / 4. /
C
C Define the three radii of the initial ellipsoid; it cuts the X axis
C at + or - ARAD, the Y axis at + or - BRAD, and the Z axis at + or -
C CRAD.
C
        ARAD=1.0
        BRAD=1.4
        CRAD=1.8
C
C Create data and generate the required triangular mesh.
C
        PRINT * , ' '
        PRINT * , 'CREATING TRIANGULAR MESH:'
C
        CALL GTTD01 (RPNT,MPNT,NPNT,LOPN,
     +               IEDG,MEDG,NEDG,LOEN,
     +               ITRI,MTRI,NTRI,LOTN,
     +               IDIM,JDIM,XLAT,XLON,ZDAT,ISCR)
C
C Print the number of points, edges, and triangles.
C
        PRINT * , '  NUMBER OF POINTS:    ',NPNT/LOPN
        PRINT * , '  NUMBER OF EDGES:     ',NEDG/LOEN
        PRINT * , '  NUMBER OF TRIANGLES: ',NTRI/LOTN
C
C Set the flags in the edge nodes that will allow us to distinguish
C "diagonal" edges from others.
C
        DO 101 IPTE=0,NEDG-LOEN,LOEN
C
          XSP1=RPNT(IEDG(IPTE+1)+1)
          YSP1=RPNT(IEDG(IPTE+1)+2)
          ZSP1=RPNT(IEDG(IPTE+1)+3)
C
          RLT1=ASIN(ZSP1)
C
          IF (XSP1.NE.0..AND.YSP1.NE.0.) THEN
            RLN1=ATAN2(YSP1,XSP1)
          ELSE
            RLN1=0.
          END IF
C
          XSP2=RPNT(IEDG(IPTE+2)+1)
          YSP2=RPNT(IEDG(IPTE+2)+2)
          ZSP2=RPNT(IEDG(IPTE+2)+3)
C
          RLT2=ASIN(ZSP2)
C
          IF (XSP2.NE.0..AND.YSP2.NE.0.) THEN
            RLN2=ATAN2(YSP2,XSP2)
          ELSE
            RLN2=0.
          END IF
C
          IEDG(IPTE+6)=0
C
          IF (ABS(ZSP1).LT..9999.AND.ABS(ZSP2).LT..9999.AND.
     +        ABS(RLT1-RLT2).GT..0001.AND.
     +        ABS(RLN1-RLN2).GT..0001) IEDG(IPTE+6)=1
C
  101   CONTINUE
C
C Deform the triangular mesh to fit on the surface of the ellipsoid,
C further deform it to create a sort of distorted "dumbbell", and
C then supply dummy data values.
C
        CALL TDGDIN (-1.,1.,-1.,1.,-1.,1.,21,21)
C
        DO 102 IPTP=0,NPNT-LOPN,LOPN
          TRAD=1./SQRT(RPNT(IPTP+1)*RPNT(IPTP+1)/(ARAD*ARAD)+
     +                 RPNT(IPTP+2)*RPNT(IPTP+2)/(BRAD*BRAD)+
     +                 RPNT(IPTP+3)*RPNT(IPTP+3)/(CRAD*CRAD))
          RPNT(IPTP+1)=TRAD*RPNT(IPTP+1)
          RPNT(IPTP+2)=TRAD*RPNT(IPTP+2)
          RPNT(IPTP+3)=TRAD*RPNT(IPTP+3)
          RPNT(IPTP+1)=RPNT(IPTP+1)*(.3+.5*RPNT(IPTP+3)**2)
          RPNT(IPTP+2)=RPNT(IPTP+2)*(.3+.5*RPNT(IPTP+3)**2)
          RPNT(IPTP+3)=RPNT(IPTP+3)*(.7+.2*(RPNT(IPTP+1)**3+
     +                                      RPNT(IPTP+2)**3))
          RPNT(IPTP+4)=TDGDVA(RPNT(IPTP+1),RPNT(IPTP+2),RPNT(IPTP+3))
  102   CONTINUE
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
C Turn on the clipping indicator.
C
        CALL GSCLIP (1)
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
C       CALL DFCRGB (IWID,151,250,0.,0.,1.,0.,1.,0.,0.) ! blue to green
C       CALL DFCRGB (IWID,151,250,0.,1.,0.,1.,0.,0.,0.) ! green to red
C       CALL DFCRGB (IWID,151,250,0.,0.,1.,1.,0.,0.,.4) ! blue to red
C       CALL DFCRGB (IWID,151,250,0.,1.,1.,1.,0.,1.,0.) ! cyan to magenta
C       CALL DFCRGB (IWID,151,250,0.,1.,1.,1.,1.,0.,0.) ! cyan to yellow
C
        CALL DFCRGB (IWID,151,250,0.,1.,1.,1.,0.,1.,0.)
C
C       CALL DFCHLS (IWID,151,250,0.,50.,100.,120.,50.,100.)
C
C Set parameters in the utilities.
C
        PRINT * , 'SETTING PARAMETERS IN CONPACKT AND PLOTCHAR'
C
C Set the mapping flag to select the TDPACK mapping (implies use of a
C special version of CTMXYZ).
C
        CALL CTSETI ('MAP - MAPPING FLAG',2)
C
C Set the area identifier for cells associated with triangles that are
C seen from "the back".
C
C       CALL CTSETI ('PAI - PARAMETER ARRAY INDEX',-1)
C       CALL CTSETI ('AIA - AREA IDENTIFIER FOR AREA',1001)
C
C Set the area identifier for cells associated with no triangles of the
C mesh.
C
C       CALL CTSETI ('PAI - PARAMETER ARRAY INDEX',-2)
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
C Set the point-interpolation threshold value.
C
        CALL CTSETR ('PIT - POINT INTERPOLATION THRESHOLD VALUE',.005)
C
C Tell CONPACKT not to do its own call to SET, since TDPACK will have
C done it.
C
        CALL CTSETI ('SET - DO-SET-CALL FLAG', 0)
C
C Move the informational label up a little.
C
        CALL CTSETR ('ILY - INFORMATIONAL LABEL Y POSITION',-.005)
C
C Tell CONPACKT to use a little more workspace in CTTDBF.
C
        CALL CTSETI ('IWB - INTEGER WORKSPACE FOR BLOCKING',5000)
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
C Find the midpoint of the data box (to be used as the point looked at).
C
        XMID=.5*(XMIN+XMAX)
        YMID=.5*(YMIN+YMAX)
        ZMID=.5*(ZMIN+ZMAX)
C
C Determine the distance (R) from which the data box will be viewed and,
C given that, the eye position.
C
        R=RMUL*SQRT((XMAX-XMIN)**2+(YMAX-YMIN)**2+(ZMAX-ZMIN)**2)
C
        XEYE=XMID+R*COS(DTOR*ANG1)*COS(DTOR*ANG2)
        YEYE=YMID+R*SIN(DTOR*ANG1)*COS(DTOR*ANG2)
        ZEYE=ZMID+R*SIN(DTOR*ANG2)
C
C SET THE TRIANGLE BLOCKING FLAGS FOR THE VIEWS TO BE DRAWN.
C --- --- -------- -------- ----- --- --- ----- -- -- ------
C
        PRINT * , 'SETTING TRIANGLE BLOCKING FLAGS'
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
        IF (ISTE.EQ.0) THEN
          OTEP=0.
        ELSE
          OTEP=-R*TAN(DTOR*ASTE/2.)
        END IF
C
C Initialize TDPACK.
C
  103   PRINT * , '  INITIALIZING TDPACK'
C
        CALL TDINIT (XEYE,YEYE,ZEYE,XMID,YMID,ZMID,
     +                              XMID,YMID,ZMID+R,OTEP)
C
C If stereo views are being done, do the requested thing, either by
C redoing the SET call to put them side by side on the same frame,
C or by calling FRAME to put them on separate frames.
C
          IF (OTEP.NE.0.) THEN
            IF (ISTE.LT.0) THEN
              CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
              IF (OTEP.LT.0.) THEN
                CALL SET  (1.-WOSW,1.,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              ELSE
                CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              END IF
            END IF
          END IF
C
C Initialize CONPACKT.
C
          PRINT * , '  INITIALIZING CONPACKT'
C
          PRINT * , '  CALLING CTMESH'
          CALL CTMESH (RPNT,NPNT,LOPN,
     +                 IEDG,NEDG,LOEN,
     +                 ITRI,NTRI,LOTN,
     +                 RWRK,LRWK,
     +                 IWRK,LIWK)
C
C CTTDBF is called to set blocking flag bits for triangles according to
C various criteria.  Its next-to-last argument may take on the following
C values:
C
C   0 => just clear the blocking flags for all triangles.
C   1 => set blocking flags for triangles that are nearly edge-on to
C        the line of sight or that are seen from the "wrong" side (so
C        that they appear to be defined by points given in clockwise
C        order, rather than in counter-clockwise order).
C   2 => set blocking flags for triangles that are hidden by other
C        triangles of the mesh.
C   3 => do both 1 and 2.
C
C Its final argument is a tolerance value, in degrees, defining what
C it means for a triangle to be nearly edge-on to the line of sight.
C Using the value zero turns off the setting of nearly-edge-on bits.
C
C The bits used in the blocking flags are as follows.  Assume that the
C seven rightmost (lowest-order) bits of the blocking flags are numbered
C as follows:
C
C   . . . B6 B5 B4 B3 B2 B1 B0
C
C Bit B0 of the blocking flag for a triangle is untouched by a call to
C CTTDBF; a user may set it to block the triangle.
C
C Bits B3, B2, and B1 are used for the view from the right eye (when
C stereo views are being drawn) or the only eye (when a simple view is
C being drawn).  If B3 is set, it indicates that the triangle is hidden
C by another triangle of the mesh; if B2 is set, it indicates that the
C triangle is nearly edge-on to the line of sight; if B1 is set, it
C indicates that the triangle is seen from the wrong side.
C
C Bits B6, B5, and B4 are defined similarly, but apply to the view from
C the left eye, rather than the right eye (when stereo views are being
C done).
C
          PRINT * , '  CALLING CTTDBF'
C
          CALL CTTDBF (RPNT,IEDG,ITRI,RWRK,IWRK,3,2.)
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
          IF (OTEP.LT.0.) THEN
            OTEP=-OTEP
            GO TO 103
          END IF
C
C Once bits B6, B5, B4, B3, B2, B1, and B0 are set in all the triangle
C blocking flags, one may use them to determine what is drawn in a
C particular view.  This is done by calling the routine CTTDBM, which
C has eight integer arguments.  The first four arguments are used to
C toggle bit values from the triangle's blocking flag, while the final
C four arguments are used to determine which of those bit values are
C considered to be meaningful.  For example, using the values "0, 0,
C 0, 0, 1, 1, 0, 1" says that none of the four values is to be toggled
C and that all will be allowed to block the triangle except the third;
C the net effect is to block triangles that are hidden, nearly edge-on,
C or blocked by the user, but none that are viewed from the wrong side.
C The four bits used from the blocking flag of each triangle are either
C bits B3, B2, B1, and B0, if the right (or only) eye is in use or bits
C B6, B5, B4, and B0, if the left eye is in use.
C
C Set masks for 3-D| t t t t u u u u | toggle/use
C triangle blocking| h e w u h e w u | hidden/edge-on/wrongside/user
C
C       CALL CTTDBM (0,0,0,0,1,1,0,1)
C
C In this example, there is a single call to CTTDBM, as just described:
C
        CALL CTTDBM (0,0,0,0,1,1,0,1)
C
C DRAW THE TRIANGULAR MESH, IF REQUESTED.
C ---- --- ---------- ----- -- ----------
C
        IF (IMSH.NE.0) THEN
C
          PRINT * , 'DRAWING TRIANGULAR MESH'
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
          IF (ISTE.EQ.0) THEN
            OTEP=0.
          ELSE
            OTEP=-R*TAN(DTOR*ASTE/2.)
          END IF
C
C Initialize TDPACK.
C
  104     PRINT * , '  INITIALIZING TDPACK'
C
          CALL TDINIT (XEYE,YEYE,ZEYE,XMID,YMID,ZMID,
     +                                XMID,YMID,ZMID+R,OTEP)
C
C If stereo views are being done, do the requested thing, either by
C redoing the SET call to put them side by side on the same frame,
C or by calling FRAME to put them on separate frames.
C
          IF (OTEP.NE.0.) THEN
            IF (ISTE.LT.0) THEN
              CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
              IF (OTEP.LT.0.) THEN
                CALL SET  (1.-WOSW,1.,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              ELSE
                CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              END IF
            ELSE
              IF (OTEP.GT.0.) CALL FRAME
            END IF
          END IF
C
C Draw the mesh, showing only the edges of unblocked triangles.
C
          CALL PLOTIT (0,0,2)
          CALL GSPLCI (8)
C
          CALL CTTDDM (RPNT,IEDG,ITRI,RWRK,IWRK,(1-IDIA)*6)
C
          CALL PLOTIT (0,0,2)
          CALL GSPLCI (1)
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
          IF (OTEP.LT.0.) THEN
            OTEP=-OTEP
            GO TO 104
          END IF
C
C Advance the frame.
C
          CALL FRAME
C
        END IF
C
C DRAW SIMPLE CONTOURS, IF REQUESTED.
C ---- ------ --------- -- ----------
C
        IF (ICON.NE.0) THEN
C
          PRINT * , 'DRAWING SIMPLE CONTOUR PLOT'
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
          IF (ISTE.EQ.0) THEN
            OTEP=0.
          ELSE
            OTEP=-R*TAN(DTOR*ASTE/2.)
          END IF
C
C Initialize TDPACK.
C
  105     PRINT * , '  INITIALIZING TDPACK'
C
          CALL TDINIT (XEYE,YEYE,ZEYE,XMID,YMID,ZMID,
     +                                XMID,YMID,ZMID+R,OTEP)
C
C If stereo views are being done, do the requested thing, either by
C redoing the SET call to put them side by side on the same frame,
C or by calling FRAME to put them on separate frames.
C
          IF (OTEP.NE.0.) THEN
            IF (ISTE.LT.0) THEN
              CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
              IF (OTEP.LT.0.) THEN
                CALL SET  (1.-WOSW,1.,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              ELSE
                CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              END IF
            ELSE
              IF (OTEP.GT.0.) CALL FRAME
            END IF
          END IF
C
C Initialize CONPACKT.
C
          PRINT * , '  INITIALIZING CONPACKT'
C
          PRINT * , '  CALLING CTMESH'
          CALL CTMESH (RPNT,NPNT,LOPN,
     +                 IEDG,NEDG,LOEN,
     +                 ITRI,NTRI,LOTN,
     +                 RWRK,LRWK,
     +                 IWRK,LIWK)
C
          IF (IMSN.NE.0) THEN
C
            PRINT * , '  DRAWING TRIANGULAR MESH'
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (8)
C
            CALL CTTDDM (RPNT,IEDG,ITRI,RWRK,IWRK,(1-IDIA)*6)
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (1)
C
C Debug code: number the triangles.
C
C           CALL PLOTIT (0,0,2)
C           CALL GSPLCI (4)
C
C           DO 999 IPTT=0,NTRI-LOTN,LOTN
C             WRITE (CTMP,'(I5)') IPTT/LOTN+1
C             XPOS=(RPNT(IEDG(ITRI(IPTT+1)+1)+1)+
C    +              RPNT(IEDG(ITRI(IPTT+1)+2)+1)+
C    +              RPNT(IEDG(ITRI(IPTT+2)+1)+1)+
C    +              RPNT(IEDG(ITRI(IPTT+2)+2)+1)+
C    +              RPNT(IEDG(ITRI(IPTT+3)+1)+1)+
C    +              RPNT(IEDG(ITRI(IPTT+3)+2)+1))/6.
C             YPOS=(RPNT(IEDG(ITRI(IPTT+1)+1)+2)+
C    +              RPNT(IEDG(ITRI(IPTT+1)+2)+2)+
C    +              RPNT(IEDG(ITRI(IPTT+2)+1)+2)+
C    +              RPNT(IEDG(ITRI(IPTT+2)+2)+2)+
C    +              RPNT(IEDG(ITRI(IPTT+3)+1)+2)+
C    +              RPNT(IEDG(ITRI(IPTT+3)+2)+2))/6.
C             ZPOS=(RPNT(IEDG(ITRI(IPTT+1)+1)+3)+
C    +              RPNT(IEDG(ITRI(IPTT+1)+2)+3)+
C    +              RPNT(IEDG(ITRI(IPTT+2)+1)+3)+
C    +              RPNT(IEDG(ITRI(IPTT+2)+2)+3)+
C    +              RPNT(IEDG(ITRI(IPTT+3)+1)+3)+
C    +              RPNT(IEDG(ITRI(IPTT+3)+2)+3))/6.
C             CALL TDPRPT (XPOS,YPOS,ZPOS,XLBL,YLBL)
C             DO 998 I=1,5
C               J=6-I
C               IF (CTMP(I:I).NE.' ') ILNB=I
C               IF (CTMP(J:J).NE.' ') IFNB=J
C 998         CONTINUE
C             CALL PLCHMQ (XLBL,YLBL,CTMP(IFNB:ILNB),.0004,0.,0.)
C 999       CONTINUE
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (1)
C
          END IF
C
C Proceed as implied by the setting of the label-positioning flag.
C
          IF (ABS(ILLP).EQ.1) THEN
C
C Draw the contour lines with labels generated by the dash package.
C
            PRINT * , '  CALLING CTCLDR'
            CALL CTCLDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
C Add the informational and high/low labels.
C
            PRINT * , '  CALLING CTLBDR'
            CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
          ELSE IF (ABS(ILLP).GT.1) THEN
C
C Create an area map for masking of labels.
C
            MAXN=0
C
            PRINT * , '  CALLING ARINAM'
            CALL ARINAM (IAMA,LAMA)
C
            PRINT * , '  CALLING CTLBAM'
            CALL CTLBAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
C Draw the contour lines masked by the area map.
C
            PRINT * , '  CALLING CTCLDM'
            CALL CTCLDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,DRWMCL)
C
            PRINT * , '  AREA MAP SPACE REQUIRED:         ',
     +                                         IAMA(1)-IAMA(6)+IAMA(5)+1
C
            PRINT * , '  NUMBER OF POINTS IN LONGEST LINE:',MAXN
C
C Draw all the labels.
C
            PRINT * , '  CALLING CTLBDR'
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
C If a left-eye view has just been done, loop back for a right-eye view.
C
          IF (OTEP.LT.0.) THEN
            OTEP=-OTEP
            GO TO 105
          END IF
C
C Advance the frame.
C
          CALL FRAME
C
        END IF
C
C DRAW COLOR-FILLED CONTOUR BANDS (USING AN AREA MAP), IF REQUESTED.
C ---- ------------ ------- ----- ------ -- ---- ----- -- ----------
C
        IF (ICOL.NE.0) THEN
C
          PRINT * , 'DRAWING COLOR-FILLED CONTOUR PLOT (USING AREA MAP)'
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
          IF (ISTE.EQ.0) THEN
            OTEP=0.
          ELSE
            OTEP=-R*TAN(DTOR*ASTE/2.)
          END IF
C
C Initialize TDPACK.
C
  106     PRINT * , '  INITIALIZING TDPACK'
C
          CALL TDINIT (XEYE,YEYE,ZEYE,XMID,YMID,ZMID,
     +                                XMID,YMID,ZMID+R,OTEP)
C
C If stereo views are being done, do the requested thing, either by
C redoing the SET call to put them side by side on the same frame,
C or by calling FRAME to put them on separate frames.
C
          IF (OTEP.NE.0.) THEN
            IF (ISTE.LT.0) THEN
              CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
              IF (OTEP.LT.0.) THEN
                CALL SET  (1.-WOSW,1.,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              ELSE
                CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              END IF
            ELSE
              IF (OTEP.GT.0.) CALL FRAME
            END IF
          END IF
C
C Initialize CONPACKT.
C
          PRINT * , '  INITIALIZING CONPACKT'
C
          PRINT * , '  CALLING CTMESH'
          CALL CTMESH (RPNT,NPNT,LOPN,
     +                 IEDG,NEDG,LOEN,
     +                 ITRI,NTRI,LOTN,
     +                 RWRK,LRWK,
     +                 IWRK,LIWK)
C
          MAXN=0
C
          PRINT * , '  CALLING CTPKCL'
          CALL CTPKCL (RPNT,IEDG,ITRI,RWRK,IWRK)
C
          PRINT * , '  CALLING ARINAM'
          CALL ARINAM (IAMA,LAMA)
C
          PRINT * , '  CALLING CTCLAM'
          CALL CTCLAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
C         PRINT * , '  CALLING CTLBAM'
C         CALL CTLBAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
          PRINT * , '  CALLING ARSCAM'
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
          IF (IMSL.NE.0) THEN
C
            PRINT * , '  DRAWING TRIANGULAR MESH'
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (8)
C
            CALL CTTDDM (RPNT,IEDG,ITRI,RWRK,IWRK,(1-IDIA)*6)
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (1)
C
          END IF
C
          IF (ICSL.NE.0) THEN
C
            IF (ABS(ILLP).EQ.1) THEN
C
              PRINT * , '  CALLING CTCLDR'
              CALL CTCLDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
              PRINT * , '  CALLING CTLBDR'
              CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
            ELSE IF (ABS(ILLP).GT.1) THEN
C
              MAXN=0
C
              PRINT * , '  CALLING ARINAM'
              CALL ARINAM (IAMA,LAMA)
C
              PRINT * , '  CALLING CTLBAM'
              CALL CTLBAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
              PRINT * , '  CALLING CTCLDM'
              CALL CTCLDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,DRWMCL)
C
              PRINT * , '  AREA MAP SPACE REQUIRED:         ',
     +                                         IAMA(1)-IAMA(6)+IAMA(5)+1
C
              PRINT * , '  NUMBER OF POINTS IN LONGEST LINE:',MAXN
C
              PRINT * , '  CALLING CTLBDR'
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
          END IF
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
          IF (OTEP.LT.0.) THEN
            OTEP=-OTEP
            GO TO 106
          END IF
C
C Advance the frame.
C
          CALL FRAME
C
        END IF
C
C DRAW COLOR-FILLED CONTOUR BANDS (USING A CELL ARRAY), IF REQUESTED.
C ---- ------------ ------- ----- ------ - ---- ------- -- ----------
C
        IF (ICAP.NE.0) THEN
C
          PRINT * , 'DRAWING COLOR-FILLED CONTOUR PLOT (CELL ARRAY)'
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
          IF (ISTE.EQ.0) THEN
            OTEP=0.
          ELSE
            OTEP=-R*TAN(DTOR*ASTE/2.)
          END IF
C
C Initialize TDPACK.
C
  107     PRINT * , '  INITIALIZING TDPACK'
C
          CALL TDINIT (XEYE,YEYE,ZEYE,XMID,YMID,ZMID,
     +                                XMID,YMID,ZMID+R,OTEP)
C
C If stereo views are being done, do the requested thing, either by
C redoing the SET call to put them side by side on the same frame,
C or by calling FRAME to put them on separate frames.
C
          IF (OTEP.NE.0.) THEN
            IF (ISTE.LT.0) THEN
              CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
              IF (OTEP.LT.0.) THEN
                CALL SET  (1.-WOSW,1.,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              ELSE
                CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                             XWDL,XWDR,YWDB,YWDT,LNLG)
              END IF
            ELSE
              IF (OTEP.GT.0.) CALL FRAME
            END IF
          END IF
C
C Initialize CONPACKT.
C
          PRINT * , '  INITIALIZING CONPACKT'
C
          PRINT * , '  CALLING CTMESH'
          CALL CTMESH (RPNT,NPNT,LOPN,
     +                 IEDG,NEDG,LOEN,
     +                 ITRI,NTRI,LOTN,
     +                 RWRK,LRWK,
     +                 IWRK,LIWK)
C
C Retrieve the arguments with which SET was called.
C
          CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
          PRINT * , '  CALLING CTTDCA'
          CALL CTTDCA (RPNT,IEDG,ITRI,RWRK,IWRK,ICRA,ICAM,ICAM,ICAN,
     +                                          XVPL,YVPB,XVPR,YVPT)
C
          PRINT * , '  CALLING GCA'
          CALL GCA (XWDL,YWDB,XWDR,YWDT,ICAM,ICAN,1,1,ICAM,ICAN,ICRA)
C
          CALL CTGETI ('IWU - INTEGER WORKSPACE USED',IIWU)
          PRINT * , '  INTEGER WORKSPACE REQUIRED:      ',IIWU
C
          CALL CTGETI ('RWU -    REAL WORKSPACE USED',IRWU)
          PRINT * , '  REAL WORKSPACE REQUIRED:         ',IRWU
C
          IF (IMSP.NE.0) THEN
C
            PRINT * , '  DRAWING TRIANGULAR MESH'
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (8)
C
            CALL CTTDDM (RPNT,IEDG,ITRI,RWRK,IWRK,(1-IDIA)*6)
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (1)
C
          END IF
C
          IF (ICOP.NE.0) THEN
C
            IF (ABS(ILLP).EQ.1) THEN
C
              PRINT * , '  CALLING CTCLDR'
              CALL CTCLDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
              PRINT * , '  CALLING CTLBDR'
              CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
            ELSE IF (ABS(ILLP).GT.1) THEN
C
              MAXN=0
C
              PRINT * , '  CALLING ARINAM'
              CALL ARINAM (IAMA,LAMA)
C
              PRINT * , '  CALLING CTLBAM'
              CALL CTLBAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA)
C
              PRINT * , '  CALLING CTCLDM'
              CALL CTCLDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,DRWMCL)
C
              PRINT * , '  AREA MAP SPACE REQUIRED:         ',
     +                                         IAMA(1)-IAMA(6)+IAMA(5)+1
C
              PRINT * , '  NUMBER OF POINTS IN LONGEST LINE:',MAXN
C
              PRINT * , '  CALLING CTLBDR'
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
          END IF
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
          IF (OTEP.LT.0.) THEN
            OTEP=-OTEP
            GO TO 107
          END IF
C
C Advance the frame.
C
          CALL FRAME
C
        END IF
C
C DONE.
C -----
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


      SUBROUTINE DFCRGB (IWID,IOFC,IOLC,BRED,BGRN,BBLU,ERED,EGRN,EBLU,
     +                                                           AMUL)
C
C This routine defines color indices IOFC through IOLC on workstation
C IWID by interpolating values from BRED/BGRN/BBLU to ERED/EGRN/EBLU.
C
C BRED, BGRN, BBLE, ERED, EGRN, and EBLU are all reals between 0 and 1.
C
C AMUL should be a real between zero and one: zero gives interpolated
C values along a straight line in the RGB color cube; larger values
C give interpolated values along a curved line that bends toward the
C black corner of the cube.  On some of the newer color laser jet
C printers, using a non-zero value of AMUL gives colors closer to what
C one sees on one's monitor; for example, if the colors range from pure
C blue to pure red, using AMUL=.6 works pretty well.
C
        DO 101 I=IOFC,IOLC
          P=REAL(IOLC-I)/REAL(IOLC-IOFC)
          Q=REAL(I-IOFC)/REAL(IOLC-IOFC)
          RRED=P*BRED+Q*ERED
          RGRN=P*BGRN+Q*EGRN
          RBLU=P*BBLU+Q*EBLU
          DMIN=SQRT(MIN((RRED-0.)**2+(RGRN-0.)**2+(RBLU-0.)**2,
     +                  (RRED-0.)**2+(RGRN-0.)**2+(RBLU-1.)**2,
     +                  (RRED-0.)**2+(RGRN-1.)**2+(RBLU-0.)**2,
     +                  (RRED-0.)**2+(RGRN-1.)**2+(RBLU-1.)**2,
     +                  (RRED-1.)**2+(RGRN-0.)**2+(RBLU-0.)**2,
     +                  (RRED-1.)**2+(RGRN-0.)**2+(RBLU-1.)**2,
     +                  (RRED-1.)**2+(RGRN-1.)**2+(RBLU-0.)**2,
     +                  (RRED-1.)**2+(RGRN-1.)**2+(RBLU-1.)**2))
          DNOM=1.-(2.*DMIN/SQRT(3.))*MAX(0.,MIN(1.,AMUL))
          RRED=RRED*DNOM
          RGRN=RGRN*DNOM
          RBLU=RBLU*DNOM
          CALL GSCR (IWID,I,RRED,RGRN,RBLU)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE DFCHLS (IWID,IOFC,IOLC,BHUE,BLIT,BSAT,EHUE,ELIT,ESAT)
C
C This routine defines color indices IOFC through IOLC on workstation
C IWID by interpolating values from BHUE/BLIT/BSAT to EHUE/ELIT/ESAT.
C
C BHUE and EHUE are expressed as angles between 0 and 720.  Linear
C interpolation will be used to get intermediate values and then the
C result will be reduced modulo 360.
C
C BLIT, BSAT, ELIT, and ESAT are expressed as percentages between 0 and
C 100.
C
        DO 101 I=IOFC,IOLC
          P=REAL(IOLC-I)/REAL(IOLC-IOFC)
          Q=REAL(I-IOFC)/REAL(IOLC-IOFC)
          RHUE=MOD(P*BHUE+Q*EHUE,360.)
          RLIT=MAX(0.,MIN(100.,P*BLIT+Q*ELIT))
          RSAT=MAX(0.,MIN(100.,P*BSAT+Q*ESAT))
          CALL HLSRGB (RHUE,RLIT,RSAT,RRED,RGRN,RBLU)
          CALL GSCR   (IWID,I,RRED,RGRN,RBLU)
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


      SUBROUTINE GTTD01 (RPNT,MPNT,NPNT,LOPN,
     +                   IEDG,MEDG,NEDG,LOEN,
     +                   ITRI,MTRI,NTRI,LOTN,
     +                   IDIM,JDIM,XLAT,XLON,ZDAT,ISCR)
C
        DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C A simple lat/lon grid is a rectangular array, at each point of which
C we have latitude, longitude, and a data value.  From this rectangular
C grid, we construct a triangular mesh to test the contouring algorithm.
C Declare arrays to receive the grid data.  XLAT and XLON are arrays of
C latitudes and longitudes and ZDAT the array of field values.
C
        DIMENSION XLAT(IDIM,JDIM),XLON(IDIM,JDIM),ZDAT(IDIM,JDIM)
C
C Each cell of the rectangular grid for which data is available at all
C four corners is split in half along one of its diagonals; resulting
C triangles form the triangular mesh.  As we create the triangular mesh,
C we have to keep track of where the points and edges of the rectangular
C grid were put (so as to avoid the problem of duplicating points and
C edges in the structure).  The array ISCR is used for this.
C
        DIMENSION ISCR(4,IDIM,JDIM)
C
C Declare external a routine to tell CTTMRG about points of overlap on
C the lat/lon grid.
C
        EXTERNAL MITD01
C
C Define a constant used to convert from degrees to radians.
C
        DATA DTOR / .017453292519943 /
C
C Define latitudes, longitudes, and dummy field values on a rectangular
C mesh to be wrapped around the globe.
C
        DO 102 I=1,IDIM
          DO 101 J=1,JDIM
            XLAT(I,J)= -90.+(REAL(J-1)/REAL(JDIM-1))*180.
            XLON(I,J)=-180.+(REAL(I-1)/REAL(IDIM-1))*360.
            ZDAT(I,J)=1.
  101     CONTINUE
  102   CONTINUE
C
C Call a general-purpose subroutine that accepts a rectangular grid
C mapped onto the surface of the globe and returns a triangular mesh
C equivalent to it.
C
        CALL CTTMRG (IDIM,JDIM,
     +               XLAT,XLON,ZDAT,ISCR,
     +                 0.,MITD01,
     +               RPNT,MPNT,NPNT,LOPN,
     +               IEDG,MEDG,NEDG,LOEN,
     +               ITRI,MTRI,NTRI,LOTN)

C Done.
C
        RETURN
C
      END


      SUBROUTINE MITD01 (IDIM,JDIM,IINI,JINI,IINO,JINO)
C
C Given the dimensions, IDIM and JDIM, of a simple lat/lon grid on the
C globe, and the indices, IINI and JINI, of a point on the grid, this
C routine returns the indices, IINO and JINO, of that coincident point
C on the grid which is to be used to represent it.  This version assumes
C that the first and last rows of the grid each map into a single point
C (perhaps the south pole and the north pole, respectively) and that
C the right and left edges of the grid lie on top of each other.
C
        IF (JINI.EQ.1) THEN
          IINO=1
          JINO=1
        ELSE IF (JINI.EQ.JDIM) THEN
          IINO=1
          JINO=JDIM
        ELSE IF (IINI.EQ.IDIM) THEN
          IINO=1
          JINO=JINI
        ELSE
          IINO=IINI
          JINO=JINI
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE TDGDIN (XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,MLOW,MHGH)
C
C This is a routine to generate test data for three-dimensional graphics
C routines.  The function used is a sum of exponentials.
C
C "MLOW" and "MHGH" are each forced to be greater than or equal to 1 and
C less than or equal to 25.
C
        COMMON /TDGDCO/ XRNG,YRNG,ZRNG,NCNT,CCNT(4,50)
        SAVE   /TDGDCO/
C
        XRNG=XMAX-XMIN
        YRNG=YMAX-YMIN
        ZRNG=ZMAX-ZMIN
C
        NLOW=MAX(1,MIN(25,MLOW))
        NHGH=MAX(1,MIN(25,MHGH))
        NCNT=NLOW+NHGH
C
        DO 101 ICNT=1,NCNT
          CCNT(1,ICNT)=XMIN+XRNG*FRAN()
          CCNT(2,ICNT)=YMIN+YRNG*FRAN()
          CCNT(3,ICNT)=ZMIN+ZRNG*FRAN()
          IF (ICNT.LE.NLOW) THEN
            CCNT(4,ICNT)=-1.
          ELSE
            CCNT(4,ICNT)=+1.
          END IF
  101   CONTINUE
C
        RETURN
C
      END


      FUNCTION TDGDVA (XPOS,YPOS,ZPOS)
C
        COMMON /TDGDCO/ XRNG,YRNG,ZRNG,NCNT,CCNT(4,50)
        SAVE   /TDGDCO/
C
        TDGDVA=0.
C
        DO 101 ICNT=1,NCNT
          TEMP=-50.*(((XPOS-CCNT(1,ICNT))/XRNG)**2+
     +               ((YPOS-CCNT(2,ICNT))/YRNG)**2+
     +               ((ZPOS-CCNT(3,ICNT))/YRNG)**2)
          IF (TEMP.GE.-20.) TDGDVA=TDGDVA+CCNT(4,ICNT)*EXP(TEMP)
  101   CONTINUE
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
