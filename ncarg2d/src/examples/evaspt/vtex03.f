

      PROGRAM TESTIT
C
C This program constructs a simple triangular mesh, including dummy flow
C data, and then traces streamlines on it.
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID, to be used in calls to GKS routines.  Use one
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
C Define a parameter saying how many pieces each edge of an icosahedron
C is to be broken into in forming the geodesic mesh.
C
C       PARAMETER (NDIV=3)
C       PARAMETER (NDIV=7)
C       PARAMETER (NDIV=15)
C       PARAMETER (NDIV=25)
        PARAMETER (NDIV=30)
C       PARAMETER (NDIV=45)
C       PARAMETER (NDIV=60)
C       PARAMETER (NDIV=80)
C       PARAMETER (NDIV=100)
C
C To represent the triangular mesh, we use three singly-dimensioned
C arrays: RPNT holds points, IEDG holds edges, and ITRI holds triangles.
C The elements of each array form "nodes" having lengths as follows:
C
        PARAMETER (LOPN=8)  !  length of a point node
C
C The eight elements of a point node are
C
C   1) the X coordinate of the point;
C   2) the Y coordinate of the point;
C   3) the Z coordinate of the point;
C   4) the X component of the flow field at the point;
C   5) the Y component of the flow field at the point;
C   6) the Z component of the flow field at the point;
C   7) the latitude of the point;
C   8) the longitude of the point.
C
C Latitude and longitude are not necessary; I'm using them for testing
C purposes.
C
        PARAMETER (LOEN=4)  !  length of an edge node
C
C The five elements of an edge node are
C
C   1) the base index, in RPNT, of point 1 of the edge;
C   2) the base index, in RPNT, of point 2 of the edge;
C   3) the index, in ITRI, of the pointer to the edge in the triangle
C      to the left of the edge (-1 if there is no triangle there);
C   4) the index, in ITRI, of the pointer to the edge in the triangle
C      to the right of the edge (-1 if there is no triangle there);
C   5) a utility flag for use by algorithms that scan the structure.
C
C The "left" and "right" sides of an edge are defined as they would be
C by an observer standing on the globe at point 1 of the edge, looking
C toward point 2 of the edge.  It is possible, if there are "holes" in
C the mesh, that there will be no triangle to the left or to the right
C of an edge, but there must be a triangle on at least one side of it.
C
        PARAMETER (LOTN=5)  !  length of a triangle node
C
C The five elements of a triangle node are
C
C   1) the base index, in IEDG, of edge 1 of the triangle;
C   2) the base index, in IEDG, of edge 2 of the triangle;
C   3) the base index, in IEDG, of edge 3 of the triangle;
C   4) a flag set non-zero to block use of the triangle, effectively
C      removing it from the mesh (different bits of this flag may be
C      used for different purposes, particularly when a 3D view is
C      being drawn);
C   5) a flag used by the streamline-drawing process.
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
C allow one to navigate the mesh, moving from triangle to triangle to
C follow a contour or a streamline; the pointers are tricky to define:
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
C The maximum number of points, edges, and triangles in a geodesic mesh
C may be computed from the parameter NDIV, which determines the order of
C the geodesic mesh:
C
        PARAMETER (MNOP=10*NDIV*NDIV+2)  !  maximum number of points
        PARAMETER (MNOE=30*NDIV*NDIV)    !  maximum number of edges
        PARAMETER (MNOT=20*NDIV*NDIV)    !  maximum number of triangles
C
C Once we know how many points, edges, and triangles we're going to use
C (at most), we can set parameters defining the space reserved for the
C triangular mesh:
C
        PARAMETER (MPNT=MNOP*LOPN)  !  space for points
        PARAMETER (MEDG=MNOE*LOEN)  !  space for edges
        PARAMETER (MTRI=MNOT*LOTN)  !  space for triangles
C
C Declare the arrays to hold the point nodes, edge nodes, and triangle
C nodes defining the triangular mesh.
C
        DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C Declare real and integer workspaces needed by VASPACKT.
C
        PARAMETER (LRWK=MTRI,LIWK=MTRI)
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare variables to hold labels.
C
        CHARACTER*64 UNLB,VNLB,WNLB,UILB,VILB,WILB
C 
C Declare a variable to hold an area map for masking purposes.
C 
        PARAMETER (LAMA=10000)
C       
        DIMENSION IAMA(LAMA)
C 
C Declare some arrays to be used in defining a masked-out area.
C   
        DIMENSION XMSK(5),YMSK(5)
C   
C Declare the routine that will do the masked drawing of vectors.
C   
        EXTERNAL VTDRPL
C
C Set the desired minimum and maximum values of U, V, and W.
C
        DATA UMIN,VMIN,WMIN,UMAX,VMAX,WMAX / -1.,-1.,-1.,+1.,+1.,+1. /
C
C Set the desired values of parameters determining the eye position.
C ANG1 is a bearing angle, ANG2 is an elevation angle, and RMUL is a
C multiplier of the length of the diagonal of the data box, specifying
C the distance from the center of the box to the eye.
C
C       DATA ANG1,ANG2,RMUL / 115., 25.,1.5 /
C       DATA ANG1,ANG2,RMUL / 210., 30.,1.5 /
C       DATA ANG1,ANG2,RMUL / 280.,-45.,1.5 /
C       DATA ANG1,ANG2,RMUL / 130., 45.,1.5 /
        DATA ANG1,ANG2,RMUL / 210.,-45.,1.5 /
C       DATA ANG1,ANG2,RMUL / 300.,-45.,1.5 /
C       DATA ANG1,ANG2,RMUL / 210., 80.,1.5 /
C       DATA ANG1,ANG2,RMUL / 300.,  5.,1.5 /
C       DATA ANG1,ANG2,RMUL /  30.,  5.,1.5 /
C
C ISTE is a flag that says whether to do a simple image (ISTE=0),
C a one-frame stereo image (ISTE=-1), or a two-frame stereo image
C (ISTE=+1).
C
        DATA ISTE / -1 /
C
C ASTE is the desired angle (in degrees) between the lines of sight for
C a pair of stereo views.
C
        DATA ASTE / 4. /
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
C Define the conversion constant from degrees to radians.
C
        DATA DTOR / .017453292519943 /
C
C Define labels for the edges of the box.
C
        DATA UNLB / ' -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ' /
        DATA VNLB / ' -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ' /
        DATA WNLB / ' -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ' /
C
        DATA UILB / 'U Coordinate Values' /
        DATA VILB / 'V Coordinate Values' /
        DATA WILB / 'W Coordinate Values' /
C
C Define the desired value of the parameter 'CTV', which says whether
C or not streamlines are to be colored and, if so, how:
C
C   ICTV = 0 => no coloring
C   ICTV < 0 => colored, user defines color threshold values used
C   ICTV > 0 => colored, VTMESH defines color threshold values used
C
C If ABS(ICTV) is between 1 and LOPN, inclusive, then colors will be
C determined from elements having index ABS(ICTV) in the point nodes:
C otherwise, colors will be determined from the vector magnitudes at
C the points.
C
C       DATA ICTV / -100 /  !  vector magnitudes     !  user values  !
C       DATA ICTV /   -8 /  !  longitude             !  user values  !
C       DATA ICTV /   -7 /  !  latitude              !  user values  !
C       DATA ICTV /   -6 /  !  W velocity component  !  user values  !
C       DATA ICTV /   -5 /  !  V velocity component  !  user values  !
C       DATA ICTV /   -4 /  !  U velocity component  !  user values  !
C       DATA ICTV /   -3 /  !  W coordinate          !  user values  !
C       DATA ICTV /   -2 /  !  V coordinate          !  user values  !
C       DATA ICTV /   -1 /  !  U coordinate          !  user values  !
C       DATA ICTV /    0 /  !  uncolored
C       DATA ICTV /   +1 /  !  U coordinate          !  auto values  !
C       DATA ICTV /   +2 /  !  V coordinate          !  auto values  !
C       DATA ICTV /   +3 /  !  W coordinate          !  auto values  !
C       DATA ICTV /   +4 /  !  U velocity component  !  auto values  !
C       DATA ICTV /   +5 /  !  V velocity component  !  auto values  !
C       DATA ICTV /   +6 /  !  W velocity component  !  auto values  !
C       DATA ICTV /   +7 /  !  latitude              !  auto values  !
C       DATA ICTV /   +8 /  !  longitude             !  auto values  !
        DATA ICTV / +100 /  !  vector magnitudes     !  auto values  !
C
C Define the desired value of the parameter 'NLV', which is the number
C of colors to be used.
C
C       DATA NCLR /   1 /
        DATA NCLR / 100 /
C
C Define the parameter that tells VASPACKT how to interpret size
C parameters.
C
        DATA IISP / 0 /
C       DATA IISP / 1 /
C
C Define the segment length for drawing streamlines.
C
        DATA SLPS / .001 /
C       DATA SLPS / .002 /
C       DATA SLPS / .005 /
C       DATA SLPS / .010 /
C       DATA SLPS / .020 /
C
C Define the maximum length of a streamline.
C
C       DATA SLLN / 8.0 /
C       DATA SLLN / 4.0 /
        DATA SLLN / 2.0 / ! <--
C       DATA SLLN / 1.0 /
C       DATA SLLN / .50 /
C       DATA SLLN / .10 /
C       DATA SLLN / .05 /
C
C Define the streamline spacing and the termination test parameters.
C
C       DATA SLSP,TTSP,TTLL / .012 , .003 , .006 /
C       DATA SLSP,TTSP,TTLL / .024 , .006 , .012 /
C       DATA SLSP,TTSP,TTLL / .036 , .009 , .018 /
        DATA SLSP,TTSP,TTLL / .048 , .012 , .024 / ! <--
C       DATA SLSP,TTSP,TTLL / .072 , .018 , .036 /
C       DATA SLSP,TTSP,TTLL / .096 , .024 , .048 /
C       DATA SLSP,TTSP,TTLL / .144 , .036 , .072 /
C       DATA SLSP,TTSP,TTLL / .192 , .048 , .096 /
C
C Define the values of VASPACKT's arrowhead parameters.
C
        DATA AHSP,AHAW,AHLN / .24 , 30. , .03 /
C
C Define the value of the debug flag and a trio of required color
C indices for VASPACKT.
C
        DATA IDBG,ICSG,ICTG,ICTT / 0 , 7 , 10 , 8 /
C
C Define the desired value of VASPACKT's point interpolation threshold.
C
C       DATA PITH /  0. /  !  point interpolation off
        DATA PITH / .01 /
C
C Define a parameter that says whether or not masking is to be tested.
C
        DATA IMSK / 0 /
C
C Define the coordinates of the area to be masked out.
C
        DATA XMSK / .3 , .7 , .7 , .3 , .3 /
        DATA YMSK / .3 , .3 , .7 , .7 , .3 /
C
C Define the value of 'VVM', which depends on the range of the data.
C
        DATA VVMM / .001 /
C
C Create data and generate the required triangular mesh.
C
        PRINT * , ' '
        PRINT * , 'CREATING TRIANGULAR MESH:'
C
        CALL GTGEO1 (RPNT,MPNT,NPNT,LOPN,  !  point list
     +               IEDG,MEDG,NEDG,LOEN,  !  edge list
     +               ITRI,MTRI,NTRI,LOTN,  !  triangle list
     +               NDIV)
C
C Print the number of points, edges, and triangles.
C
        PRINT * , '  NUMBER OF POINTS:    ',NPNT/LOPN
        PRINT * , '  NUMBER OF EDGES:     ',NEDG/LOEN
        PRINT * , '  NUMBER OF TRIANGLES: ',NTRI/LOTN
C
C Modify the dummy velocity-vector data generated by GTGEO1 to make it
C smoother.  Basically, each velocity vector points a little north of
C due east, with some smooth variation based on the original data.
C
        DO 101 I=0,NPNT-LOPN,LOPN
          DNOM=SQRT(RPNT(I+1)**2+RPNT(I+2)**2+RPNT(I+3)**2)
          RPNT(I+1)=RPNT(I+1)/DNOM
          RPNT(I+2)=RPNT(I+2)/DNOM
          RPNT(I+3)=RPNT(I+3)/DNOM
          CALL XYZLLP (RPNT(I+1),RPNT(I+2),RPNT(I+3),RLAT,RLON)
          CALL LLPXYZ (MAX(-90.,MIN(+90.,RLAT+.25)),RLON+2.,
     +                                                   RVOX,RVOY,RVOZ)
          RVOX=RVOX+RPNT(I+4)/15.
          RVOY=RVOY+RPNT(I+5)/15.
          RVOZ=RVOZ+RPNT(I+6)/15.
          DNOM=SQRT(RVOX**2+RVOY**2+RVOZ**2)
          RPNT(I+4)=RVOX/DNOM-RPNT(I+1)
          RPNT(I+5)=RVOY/DNOM-RPNT(I+2)
          RPNT(I+6)=RVOZ/DNOM-RPNT(I+3)
          RPNT(I+7)=RLAT
          RPNT(I+8)=RLON
  101   CONTINUE
C
C Open GKS.
C
        CALL GOPKS (IERF,0)
        CALL GOPWK (IWID,LUNI,IWTY)
        CALL GACWK (IWID)
C
C Turn clipping off.
C
        CALL GSCLIP (0)
C
C Define some colors to use.
C
        CALL GSCR   (IWID, 0,1.,1.,1.)  !  white (background)
        CALL GSCR   (IWID, 1,0.,0.,0.)  !  black (foreground)
        CALL GSCR   (IWID, 2,.8,.8,.8)  !  gray (mesh lines)
        CALL GSCR   (IWID, 3,1.,0.,0.)  !  red
        CALL GSCR   (IWID, 4,0.,1.,0.)  !  green
        CALL GSCR   (IWID, 5,0.,0.,1.)  !  blue
        CALL GSCR   (IWID, 6,1.,1.,0.)  !  yellow
        CALL GSCR   (IWID, 7,1.,.7,1.)  !  magenta
        CALL GSCR   (IWID, 8,.7,1.,1.)  !  cyan
        CALL GSCR   (IWID, 9,.6,.3,.3)  !  reddish-gray (streamlines)
        CALL GSCR   (IWID,10,.3,.6,.3)  !  greenish-gray (subtriangles)
C       
C Define 100 colors, associated with color indices 151 through 250, to
C be used for color-coding along streamlines, ranging from one color to
C another.
C
        CALL DFCRGB (IWID,151,150+NCLR,0.,0.,1.,1.,0.,0.,0.)
C
C Initialize the area map as directed.
C
        IF (IMSK.EQ.0) THEN
          IAMA(1)=0
        ELSE
          CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
          CALL ARINAM (IAMA,LAMA)
          CALL AREDAM (IAMA,XMSK,YMSK,5,1,-1,0)
        END IF
C
C Select font number 25, turn on the outlining of filled fonts, set the
C line width to 1, and set the outline color to the foreground color.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
        CALL PCSETR ('OL - OUTLINE LINE WIDTH',1.)
        CALL PCSETI ('OC - OUTLINE LINE COLOR',1)
C
C Specify the field of view to be used.
C
        CALL TDSETI ('FOV',40)
C
C Make TDPACK characters a bit bigger.
C
        CALL TDSETR ('CS1',1.25)
C
C Tell VASPACKT what the minimum velocity vector magnitude is to be.
C
        CALL VTSETR ('VVM - VELOCITY VECTOR MINIMUM MAGNITUDE',VVMM)
C
C Set VASPACKT's size-interpretation parameter.
C
        CALL VTSETI ('ISP',IISP)
C
C Set VASPACKT's streamline length.
C
        CALL VTSETR ('SLL',SLLN)
C
C Set VASPACKT's streamline segment length.
C
        CALL VTSETR ('SLP',SLPS)
C
C Set VASPACKT's streamline spacing.
C
        CALL VTSETR ('SLS',SLSP)
C
C Set VASPACKT's termination test parameters.
C
        CALL VTSETR ('TTS',TTSP)
        CALL VTSETR ('TTL',TTLL)
C
C Set VASPACKT's arrowhead parameters.
C
        CALL VTSETR ('AHS',AHSP)
        CALL VTSETR ('AHA',AHAW)
        CALL VTSETR ('AHL',AHLN)
C
C Set VASPACKT's debug flag and a trio of color indices.
C
        CALL VTSETI ('DBG',IDBG)
        CALL VTSETI ('SGC',ICSG)
        CALL VTSETI ('STC',ICTG)
        CALL VTSETI ('TTC',ICTT)
C
C Set VASPACKT's point interpolation threshold value.
C
        CALL VTSETR ('PIT',PITH)
C
C Set VASPACKT parameters having to do with coloring of streamlines.
C
        CALL VTSETI ('CTV - COLOR THRESHOLD VALUE CONTROL',ICTV)
C
C Initialize coloring of the streamlines.
C
        IF (ICTV.NE.0.AND.NCLR.NE.0) THEN
C
          IF (ICTV.LT.0) THEN
C
            DO 102 I=0,NPNT-LOPN,LOPN
              IF (ABS(ICTV).LE.LOPN) THEN
                TMAG=RPNT(I+ABS(ICTV))
              ELSE
                TMAG=SQRT(RPNT(I+4)**2+RPNT(I+5)**2+RPNT(I+6)**2)
              END IF
              IF (I.EQ.0) THEN
                TMIN=TMAG
                TMAX=TMAG
              ELSE
                TMIN=MIN(TMIN,TMAG)
                TMAX=MAX(TMAX,TMAG)
              END IF
  102       CONTINUE
C
          END IF
C
          CALL VTSETI ('NLV - NUMBER OF COLOR LEVELS',NCLR)
C
          DO 103 I=1,NCLR
            CALL VTSETI ('PAI - PARAMETER ARRRAY INDEX',I)
            CALL VTSETI ('CLR - GKS COLOR INDEX',150+I)
            IF (ICTV.LT.0) THEN
              CALL VTSETR ('TVL',
     +                     TMIN+(TMAX-TMIN)*(REAL(I)/REAL(NCLR+1))**2)
            END IF
  103     CONTINUE
C
        END IF
C
C Tell VASPACKT to use the TDPACK mapping (implies the use of a special
C version of VTMXYZ).
C
        CALL VTSETI ('MAP - MAPPING FLAG',2)
C
C Tell VASPACKT not to do its own call to SET, since TDPACK will have
C done it.
C
        CALL VTSETI ('SET - DO-SET-CALL FLAG', 0)
C
C Find the midpoint of the data box (to be used as the point looked at).
C
        UMID=.5*(UMIN+UMAX)
        VMID=.5*(VMIN+VMAX)
        WMID=.5*(WMIN+WMAX)
C
C Determine the distance (RDST) from which the data box will be viewed
C and, given that, the eye position.
C
        RDST=RMUL*SQRT((UMAX-UMIN)**2+(VMAX-VMIN)**2+(WMAX-WMIN)**2)
C
        UEYE=UMID+RDST*COS(DTOR*ANG1)*COS(DTOR*ANG2)
        VEYE=VMID+RDST*SIN(DTOR*ANG1)*COS(DTOR*ANG2)
        WEYE=WMID+RDST*SIN(DTOR*ANG2)
C
C SET THE TRIANGLE BLOCKING FLAGS FOR THE VIEWS TO BE DRAWN.
C --- --- -------- -------- ----- --- --- ----- -- -- ------
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
        IF (ISTE.EQ.0) THEN
          OTEP=0.                       !  (single view)
        ELSE
          OTEP=-RDST*TAN(DTOR*ASTE/2.)  !  (left-eye view)
        END IF
C
C Initialize TDPACK.
C
  104   CALL TDINIT (UEYE,VEYE,WEYE,UMID,VMID,WMID,
     +                              UMID,VMID,WMID+RDST,OTEP)
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
     +                           XWDL,XWDR,YWDB,YWDT,LNLG)
            ELSE
              CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                           XWDL,XWDR,YWDB,YWDT,LNLG)
            END IF
          END IF
        END IF
C
C Initialize VASPACKT.
C
        CALL VTMESH (RPNT,NPNT,LOPN,  !  point list
     +               IEDG,NEDG,LOEN,  !  edge list
     +               ITRI,NTRI,LOTN,  !  triangle list
     +               RWRK,LRWK,       !  real workspace
     +               IWRK,LIWK)       !  integer workspace
C
C Retrieve and print the average edge length.
C
        CALL VTGETR ('AEL',AVEL)
        PRINT * , '  AVERAGE LENGTH OF EDGES: ',AVEL
C
C VTTDBF is called to set blocking flag bits for triangles according to
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
C VTTDBF; a user may set it to block the triangle.
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
C drawn).
C
        CALL VTTDBF (RPNT,IEDG,ITRI,RWRK,IWRK,1,2.)
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
        IF (OTEP.LT.0.) THEN
          OTEP=-OTEP
          GO TO 104
        END IF
C
C Once bits B6, B5, B4, B3, B2, B1, and B0 are set in all the triangle
C blocking flags, one may use them to determine what is drawn in a
C particular view.  This is done by calling the routine VTTDBM, which
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
C       CALL VTTDBM (0,0,0,0,1,1,0,1)
C
C DRAW THE TRIANGULAR MESH WITH STREAMLINES ON IT.
C ---- --- ---------- ---- ---- ----------- -- ---
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
        IF (ISTE.EQ.0) THEN
          OTEP=0.                       !  (single view)
        ELSE
          OTEP=-RDST*TAN(DTOR*ASTE/2.)  !  (left-eye view)
        END IF
C
C Initialize TDPACK.
C
  105   CALL TDINIT (UEYE,VEYE,WEYE,UMID,VMID,WMID,
     +                              UMID,VMID,WMID+RDST,OTEP)
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
     +                           XWDL,XWDR,YWDB,YWDT,LNLG)
            ELSE
              CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                           XWDL,XWDR,YWDB,YWDT,LNLG)
            END IF
          ELSE
            IF (OTEP.GT.0.) CALL FRAME
          END IF
        END IF
C
C Call VTTDBM, as described above.  We don't bother checking for hidden
C triangles, because, for a geodesic mesh, we know that all triangles
C that are hidden are viewed from the wrong side and it's easier to do
C that check.
C
C Set masks for 3-D| t t t t u u u u | toggle/use
C triangle blocking| h e w u h e w u | hidden/edge-on/wrong-side/user
C
        CALL VTTDBM (0,0,0,0,0,1,1,1)
C
C Draw labels for the axes.
C
        CALL TDLBLS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,
     +               UNLB,VNLB,WNLB,UILB,VILB,WILB,1)
C
C Draw the sides of the box that could be hidden.
C
        CALL TDGRDS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,
     +               .1*(UMAX-UMIN),.1*(VMAX-VMIN),.1*(WMAX-WMIN),
     +                                                       12,1)
C
C Color the visible triangles of the mesh in yellow.
C
        CALL GSFACI (6)
C
        CALL VTTDFM (RPNT,IEDG,ITRI,RWRK,IWRK)
C
        CALL GSFACI (1)
C
C Draw the triangular mesh in a light gray.
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (2)
        CALL GSLWSC (1.)
C
        CALL VTTDDM (RPNT,IEDG,ITRI,RWRK,IWRK,0)
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (1)
        CALL GSLWSC (1.)
C
C Draw the streamlines, either in a reddish-gray or in varying
C colors, depending on the value of ICTV, above.
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (9)
C
        CALL VTSLDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,VTDRPL)
C
        CALL PLOTIF (0.,0.,2)
        CALL GSPLCI (1)
C
C Draw the sides of the box that could not be hidden.
C
        CALL TDGRDS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,
     +               .1*(UMAX-UMIN),.1*(VMAX-VMIN),.1*(WMAX-WMIN),
     +                                                       12,0)
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
C Draw another frame or two using EZMAP.
C
        DO 107 IFRA=1,2
C
          DO 106 I=0,NTRI-LOTN,LOTN
            ITRI(I+4)=0
  106     CONTINUE
C
          IF (IFRA.EQ.1) THEN
            CALL MAPROJ ('CE',40.,-105.,0.)
          ELSE IF (IFRA.EQ.2) THEN
            CALL MAPROJ ('LE',40.,-105.,0.)
C           CALL MAPROJ ('AE',40.,-105.,0.)
C           CALL MAPROJ ('ME',40.,-105.,0.)
C           CALL MAPROJ ('MO',40.,-105.,0.)
C           CALL MAPROJ ('RO',40.,-105.,0.)
C           CALL MAPROJ ('OR',40.,-105.,0.)
C           CALL MAPROJ ('SV',40.,-105.,0.)
C           CALL MAPROJ ('LC',40.,-105.,60.)
C           CALL MAPROJ ('ST',40.,-105.,60.)
C           CALL MAPROJ ('GN',40.,-105.,60.)
          END IF
C
          CALL MAPDRW
C
          CALL VTSETI ('MAP - MAPPING FLAG',1)
          CALL VTSETR ('ORV - OUT-OF-RANGE FLAG',1.E12)
C
          CALL VTMESH (RPNT,NPNT,LOPN,  !  point list
     +                 IEDG,NEDG,LOEN,  !  edge list
     +                 ITRI,NTRI,LOTN,  !  triangle list
     +                 RWRK,LRWK,       !  real workspace
     +                 IWRK,LIWK)       !  integer workspace
C
          CALL PLOTIF (0.,0.,2)
          CALL GSPLCI (9)
C
          CALL VTSLDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,VTDRPL)
C
          CALL PLOTIF (0.,0.,2)
          CALL GSPLCI (1)
C
          CALL FRAME
C
  107   CONTINUE
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


      SUBROUTINE GTGEO1 (RPNT,MPNT,NPNT,LOPN,
     +                   IEDG,MEDG,NEDG,LOEN,
     +                   ITRI,MTRI,NTRI,LOTN,
     +                   NDIV)
C
        DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C Construct a triangular mesh representing a geodesic sphere as an
C exercise to demonstrate its feasibility and to see how the timing
C varies as a function of the resolution of the mesh.
C
C Declare arrays to hold information describing an icosahedron.
C
        DIMENSION JFCE(3,20),JEDG(2,30),XCVI(12),YCVI(12),ZCVI(12)
C
C Define the twenty faces of an icosahedron (pointers to edges).
C
        DATA JFCE(1, 1),JFCE(2, 1),JFCE(3, 1) /  1,11, 2 /
        DATA JFCE(1, 2),JFCE(2, 2),JFCE(3, 2) /  2,19, 3 /
        DATA JFCE(1, 3),JFCE(2, 3),JFCE(3, 3) /  3,25, 4 /
        DATA JFCE(1, 4),JFCE(2, 4),JFCE(3, 4) /  4,29, 5 /
        DATA JFCE(1, 5),JFCE(2, 5),JFCE(3, 5) /  1, 5,14 /
        DATA JFCE(1, 6),JFCE(2, 6),JFCE(3, 6) / 12,14,28 /
        DATA JFCE(1, 7),JFCE(2, 7),JFCE(3, 7) / 12,27,13 /
        DATA JFCE(1, 8),JFCE(2, 8),JFCE(3, 8) / 11,13,20 /
        DATA JFCE(1, 9),JFCE(2, 9),JFCE(3, 9) / 20,30,21 /
        DATA JFCE(1,10),JFCE(2,10),JFCE(3,10) / 19,21,26 /
        DATA JFCE(1,11),JFCE(2,11),JFCE(3,11) / 16,26,18 /
        DATA JFCE(1,12),JFCE(2,12),JFCE(3,12) / 16,17,25 /
        DATA JFCE(1,13),JFCE(2,13),JFCE(3,13) / 15,23,17 /
        DATA JFCE(1,14),JFCE(2,14),JFCE(3,14) / 23,24,29 /
        DATA JFCE(1,15),JFCE(2,15),JFCE(3,15) / 22,28,24 /
        DATA JFCE(1,16),JFCE(2,16),JFCE(3,16) /  7, 8,22 /
        DATA JFCE(1,17),JFCE(2,17),JFCE(3,17) /  6, 7,15 /
        DATA JFCE(1,18),JFCE(2,18),JFCE(3,18) /  6,18,10 /
        DATA JFCE(1,19),JFCE(2,19),JFCE(3,19) /  9,10,30 /
        DATA JFCE(1,20),JFCE(2,20),JFCE(3,20) /  8, 9,27 /
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
C Define the 12 vertices of the icosahedron (note radius less than one).
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
C Check array sizes to see if we have enough space.
C
        IF ((10*NDIV*NDIV+2)*LOPN.GT.MPNT) THEN
          PRINT * , ' '
          PRINT * , 'GTGEO1 - STOP - POINT ARRAY TOO SMALL'
          STOP
        END IF
C
        IF (30*NDIV*NDIV*LOEN.GT.MEDG) THEN
          PRINT * , ' '
          PRINT * , 'GTGEO1 - STOP - EDGE ARRAY TOO SMALL'
          STOP
        END IF
C
        IF (20*NDIV*NDIV*LOTN.GT.MTRI) THEN
          PRINT * , ' '
          PRINT * , 'GTGEO1 - STOP - TRIANGLE ARRAY TOO SMALL'
          STOP
        END IF
C
C Initialize the number of points, edges, and triangles.
C
        NPNT=0
        NEDG=0
        NTRI=0
C
C Put the twelve vertices of the icosahedron in the point list; modify
C each to lie on the surface of a sphere of radius 1.
C
        DO 101 I=1,12
          RPNT(NPNT+1)=XCVI(I)
          RPNT(NPNT+2)=YCVI(I)
          RPNT(NPNT+3)=ZCVI(I)
          NPNT=NPNT+LOPN
  101   CONTINUE
C
C On each edge of the icosahedron, interpolate NDIV-1 points and put
C them in the point list.
C
        DO 103 I=1,30
          IPPA=(JEDG(1,I)-1)*LOPN
          IPPB=(JEDG(2,I)-1)*LOPN
          DO 102 J=1,NDIV-1
            RPNT(NPNT+1)=(REAL(NDIV-J)*RPNT(IPPA+1)+
     +                    REAL(     J)*RPNT(IPPB+1))/REAL(NDIV)
            RPNT(NPNT+2)=(REAL(NDIV-J)*RPNT(IPPA+2)+
     +                    REAL(     J)*RPNT(IPPB+2))/REAL(NDIV)
            RPNT(NPNT+3)=(REAL(NDIV-J)*RPNT(IPPA+3)+
     +                    REAL(     J)*RPNT(IPPB+3))/REAL(NDIV)
            NPNT=NPNT+LOPN
  102     CONTINUE
  103   CONTINUE
C
C On each edge of the icosahedron, generate NDIV pieces of the edge and
C put them in the edge list.
C
        DO 105 I=1,30
          IPP2=(JEDG(1,I)-1)*LOPN
          DO 104 J=1,NDIV
            IPP1=IPP2
            IF (J.LT.NDIV) THEN
              IPP2=(12+(I-1)*(NDIV-1)+J-1)*LOPN
            ELSE
              IPP2=(JEDG(2,I)-1)*LOPN
            END IF
            IEDG(NEDG+1)=IPP1
            IEDG(NEDG+2)=IPP2
            IEDG(NEDG+3)=0
            IEDG(NEDG+4)=-1
            NEDG=NEDG+LOEN
  104     CONTINUE
  105   CONTINUE
C
C On each face of the icosahedron, generate (NDIV-2)*(NDIV-1)/2 interior
C points and put them in the point list (completing it).  Also generate
C 3*(NDIV-1)*NDIV/2 interior edges and put them in the edge list (which
C completes it).  Also generate NDIV*NDIV triangles and put them in the
C triangle list (completing it).
C
        DO 110 I=1,20
          IF (JEDG(1,JFCE(1,I)).EQ.JEDG(1,JFCE(2,I)).OR.
     +        JEDG(1,JFCE(1,I)).EQ.JEDG(2,JFCE(2,I))) THEN
            IPPA=(12+(JFCE(1,I)-1)*(NDIV-1)-1)*LOPN
            IIPA=LOPN
            IPEA=((JFCE(1,I)-1)*NDIV-1)*LOEN
            IIEA=LOEN
          ELSE
            IPPA=(12+(JFCE(1,I)-1)*(NDIV-1)+NDIV-1)*LOPN
            IIPA=-LOPN
            IPEA=((JFCE(1,I)-1)*NDIV+NDIV)*LOEN
            IIEA=-LOEN
          END IF
          IF (JEDG(1,JFCE(3,I)).EQ.JEDG(1,JFCE(2,I)).OR.
     +        JEDG(1,JFCE(3,I)).EQ.JEDG(2,JFCE(2,I))) THEN
            IPPB=(12+(JFCE(3,I)-1)*(NDIV-1)-1)*LOPN
            IIPB=LOPN
            IPEB=((JFCE(3,I)-1)*NDIV-1)*LOEN
            IIEB=LOEN
          ELSE
            IPPB=(12+(JFCE(3,I)-1)*(NDIV-1)+NDIV-1)*LOPN
            IIPB=-LOPN
            IPEB=((JFCE(3,I)-1)*NDIV+NDIV)*LOEN
            IIEB=-LOEN
          END IF
          IF (JEDG(1,JFCE(2,I)).EQ.JEDG(1,JFCE(1,I)).OR.
     +        JEDG(1,JFCE(2,I)).EQ.JEDG(2,JFCE(1,I))) THEN
            IPEC=((JFCE(2,I)-1)*NDIV)*LOEN
            IIEC=LOEN
          ELSE
            IPEC=((JFCE(2,I)-1)*NDIV+NDIV-1)*LOEN
            IIEC=-LOEN
          END IF
          DO 109 J=1,NDIV
            IPPA=IPPA+IIPA
            IPPB=IPPB+IIPB
            IPEA=IPEA+IIEA
            IPEB=IPEB+IIEB
            DO 106 K=1,NDIV-J-1
              RPNT(NPNT+1)=(REAL(NDIV-J-K)*RPNT(IPPA+1)+
     +                      REAL(       K)*RPNT(IPPB+1))/REAL(NDIV-J)
              RPNT(NPNT+2)=(REAL(NDIV-J-K)*RPNT(IPPA+2)+
     +                      REAL(       K)*RPNT(IPPB+2))/REAL(NDIV-J)
              RPNT(NPNT+3)=(REAL(NDIV-J-K)*RPNT(IPPA+3)+
     +                      REAL(       K)*RPNT(IPPB+3))/REAL(NDIV-J)
              NPNT=NPNT+LOPN
  106       CONTINUE
            IPED=NEDG
            IIED=LOEN
            IPP2=IPPA
            DO 107 K=1,NDIV-J
              IPP1=IPP2
              IF (K.LT.NDIV-J) THEN
                IPP2=NPNT-(NDIV-J-K)*LOPN
              ELSE
                IPP2=IPPB
              END IF
              IEDG(NEDG+1)=IPP1
              IEDG(NEDG+2)=IPP2
              IEDG(NEDG+3)=-1
              IEDG(NEDG+4)=-1
              NEDG=NEDG+LOEN
  107       CONTINUE
            IPEF=IPEA
            DO 108 K=0,2*(NDIV-J)
              IF (MOD(K,2).EQ.0) THEN
                IF (K.LT.(NDIV-J)*2) THEN
                  IPEG=NEDG
                  IF (IIEC.GT.0) THEN
                    IEDG(NEDG+1)=IEDG(IPEC+(K/2)*IIEC+2)
                  ELSE
                    IEDG(NEDG+1)=IEDG(IPEC+(K/2)*IIEC+1)
                  END IF
                  IF (IIED.GT.0) THEN
                    IEDG(NEDG+2)=IEDG(IPED+(K/2)*IIED+1)
                  ELSE
                    IEDG(NEDG+2)=IEDG(IPED+(K/2)*IIED+2)
                  END IF
                  IEDG(NEDG+3)=-1
                  IEDG(NEDG+4)=-1
                  NEDG=NEDG+LOEN
                ELSE
                  IPEG=IPEB
                END IF
                ITRI(NTRI+1)=IPEF
                ITRI(NTRI+2)=IPEC+(K/2)*IIEC
                ITRI(NTRI+3)=IPEG
                ITRI(NTRI+4)=0
                NTRI=NTRI+LOTN
                CALL STPIEN (ITRI,NTRI-LOTN,IEDG)
              ELSE
                IPEG=NEDG
                IF (IIEC.GT.0) THEN
                  IEDG(NEDG+1)=IEDG(IPEC+(K/2)*IIEC+2)
                ELSE
                  IEDG(NEDG+1)=IEDG(IPEC+(K/2)*IIEC+1)
                END IF
                IF (IIED.GT.0) THEN
                  IEDG(NEDG+2)=IEDG(IPED+(K/2)*IIED+2)
                ELSE
                  IEDG(NEDG+2)=IEDG(IPED+(K/2)*IIED+1)
                END IF
                IEDG(NEDG+3)=-1
                IEDG(NEDG+4)=-1
                NEDG=NEDG+LOEN
                ITRI(NTRI+1)=IPEF
                ITRI(NTRI+2)=IPEG
                ITRI(NTRI+3)=IPED+(K/2)*IIEC
                ITRI(NTRI+4)=0
                NTRI=NTRI+LOTN
                CALL STPIEN (ITRI,NTRI-LOTN,IEDG)
              END IF
              IPEF=IPEG
  108       CONTINUE
            IPEC=IPED
            IIEC=IIED
  109     CONTINUE
  110   CONTINUE
C
C Blow the elaborated icosahedron up into a sphere, supply dummy data
C at each of its vertices and clear the "additional info" slots in the
C point nodes.
C
        DO 111 I=0,NPNT-LOPN,LOPN
          DNOM=SQRT(RPNT(I+1)**2+RPNT(I+2)**2+RPNT(I+3)**2)
          RPNT(I+1)=.9*RPNT(I+1)/DNOM
          RPNT(I+2)=.9*RPNT(I+2)/DNOM
          RPNT(I+3)=.9*RPNT(I+3)/DNOM
  111   CONTINUE
C
C Initialize the dummy-data generator and generate X flow components.
C
        CALL TDGDIN (-1.,1.,-1.,1.,-1.,1.,21,21)
C
        DO 112 I=0,NPNT-LOPN,LOPN
          RPNT(I+4)=TDGDVA(RPNT(I+1),RPNT(I+2),RPNT(I+3))
  112   CONTINUE
C
C Initialize the dummy-data generator and generate Y flow components.
C
        CALL TDGDIN (-1.,1.,-1.,1.,-1.,1.,21,21)
C
        DO 113 I=0,NPNT-LOPN,LOPN
          RPNT(I+5)=TDGDVA(RPNT(I+1),RPNT(I+2),RPNT(I+3))
  113   CONTINUE
C
C Initialize the dummy-data generator and generate Z flow components.
C
        CALL TDGDIN (-1.,1.,-1.,1.,-1.,1.,21,21)
C
        DO 114 I=0,NPNT-LOPN,LOPN
          RPNT(I+6)=TDGDVA(RPNT(I+1),RPNT(I+2),RPNT(I+3))
  114   CONTINUE
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE STPIEN (ITRI,NTRI,IEDG)
C
        DIMENSION ITRI(*),IEDG(*)
C
C This subroutine sets the triangle pointers in the edge nodes pointed
C to by a triangle node just defined.
C
        IF (IEDG(ITRI(NTRI+1)+2).EQ.IEDG(ITRI(NTRI+2)+1).OR.
     +      IEDG(ITRI(NTRI+1)+2).EQ.IEDG(ITRI(NTRI+2)+2)) THEN
          IEDG(ITRI(NTRI+1)+3)=NTRI+1
        ELSE
          IEDG(ITRI(NTRI+1)+4)=NTRI+1
        END IF
C
        IF (IEDG(ITRI(NTRI+2)+2).EQ.IEDG(ITRI(NTRI+3)+1).OR.
     +      IEDG(ITRI(NTRI+2)+2).EQ.IEDG(ITRI(NTRI+3)+2)) THEN
          IEDG(ITRI(NTRI+2)+3)=NTRI+2
        ELSE
          IEDG(ITRI(NTRI+2)+4)=NTRI+2
        END IF
C
        IF (IEDG(ITRI(NTRI+3)+2).EQ.IEDG(ITRI(NTRI+1)+1).OR.
     +      IEDG(ITRI(NTRI+3)+2).EQ.IEDG(ITRI(NTRI+1)+2)) THEN
          IEDG(ITRI(NTRI+3)+3)=NTRI+3
        ELSE
          IEDG(ITRI(NTRI+3)+4)=NTRI+3
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
C         CCNT(1,ICNT)=XMIN+XRNG*FRAN()
C         CCNT(2,ICNT)=YMIN+YRNG*FRAN()
C         CCNT(3,ICNT)=ZMIN+ZRNG*FRAN()
          CCNT(1,ICNT)=XMIN+XRNG*GRAN()
          CCNT(2,ICNT)=YMIN+YRNG*GRAN()
          CCNT(3,ICNT)=ZMIN+ZRNG*GRAN()
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


      FUNCTION GRAN ()
C
C This function generates a sequence of "random" numbers.  Obviously,
C it repeats after the 100th such number.  This is not very important,
C because of the way in which these numbers are being used.
C
      DIMENSION RSEQ (100)
      SAVE ISEQ
      DATA RSEQ / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     +            .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     +            .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     +            .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     +            .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     +            .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     +            .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     +            .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     +            .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     +            .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
      DATA ISEQ / 0 /
      ISEQ=MOD(ISEQ,100)+1
      GRAN=RSEQ(ISEQ)
      RETURN
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
        IF (RVOX.NE.0..OR.RVOY.NE.0.) THEN
          RLON=RTOD*ATAN2(RVOY,RVOX)
        ELSE
          RLON=0.
        END IF
C
        RETURN
C
      END


C THE FOLLOWING ROUTINES SHOULD BECOME A PART OF TDPACK.


      SUBROUTINE TDAROW (UCP1,VCP1,WCP1,UCP2,VCP2,WCP2,ARHL,ARHW)
C
        DIMENSION UCRV(2),VCRV(2),WCRV(2)
C
C Draw the projection of an arrow from the point (UCP1,VCP1,WCP1) to the
C point (UCP2,VCP2,WCP2).  ARHL and ARHW define the length and width of
C the arrowhead.
C
C First, define a two-point "curve".
C
        UCRV(1)=UCP1
        VCRV(1)=VCP1
        WCRV(1)=WCP1
C
        UCRV(2)=UCP2
        VCRV(2)=VCP2
        WCRV(2)=WCP2
C
C Use a TDPACK routine to draw the projection of the "curve" with a
C cone-shaped arrowhead on the end of it.
C
        CALL TDCURV (UCRV,VCRV,WCRV,2,1,AHLN,AHWD)
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE TDCURV (UCRV,VCRV,WCRV,NCRV,IARH,ARHL,ARHW)
C
        DIMENSION UCRV(NCRV),VCRV(NCRV),WCRV(NCRV)
C
C Draw the projection of a curve in 3-space, as defined by the points
C (UCRV(I),VCRV(I),WCRV(I)), for I from 1 to NCRV.
C
C In addition, if IARH is non-zero, draw the projection of one of the
C following objects at one end of the curve (at the beginning of it if
C IARH is negative, at the end of it if IARH is positive).  If IARH has
C absolute value 1, what is used is a simple arrowhead, of length ARHL
C and width ARHW, lying in a plane that is perpendicular to both the
C unit vector having components UDCP, VDCP, and WDCP, as defined in the
C common block TDPOAH, and to the terminal segment of the curve; if
C IARH has absolute value 2 or greater, what is drawn is a cone-shaped
C arrowhead, of length ARHL and width ARHW; the magnitude of IARH is
C of the form NPAC+1000*NPCT, where NPAC is the number of points to be
C used to draw the circular base of the cone and NPCT is the number of
C points on the base to be connected to the tip.  NPAC is forced to be
C 32 or greater and NPCT is forced to be NPAC/4 or greater; therefore,
C if you use IARH=2, you'll get NPAC=32 and NPCT=8.
C
C UDCP, VDCP, and WDCP are the direction cosines for the normal to the
C plane in which an arrowhead is to be drawn when ABS(IARH) = 1.
C
        COMMON /TDPOAH/ UDCP,VDCP,WDCP
        SAVE   /TDPOAH/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDCURV - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Project the points of the curve, convert to the fractional coordinate
C system, and draw the curve using calls to the SPPS routine PLOTIF.
C
        DO 101 I=1,NCRV
          CALL TDPRPT (UCRV(I),VCRV(I),WCRV(I),XPOS,YPOS)
          CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),MIN(I-1,1))
  101   CONTINUE
C
C Flush the point-draw buffers.
C
        CALL PLOTIF (0.,0.,2)
C
C If an arrowhead is to be drawn at the end of the curve, do that.
C
        IF (IARH.NE.0.AND.NCRV.GE.2) THEN
          IF (IARH.LT.0) THEN
            UEND=UCRV(1)
            VEND=VCRV(1)
            WEND=WCRV(1)
            UBEG=UCRV(2)
            VBEG=VCRV(2)
            WBEG=WCRV(2)
          ELSE
            UEND=UCRV(NCRV)
            VEND=VCRV(NCRV)
            WEND=WCRV(NCRV)
            UBEG=UCRV(NCRV-1)
            VBEG=VCRV(NCRV-1)
            WBEG=WCRV(NCRV-1)
          END IF
          DNOM=SQRT((UEND-UBEG)**2+(VEND-VBEG)**2+(WEND-WBEG)**2)
          IF (DNOM.NE.0.) THEN
            UDCC=(UEND-UBEG)/DNOM
            VDCC=(VEND-VBEG)/DNOM
            WDCC=(WEND-WBEG)/DNOM
            UACC=UEND-ARHL*UDCC
            VACC=VEND-ARHL*VDCC
            WACC=WEND-ARHL*WDCC
            IF (ABS(IARH).EQ.1) THEN
              CALL TDPRPT (UEND,VEND,WEND,XEND,YEND)
              UDC3=VDCC*WDCP-WDCC*VDCP
              VDC3=WDCC*UDCP-UDCC*WDCP
              WDC3=UDCC*VDCP-VDCC*UDCP
              CALL TDPRPT (UACC-(ARHW/2.)*UDC3,
     +                     VACC-(ARHW/2.)*VDC3,
     +                     WACC-(ARHW/2.)*WDC3,XPOS,YPOS)
              CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),0)
              CALL PLOTIF (CUFX(XEND),CUFY(YEND),1)
              CALL TDPRPT (UACC+(ARHW/2.)*UDC3,
     +                     VACC+(ARHW/2.)*VDC3,
     +                     WACC+(ARHW/2.)*WDC3,XPOS,YPOS)
              CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),1)
            ELSE
              NPAC=MAX(32,MOD(ABS(IARH),1000))
              NPCT=MAX(NPAC/4,ABS(IARH)/1000)
              CALL TDCONE (UACC,VACC,WACC,UDCC,VDCC,WDCC,ARHW/2.,NPAC,
     +                                              NPCT,UEND,VEND,WEND)
            END IF
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE TDCUDP (UCRV,VCRV,WCRV,NCRV,IARH,ARHL,ARHW)
C
        DIMENSION UCRV(NCRV),VCRV(NCRV),WCRV(NCRV)
C
C Draw the projection of a curve in 3-space, as defined by the points
C (UCRV(I),VCRV(I),WCRV(I)), for I from 1 to NCRV.  This routine is just
C like TDCURV, except that it calls the DASHPACK routine DPDRAW instead
C of the SPPS routine PLOTIF and can therefore be made to draw a dashed
C line.
C
C In addition, if IARH is non-zero, draw the projection of one of the
C following objects at one end of the curve (at the beginning of it if
C IARH is negative, at the end of it if IARH is positive).  If IARH has
C absolute value 1, what is used is a simple arrowhead, of length ARHL
C and width ARHW, lying in a plane that is perpendicular to both the
C unit vector having components UDCP, VDCP, and WDCP, as defined in the
C common block TDPOAH, and to the terminal segment of the curve; if
C IARH has absolute value 2 or greater, what is drawn is a cone-shaped
C arrowhead, of length ARHL and width ARHW; the magnitude of IARH is
C of the form NPAC+1000*NPCT, where NPAC is the number of points to be
C used to draw the circular base of the cone and NPCT is the number of
C points on the base to be connected to the tip.  NPAC is forced to be
C 32 or greater and NPCT is forced to be NPAC/4 or greater; therefore,
C if you use IARH=2, you'll get NPAC=32 and NPCT=8.
C
C UDCP, VDCP, and WDCP are the direction cosines for the normal to the
C plane in which an arrowhead is to be drawn when ABS(IARH) = 1.
C
        COMMON /TDPOAH/ UDCP,VDCP,WDCP
        SAVE   /TDPOAH/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('TDCUDP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Project the points of the curve, convert to the fractional coordinate
C system, and draw the curve using calls to the DASHPACK routine DPDRAW.
C
        DO 101 I=1,NCRV
          CALL TDPRPT (UCRV(I),VCRV(I),WCRV(I),XPOS,YPOS)
          CALL DPDRAW (CUFX(XPOS),CUFY(YPOS),MIN(I-1,1))
  101   CONTINUE
C
C Flush the point-draw buffers.
C
        CALL DPDRAW (0.,0.,2)
C
C If an arrowhead is to be drawn at the end of the curve, do that.
C
        IF (IARH.NE.0.AND.NCRV.GE.2) THEN
          IF (IARH.LT.0) THEN
            UEND=UCRV(1)
            VEND=VCRV(1)
            WEND=WCRV(1)
            UBEG=UCRV(2)
            VBEG=VCRV(2)
            WBEG=WCRV(2)
          ELSE
            UEND=UCRV(NCRV)
            VEND=VCRV(NCRV)
            WEND=WCRV(NCRV)
            UBEG=UCRV(NCRV-1)
            VBEG=VCRV(NCRV-1)
            WBEG=WCRV(NCRV-1)
          END IF
          DNOM=SQRT((UEND-UBEG)**2+(VEND-VBEG)**2+(WEND-WBEG)**2)
          IF (DNOM.NE.0.) THEN
            UDCC=(UEND-UBEG)/DNOM
            VDCC=(VEND-VBEG)/DNOM
            WDCC=(WEND-WBEG)/DNOM
            UACC=UEND-ARHL*UDCC
            VACC=VEND-ARHL*VDCC
            WACC=WEND-ARHL*WDCC
            IF (ABS(IARH).EQ.1) THEN
              CALL TDPRPT (UEND,VEND,WEND,XEND,YEND)
              UDC3=VDCC*WDCP-WDCC*VDCP
              VDC3=WDCC*UDCP-UDCC*WDCP
              WDC3=UDCC*VDCP-VDCC*UDCP
              CALL TDPRPT (UACC-(ARHW/2.)*UDC3,
     +                     VACC-(ARHW/2.)*VDC3,
     +                     WACC-(ARHW/2.)*WDC3,XPOS,YPOS)
              CALL DPDRAW (CUFX(XPOS),CUFY(YPOS),0)
              CALL DPDRAW (CUFX(XEND),CUFY(YEND),1)
              CALL TDPRPT (UACC+(ARHW/2.)*UDC3,
     +                     VACC+(ARHW/2.)*VDC3,
     +                     WACC+(ARHW/2.)*WDC3,XPOS,YPOS)
              CALL DPDRAW (CUFX(XPOS),CUFY(YPOS),1)
            ELSE
              NPAC=MAX(32,MOD(ABS(IARH),1000))
              NPCT=MAX(NPAC/4,ABS(IARH)/1000)
              CALL TDCONE (UACC,VACC,WACC,UDCC,VDCC,WDCC,ARHW/2.,NPAC,
     +                                              NPCT,UEND,VEND,WEND)
            END IF
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE TDCONE (UACC,VACC,WACC,UDCS,VDCS,WDCS,RADI,NPAC,
     +                                       NPCT,UTIP,VTIP,WTIP)
C
C Draw the projection of a cone in 3-space.  The circular base of the
C cone is centered at the point (UACC,VACC,WACC), is perpendicular to
C the vector with direction cosines UDCS, VDCS, and WDCS, has a radius
C of RADI, and is drawn using NPAC equispaced points along the circle;
C NPCT points equispaced along the circle are connected to the tip,
C which is at (UTIP,VTIP,WTIP).
C
C (This routine is included here because it has a fix that might not
C have made it into the official NCAR Graphics installed on my system.)
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Below, the method used to construct the cone and to put it in the
C desired position is described in terms of the angles RLAT and RLON.
C We can compute the required functions of RLAT and RLON directly from
C the direction cosines that we are given, so we never actually need to
C compute RLAT and RLON themselves.  Still, it may be helpful to see
C how they would be computed:
C
C       RLAT=RTOD*ASIN(WDCS)
C
C       IF (UDCS.NE.0.OR.VDCS.NE.0.) THEN
C         RLON=RTOD*ATAN2(VDCS,UDCS)
C       ELSE
C         RLON=0.
C       END IF
C
C Draw the circular base of the arrowhead.  Essentially, we form the
C circle in the UV plane, perform two rotations (one about the V axis,
C by the angle 90-RLAT, and one about the W axis, by the angle RLON),
C and then translate the circle to its desired final position.  The
C rotations involve sines and cosines of 90-RLAT and of RLON; the code
C is simplified by computing these from the direction cosines.
C
        DNOM=SQRT(UDCS*UDCS+VDCS*VDCS)
C
        DO 101 I=0,NPAC
          UCR1=RADI*COS(DTOR*REAL(I)*(360./REAL(NPAC)))
          UCR2=UCR1*WDCS
          VCR2=RADI*SIN(DTOR*REAL(I)*(360./REAL(NPAC)))
          IF (DNOM.EQ.0.) THEN
            UCR3=UCR2
            VCR3=VCR2
          ELSE
            UCR3=(UCR2*UDCS-VCR2*VDCS)/DNOM
            VCR3=(VCR2*UDCS+UCR2*VDCS)/DNOM
          END IF
          WCR3=-UCR1*SQRT(1.-WDCS*WDCS)
          CALL TDPRPT (UACC+UCR3,VACC+VCR3,WACC+WCR3,XPOS,YPOS)
          CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),MIN(I,1))
  101   CONTINUE
C
C Connect some points along the base of the circle to the tip, using
C very similar code.
C
        CALL TDPRPT (UTIP,VTIP,WTIP,XTIP,YTIP)
C
        XTIP=CUFX(XTIP)
        YTIP=CUFY(YTIP)
C
        DO 102 I=0,NPCT
          CALL PLOTIF (XTIP,YTIP,0)
          UCR1=RADI*COS(DTOR*REAL(I)*(360./REAL(NPCT)))
          UCR2=UCR1*WDCS
          VCR2=RADI*SIN(DTOR*REAL(I)*(360./REAL(NPCT)))
          IF (DNOM.EQ.0.) THEN
            UCR3=UCR2
            VCR3=VCR2
          ELSE
            UCR3=(UCR2*UDCS-VCR2*VDCS)/DNOM
            VCR3=(VCR2*UDCS+UCR2*VDCS)/DNOM
          END IF
          WCR3=-UCR1*SQRT(1.-WDCS*WDCS)
          CALL TDPRPT (UACC+UCR3,VACC+VCR3,WACC+WCR3,XPOS,YPOS)
          CALL PLOTIF (CUFX(XPOS),CUFY(YPOS),1)
  102   CONTINUE
C
C Flush the point-draw buffers.
C
        CALL PLOTIF (0.,0.,2)
C
C Done.
C
        RETURN
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
