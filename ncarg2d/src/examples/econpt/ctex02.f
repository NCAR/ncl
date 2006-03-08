

      PROGRAM CTEX02
C
C
C D E S C R I P T I O N ------------------------------------------------
C
C
C This example illustrates the use of CONPACKT to draw contours on a
C planar triangular mesh that has been created in such a way as to fill
C an area having an irregular boundary, including some holes.  The area
C chosen is, by default, the conterminous 48 states of the US, as seen
C on an orthographic projection of the globe, but the program could
C easily be modified to work for an area defined differently.
C
C The NCAR Graphics package EZMAP is initialized to define the desired
C view of the globe and the package AREAS is used to produce an area
C map representing that view.  The area map resides in a labelled COMMON
C block shared with a function, ISPOTM(X,Y), which can be called to
C determine whether or not a point (X,Y), in the user coordinate system,
C is in the part of the map representing the United States.
C
C A mesh is created by tiling the GKS window with equilateral triangles
C having sides of length SIDE (expressed as a fraction of the width of
C the window), omitting all triangles that fall entirely outside the
C United States, and modifying those that lie partly inside and partly
C outside it.  Meshes are created for several different values of SIDE,
C as determined by values in a PARAMETER statement below.  At each
C resolution, three pictures are drawn, showing: 1) the mesh itself;
C 2) simple contours; and 3) colored contour bands.
C
C
C D E C L A R A T I O N S ----------------------------------------------
C
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
C Define parameters specifying values of SIDE for which meshes will be
C created: we will use SIDE = SFST, SFST/SDIV, (SFST/SDIV)/SDIV, and so
C on, as long as SIDE is greater than or equal to SLST.  Note: if the
C final value of SIDE is made less than .0025, it will be necessary
C to increase the values of NOPN and LAMB, below.
C
        PARAMETER (SFST=.0400,SDIV=4.,SLST=.0024)
C
C To represent the triangular mesh, we use three singly-dimensioned
C arrays: RPNT holds information about points; IEDG, information about
C edges; and ITRI, information about triangles.  The elements of each
C array form "nodes" having lengths as follows:
C
        PARAMETER (LOPN=4)
        PARAMETER (LOEN=5)
        PARAMETER (LOTN=4)
C
C The four elements of a point node, in RPNT, are
C
C   1. the X coordinate of the point;
C   2. the Y coordinate of the point;
C   3. the Z coordinate of the point (always 0 in this example);
C   4. the field value at the point.
C
C The five elements of an edge node, in IEDG, are
C
C   1. the "base index", in RPNT, of point 1 of the edge;
C   2. the "base index", in RPNT, of point 2 of the edge;
C   3. the index, in ITRI, of the pointer to the edge in the node for
C      the triangle to its left (-1 if there is no such triangle);
C   4. the index, in ITRI, of the pointer to the edge in the node for
C      the triangle to its right (-1 if there is no such triangle);
C   5. a utility flag for use by algorithms that scan the structure.
C
C (The "left" and "right" sides of an edge are defined as they would be
C by an observer standing at point 1 of the edge, looking toward point
C 2 of the edge.  It is possible, if there are "holes" in the mesh, that
C there will be no triangle to the left or to the right of an edge, but
C there must be a triangle to one side or the other.)
C
C The four elements of a triangle node, in ITRI, are
C
C   1. the "base index", in IEDG, of edge 1 of the triangle;
C   2. the "base index", in IEDG, of edge 2 of the triangle;
C   3. the "base index", in IEDG, of edge 3 of the triangle;
C   4. a flag set non-zero to block use of the triangle, effectively
C      removing it from the mesh.
C
C The edges pointed to by a triangle node must be in counterclockwise
C order, as viewed from above the mesh.
C
C The "base index" of a point node, an edge node, or a triangle node is
C a non-negative multiple of the length of the node, to which can be
C added an offset to get the index of a particular element of the node.
C Suppose I is the base index of a triangle node in ITRI; then ITRI(I+1)
C is the triangle node's first element, whose value is the base index of
C the node in IEDG that describes the first edge of the triangle.  So
C IEDG(ITRI(I+1)+2) is the second element of that edge node (whose value
C is the base index of the node in RPNT describing the second point of
C the first edge of the triangle with base index I).  Similarly, then,
C we see that RPNT(IEDG(ITRI(I+1)+2)+3) is the third (Z) coordinate of
C the second point of the first edge of the triangle with base index I.
C
C The pointers from the edge nodes back to the triangle nodes (elements
C 3 and 4 of each edge node) allow CONPACKT to navigate the mesh, moving
C from triangle to triangle as it follows a contour line; the pointers
C are tricky to define: if IPTE is the base index of an edge node and
C IEDG(IPTE+3) is zero or more, saying that there is a triangle to the
C left of the edge, then IEDG(IPTE+3) is the actual index of the element
C of the triangle node that points to the edge node; that is to say,
C ITRI(IEDG(IPTE+3))=IPTE.  The base index of the triangle node is IPTL,
C where IPTL is the largest multiple of LOTN less than IEDG(IPTE+3), as
C defined by the formula IPTL = LOTN*((IEDG(IPTE+3)-1)/LOTN), and the
C index of the pointer to the edge within the triangle node is IPIL =
C IEDG(IPTE+3)-IPTL (which has the value 1, 2, or 3); observe that
C IEDG(IPTL+IPIL)=IPTE.  Similar comments apply to element 4 of an edge
C node: if zero or more, it points into the triangle node defining the
C triangle to the right of the edge: ITRI(IEDG(IPTE+4))=IPTE; the base
C index of the node for the triangle to the right of the edge is IPTR=
C LOTN*((IEDG(IPTE+4)-1)/LOTN); the index of the pointer to the edge in
C the triangle node is IPIR=IEDG(IPTE+4)-IPTR; and IEDG(IPTR+IPIR)=IPTE.
C
C Define the number of point nodes, edge nodes, and triangle nodes that
C the triangular mesh arrays will be able to hold.  We make use of the
C fact that, in a tiling of the plane with equilateral triangles, there
C are three times as many edges and two times as many triangles as
C there are vertices.
C
        PARAMETER (NOPN=100000)
        PARAMETER (NOEN=3*NOPN)
        PARAMETER (NOTN=2*NOPN)
C
        PARAMETER (MPPP=NOPN,MPPE=NOEN)
C
C Compute the space required in the arrays (in each case, the number of
C nodes times the length of a node).
C
        PARAMETER (MPNT=NOPN*LOPN)
        PARAMETER (MEDG=NOEN*LOEN)
        PARAMETER (MTRI=NOTN*LOTN)
C
C Declare the arrays to hold the point nodes, edge nodes, and triangle
C nodes defining the triangular mesh.
C
        DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C Declare a triangle buffer in which to store triangles temporarily.
C This buffer makes it possible to randomize the order in which the
C triangles of the mesh are processed.  This is necessary to ensure
C friendly behavior by the tree sorts that are done to identify points
C and edges that more than one triangle have in common.  The size of
C the buffer (MBUF) and the number of triangles processed from it at
c one time when the buffer is full (KBUF) are dependent on the size of
C the user's triangular mesh.
C
        PARAMETER (MBUF=NOTN/10,KBUF=MBUF/25)
C
        DIMENSION TBUF(12,MBUF)
C
C Declare a scratch array to be used to sort points.
C
        DIMENSION IPPP(3,MPPP)
C
C Declare a scratch array to be used to sort edges.
C
        DIMENSION IPPE(2,MPPE)
C
C Declare a character variable into which to encode a label.
C
        CHARACTER*14 LABL
C
C Declare real and integer workspaces needed by CONPACKT.
C
        PARAMETER (LRWK=10000,LIWK=1000)
C
        DIMENSION RWRK(LRWK),IWRK(LIWK)
C
C Declare the area map array needed to determine whether a point is in
C the area to be covered by the triangular mesh or not, putting it in
C COMMON so that ISPOTM can get at it.
C
        PARAMETER (LAMA=200000)
C
        COMMON /CTEXCO/ IAMA(LAMA)
C
C Declare the area map array needed to do solid fill of contour bands.
C
        PARAMETER (LAMB=200000)
C
        DIMENSION IAMB(LAMB)
C
C Declare workspace arrays to be used in calls to the AREAS routine
C ARSCAM.
C
        PARAMETER (NCRB=LAMB/10,NGPS=2)
C
        DIMENSION XCRB(NCRB),YCRB(NCRB),IAAI(NGPS),IAGI(NGPS)
C
C Declare external the routine that does color fill of contour bands.
C
        EXTERNAL DCFOCB
C
C Define a parameter that says whether or not to draw and label the mesh
C on a separate frame.  IDTM=1 => draw mesh only, IDTM=2 => add lines
C connecting triangle centers, IDTM=3 => add numeric labels for points,
C edges, and triangles.  The lines connecting triangle centers and the
C labels are suppressed for small values of SIDE, so if you set IDTM to
C 2 or 3 and don't get what you want, you may have to change one or more
C tests below.
C
        DATA IDTM / 3 /
C
C Define a parameter that says whether or not to draw simple contours.
C ICON=1 => draw contour plot only, IDTM=2 => draw entire mesh, with
C contours on top of it.
C
        DATA ICON / 2 /
C
C Define a parameter that says whether or not to draw a color-filled
C contour plot.
C
        DATA ICOL / 1 /
C
C Define the sizes of point labels, edge labels, and triangle labels.
C
        DATA PLSZ,ELSZ,TLSZ / .0010,.0006,.0008 /
C
C
C A R I T H M E T I C   S T A T E M E N T   F U N C T I O N S ----------
C
C
C The function Z(X,Y) defines the field of data to be contoured on the
C mesh; the first line defines it as 10 times the square of the distance
C from the origin (a paraboloid); the second, a "monkey saddle"; and the
C third, a somewhat more interesting function with a high on one side
C and a low on the other.  Of course, one could supply realistic data
C representing some physical quantity of interest.
C
C       Z(X,Y)=10.*(X*X+Y*Y)
C       Z(X,Y)=10.*(X*X-Y*Y)
        Z(X,Y)=X+Y+1./((X-.07)**2+Y**2+.09)-1./((X+.07)**2+Y**2+.09)
C
C
C O P E N   A N D   I N I T I A L I Z E   G K S ------------------------
C
C
        PRINT * , ' '
        PRINT * , 'OPENING AND INITIALIZING GKS'
C
C Open GKS.
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
C 7 = blue; 8 = a yellow for unblocked portions of the mesh; 9 = 
C a yellow for blocked portions of the mesh; and 10 = light magenta).
C
        CALL GSCR   (IWID, 0,1.,1.,1.)
        CALL GSCR   (IWID, 1,0.,0.,0.)
        CALL GSCR   (IWID, 2,1.,1.,0.)
        CALL GSCR   (IWID, 3,1.,0.,1.)
        CALL GSCR   (IWID, 4,1.,0.,0.)
        CALL GSCR   (IWID, 5,0.,1.,1.)
        CALL GSCR   (IWID, 6,0.,1.,0.)
        CALL GSCR   (IWID, 7,0.,0.,1.)
        CALL GSCR   (IWID, 8,.8,.8,0.)
        CALL GSCR   (IWID, 9,1.,1.,0.)
        CALL GSCR   (IWID,10,1.,.8,1.)
C
C Define 100 colors, associated with color indices 151 through 250, to
C be used for color-filled contour bands and in cell arrays, ranging
C from blue to red.
C
        CALL DFCLRS (IWID,151,250,0.,0.,1.,1.,0.,0.)
C
C
C I N I T I A L I Z E   E Z M A P --------------------------------------
C
C
        PRINT * , 'INITIALIZING EZMAP'
C
C Tell EZMAP what projection to use (orthographic centered over the US).
C
        CALL MAPROJ ('OR',40.,-97.,0.,0.,0.)
C
C Tell EZMAP what portion of the projection plane to use, using angular
C limits.
C
        CALL MAPSET ('AN',23.,23.,23.,23.)
C
C Tell EZMAP where to put the map on the plotter frame (providing a bit
C more room for labels at the top and the bottom of the frame.)
C
        CALL MAPPOS (.08,.92,.08,.92)
C
C Tell EZMAP not to put vertical strips into an area map (so that there
C will only be one edge group in the map).
C
        CALL MPSETI ('VS',0)
C
C Tell PLOTCHAR to use one of the filled fonts and to outline each
C character.
C
        CALL PCSETI ('FN',25)
        CALL PCSETI ('OF',1)
C
C Initialize EZMAP.
C
        CALL MAPINT
C
C Draw the lat/lon grid, label it, and draw geopolitical outlines on it.
C
        CALL MAPGRD
        CALL MAPLBL
        CALL MPLNDR ('Earth..2',3)
C
C Label the frame.
C
        CALL PLCHHQ (CFUX(.5),CFUY(.960),'Simple Map Of Area In Which Tr
     +iangular Mesh Is To Fit',.018,0.,0.)
C
C Advance the frame.
C
        CALL FRAME
C
C Create an area map, representing the map, for use by ISPOTM and report
C its length.
C
        CALL ARINAM (IAMA,LAMA)
        CALL MDLNAM ('Earth..2',3,IAMA)
        CALL ARPRAM (IAMA,0,0,0)
C
        PRINT * , 'LENGTH OF AREA MAP A:',IAMA(1)-(IAMA(6)-IAMA(5)-1)
C
C
C L O O P   O N   D I F F E R E N T   S I D E   L E N G T H S ----------
C
C
C Initialize the value of SIDE.
C
        SIDE=SFST
C
C Control comes here with the initial value of SIDE and each new value.
C Set a character-size multiplier.
C
  101   CSMU=SIDE/.025
C
C
C C R E A T E   A   T R I A N G U L A R   M E S H ----------------------
C
C
C To create the initial triangular mesh, we use a routine, described
C in the programmer document for CONPACKT, called CTTMTX (ConpackT,
C Triangular Mesh from a Triangle list, eXtended).
C
        PRINT * , ' '
        PRINT * , 'CREATING TRIANGULAR MESH FOR SIDE LENGTH ',SIDE
C
C Encode a label to use on each frame, expressing the value of SIDE.
C
        WRITE (LABL,'(''SIDE = '',F7.5)') SIDE
C
C Retrieve the parameters used in the SET call done by EZMAP.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C Compute the length of an equilateral triangle's side in the user
C system, half that length, and the length of an altitude.
C
        SLEN=(XWDR-XWDL)*SIDE
        PRINT * , 'SLEN = ',SLEN
        HLEN=SLEN/2.
        ALEN=1.7320508075689*SLEN/2.
C
C Compute an "epsilon" and its square, to be used by the routine PRANET.
C ELEN should be in the open interval (0,SLEN/2); it affects the way
C triangles at the edge of the area to be covered by the mesh will be
C clipped.  It's a bit difficult to describe what this parameter does;
C see the code of PRANET and/or experiment (if you must).
C
        ELEN=SLEN/10.
        ESQR=ELEN*ELEN
C
C Initialize various counters.  NPPP is the number of points in the
C point sorter, NPPE is the number of edges in the edge sorter, NBUF
C is the number of triangles in the triangle randomizing buffer, and
C NTRI is the number of triangles in the final triangle list.
C
        NPPP=0
        NPPE=0
        NBUF=0
        NTRI=0
C
C Loop to generate the triangles of the mesh.  The variables J and I
C specify the vertical and horizontal positions, respectively, of a
C position in the viewport at which are to be put two equilateral
C triangles.  Each triangle is delivered to the subroutine PRANET for
C processing: PRANET may omit the triangle if it is entirely outside
C the desired area, include it as is if it is entirely inside the
C desired area, or modify it to make it fit inside the desired area
C (which may require that it be broken into two pieces) if it is
C partly inside and partly outside the desired area.
C
        DO 103 J=INT(YWDB/ALEN)-2,INT(YWDT/ALEN)+2
C
          DO 102 I=INT(XWDL/SLEN)-2,INT(XWDR/SLEN)+2
C
C If the triangle buffer doesn't have room in it for at least four more
C triangles, process KBUF randomly-chosen triangles from it, leaving the
C remainder at the beginning of the buffer.
C
            IF (NBUF+4.GT.MBUF) THEN
              CALL CTTMTX (KBUF,
     +                     TBUF,MBUF,NBUF,
     +                     ELEN/10.,
     +                     IPPP,MPPP,NPPP,
     +                     IPPE,MPPE,NPPE,
     +                     RPNT,MPNT,NPNT,LOPN,
     +                     IEDG,MEDG,NEDG,LOEN,
     +                     ITRI,MTRI,NTRI,LOTN)
            END IF
C
C At each step, we generate two equilateral triangles, making sure to
C generate the X and Y coordinates of the vertices in such a way that
C coincident points in different triangles will be recognized as such,
C and then call the subroutine PRANET to process each one.
C
            IF (MOD(10000+J,2).EQ.0) THEN
C
              NBUF=NBUF+1
C
              TBUF( 1,NBUF)=REAL(I  )*SLEN
              TBUF( 2,NBUF)=REAL(J  )*ALEN
C
              TBUF( 5,NBUF)=REAL(I  )*SLEN+HLEN
              TBUF( 6,NBUF)=REAL(J+1)*ALEN
C
              TBUF( 9,NBUF)=REAL(I-1)*SLEN+HLEN
              TBUF(10,NBUF)=REAL(J+1)*ALEN
C
              CALL PRANET (TBUF,NBUF,ESQR)
C
              NBUF=NBUF+1
C
              TBUF( 1,NBUF)=REAL(I  )*SLEN
              TBUF( 2,NBUF)=REAL(J  )*ALEN
C
              TBUF( 5,NBUF)=REAL(I+1)*SLEN
              TBUF( 6,NBUF)=REAL(J  )*ALEN
C
              TBUF( 9,NBUF)=REAL(I  )*SLEN+HLEN
              TBUF(10,NBUF)=REAL(J+1)*ALEN
C
              CALL PRANET (TBUF,NBUF,ESQR)
C
            ELSE
C
              NBUF=NBUF+1
C
              TBUF( 1,NBUF)=REAL(I  )*SLEN+HLEN
              TBUF( 2,NBUF)=REAL(J  )*ALEN
C
              TBUF( 5,NBUF)=REAL(I+1)*SLEN
              TBUF( 6,NBUF)=REAL(J+1)*ALEN
C
              TBUF( 9,NBUF)=REAL(I  )*SLEN
              TBUF(10,NBUF)=REAL(J+1)*ALEN
C
              CALL PRANET (TBUF,NBUF,ESQR)
C
              NBUF=NBUF+1
C
              TBUF( 1,NBUF)=REAL(I  )*SLEN+HLEN
              TBUF( 2,NBUF)=REAL(J  )*ALEN
C
              TBUF( 5,NBUF)=REAL(I+1)*SLEN+HLEN
              TBUF( 6,NBUF)=REAL(J  )*ALEN
C
              TBUF( 9,NBUF)=REAL(I+1)*SLEN
              TBUF(10,NBUF)=REAL(J+1)*ALEN
C
              CALL PRANET (TBUF,NBUF,ESQR)
C
            END IF
C
  102     CONTINUE
C
  103   CONTINUE
C
C Process any triangles that remain in the triangle buffer.
C
        IF (NBUF.NE.0) THEN
          CALL CTTMTX (NBUF,
     +                 TBUF,MBUF,NBUF,
     +                 ELEN/10.,
     +                 IPPP,MPPP,NPPP,
     +                 IPPE,MPPE,NPPE,
     +                 RPNT,MPNT,NPNT,LOPN,
     +                 IEDG,MEDG,NEDG,LOEN,
     +                 ITRI,MTRI,NTRI,LOTN)
        END IF
C
C Print the number of points, edges, and triangles.
C
        PRINT * , 'NUMBER OF POINTS:    ',NPNT/LOPN
        PRINT * , 'NUMBER OF EDGES:     ',NEDG/LOEN
        PRINT * , 'NUMBER OF TRIANGLES: ',NTRI/LOTN
C
C
C S U P P L Y   D A T A   V A L U E S   O N   T H E   M E S H ----------
C
C
        PRINT * , 'COMPUTING DATA VALUES ON THE MESH'
C
        DO 104 I=0,NPNT-LOPN,LOPN
          RPNT(I+3)=0.
          RPNT(I+4)=Z(RPNT(I+1),RPNT(I+2))
  104   CONTINUE
C
C
C D R A W   A   P I C T U R E   O F   T H E   M E S H ------------------
C
C
        IF (IDTM.GE.1) THEN
C
          PRINT * , 'DRAWING A PICTURE OF THE MESH'
C
C Draw the lat/lon grid, label it, and draw geopolitical outlines on it.
C
          CALL MAPGRD
          CALL MAPLBL
          CALL MPLNDR ('Earth..2',3)
C
C Label the frame.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.975),'Area Filled By Simple Triang
     +ular Mesh',.018,0.,0.)
C
          CALL PLCHHQ (CFUX(.9),CFUY(.11),LABL,.014,0.,1.)
C
          CALL PLCHHQ (CFUX(.5),CFUY(.946), 'The mesh is in yellow, in a
     + lighter shade where triangles are blocked.',.011,0.,0.)
C
C Draw a bunch of I's and O's to see what ISPOTM is doing.  (This is
C debug stuff, but it might prove useful to a user modifying the code.)
C
C         DO 199 I=1,101
C         DO 199 J=1,101
C           X=CFUX(REAL(I-1)/100.)
C           Y=CFUY(REAL(J-1)/100.)
C           IF (ISPOTM(X,Y).EQ.0) THEN
C             CALL PLCHMQ (X,Y,'O',.005,0.,0.)
C           ELSE
C             CALL PLCHMQ (X,Y,'I',.005,0.,0.)
C           END IF
C 199     CONTINUE
C
C Draw the mesh, using two shades of yellow to distinguish blocked
C portions of it from unblocked portions.  (Actually, in this program,
C there will be no blocked portions.)
C
          CALL DRWMSH (RPNT,NPNT,LOPN,
     +                 IEDG,NEDG,LOEN,
     +                 ITRI,NTRI,LOTN,
     +                 8,9)
C
C Connect the centers of adjacent triangles, in light magenta.
C
          IF (IDTM.GE.2.AND.SIDE.GT..01) THEN
C
C Add some labels at the bottom of the frame.
C
            CALL PLCHHQ (CFUX(.5),CFUY(.059),'Magenta lines connect cent
     +ers of adjacent triangles.',.011,0.,0.)
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (10)
            CALL GSLWSC (1.)
C
            DO 105 IPTE=0,NEDG-LOEN,LOEN
              IF (IEDG(IPTE+3).GE.0.AND.IEDG(IPTE+4).GE.0) THEN
                IPTL=LOTN*((IEDG(IPTE+3)-1)/LOTN)
                IPTR=LOTN*((IEDG(IPTE+4)-1)/LOTN)
                XCT1=(RPNT(IEDG(ITRI(IPTL+1)+1)+1)+
     +                RPNT(IEDG(ITRI(IPTL+1)+2)+1)+
     +                RPNT(IEDG(ITRI(IPTL+2)+1)+1)+
     +                RPNT(IEDG(ITRI(IPTL+2)+2)+1)+
     +                RPNT(IEDG(ITRI(IPTL+3)+1)+1)+
     +                RPNT(IEDG(ITRI(IPTL+3)+2)+1))/6.
                YCT1=(RPNT(IEDG(ITRI(IPTL+1)+1)+2)+
     +                RPNT(IEDG(ITRI(IPTL+1)+2)+2)+
     +                RPNT(IEDG(ITRI(IPTL+2)+1)+2)+
     +                RPNT(IEDG(ITRI(IPTL+2)+2)+2)+
     +                RPNT(IEDG(ITRI(IPTL+3)+1)+2)+
     +                RPNT(IEDG(ITRI(IPTL+3)+2)+2))/6.
                XCT2=(RPNT(IEDG(ITRI(IPTR+1)+1)+1)+
     +                RPNT(IEDG(ITRI(IPTR+1)+2)+1)+
     +                RPNT(IEDG(ITRI(IPTR+2)+1)+1)+
     +                RPNT(IEDG(ITRI(IPTR+2)+2)+1)+
     +                RPNT(IEDG(ITRI(IPTR+3)+1)+1)+
     +                RPNT(IEDG(ITRI(IPTR+3)+2)+1))/6.
                YCT2=(RPNT(IEDG(ITRI(IPTR+1)+1)+2)+
     +                RPNT(IEDG(ITRI(IPTR+1)+2)+2)+
     +                RPNT(IEDG(ITRI(IPTR+2)+1)+2)+
     +                RPNT(IEDG(ITRI(IPTR+2)+2)+2)+
     +                RPNT(IEDG(ITRI(IPTR+3)+1)+2)+
     +                RPNT(IEDG(ITRI(IPTR+3)+2)+2))/6.
                CALL LINE (XCT1,YCT1,XCT2,YCT2)
              END IF
  105       CONTINUE
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (1)
            CALL GSLWSC (1.)
C
          END IF
C
C Label the points, edges, and triangles of the mesh (if the side length
C is large enough).
C
          IF (IDTM.GE.3.AND.SIDE.GT..005) THEN
C
C Add some labels at the bottom of the frame.
C
            CALL PLCHHQ (CFUX(.5),CFUY(.037),'Point numbers are in blue;
     + edge numbers, in green; triangle numbers, in red.',.011,0.,0.)
C
            CALL PLCHHQ (CFUX(.5),CFUY(.015),'Use "idt" to zoom in and s
     +ee the numbers.',.011,0.,0.)
C
C Label the points of the mesh in blue.
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (7)
            CALL GSLWSC (1.)
C
            DO 106 IPTP=0,NPNT-LOPN,LOPN
              CALL LABLIT (IPTP/LOPN+1,CSMU*PLSZ,0.,
     +                     RPNT(IPTP+1),RPNT(IPTP+2))
  106       CONTINUE
C
C Label the edges of the mesh in green.
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (6)
            CALL GSLWSC (1.)
C
            DO 107 IPTE=0,NEDG-LOEN,LOEN
              CALL LABLIT (IPTE/LOEN+1,CSMU*ELSZ,0.,
     +                   .5*(RPNT(IEDG(IPTE+1)+1)+RPNT(IEDG(IPTE+2)+1)),
     +                   .5*(RPNT(IEDG(IPTE+1)+2)+RPNT(IEDG(IPTE+2)+2)))
  107       CONTINUE
C
C Label the triangles of the mesh in red.
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (4)
            CALL GSLWSC (1.)
C
            DO 108 IPTT=0,NTRI-LOTN,LOTN
              CALL LABLIT (IPTT/LOTN+1,CSMU*TLSZ,0.,
     +                     (RPNT(IEDG(ITRI(IPTT+1)+1)+1)+
     +                      RPNT(IEDG(ITRI(IPTT+1)+2)+1)+
     +                      RPNT(IEDG(ITRI(IPTT+2)+1)+1)+
     +                      RPNT(IEDG(ITRI(IPTT+2)+2)+1)+
     +                      RPNT(IEDG(ITRI(IPTT+3)+1)+1)+
     +                      RPNT(IEDG(ITRI(IPTT+3)+2)+1))/6.,
     +                     (RPNT(IEDG(ITRI(IPTT+1)+1)+2)+
     +                      RPNT(IEDG(ITRI(IPTT+1)+2)+2)+
     +                      RPNT(IEDG(ITRI(IPTT+2)+1)+2)+
     +                      RPNT(IEDG(ITRI(IPTT+2)+2)+2)+
     +                      RPNT(IEDG(ITRI(IPTT+3)+1)+2)+
     +                      RPNT(IEDG(ITRI(IPTT+3)+2)+2))/6.)
  108       CONTINUE
C
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (1)
            CALL GSLWSC (1.)
C
          END IF
C
C Advance the frame.
C
          CALL FRAME
C
        END IF
C
C
C D R A W   A   S I M P L E   C O N T O U R   P L O T ------------------
C
C
        IF (ICON.NE.0) THEN
C
          PRINT * , 'DRAWING A SIMPLE CONTOUR PLOT'
C
C Label the frame.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.960), 'Contours on Triangular Mesh
     +',.018,0.,0.)
C
          CALL PLCHHQ (CFUX(.9),CFUY(.11),LABL,.014,0.,1.)
C
C Draw the lat/lon grid, label it, and draw geopolitical outlines on it.
C
          CALL MAPGRD
          CALL MAPLBL
          CALL MPLNDR ('Earth..2',3)
C
C Tell CONPACKT not to do a SET call, since it has already been done.
C
          CALL CTSETI ('SET - DO-SET-CALL FLAG',0)
C
C Tell CONPACKT to draw the mesh's outer boundary in red.
C
          CALL CTSETI ('PAI - PARAMETER ARRAY INDEX',-1)
          CALL CTSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
          CALL CTSETI ('CLC - CONTOUR LEVEL COLOR INDEX',4)
C
C Tell CONPACKT to use bigger labels on contours drawn by the dash
C package and to put them a little closer together.
C
          CALL CTSETR ('DPS - DASH PATTERN SIZE (CHARACTERS)',.015)
          CALL CTSETR ('DPV - DASH PATTERN VECTOR LENGTH',.0025)
C
C Initialize CONPACKT.
C
          CALL CTMESH (RPNT,NPNT,LOPN,
     +                 IEDG,NEDG,LOEN,
     +                 ITRI,NTRI,LOTN,
     +                 RWRK,LRWK,
     +                 IWRK,LIWK)
C
C Draw the mesh in two shades of yellow, to distinguish blocked portions
C of it from unblocked portions.
C
          IF (ICON.GE.2) THEN
            CALL DRWMSH (RPNT,NPNT,LOPN,
     +                   IEDG,NEDG,LOEN,
     +                   ITRI,NTRI,LOTN,
     +                   8,9)
          END IF
C
C Draw contour lines with labels written by the dash package.
C
          CALL CTCLDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
C Add informational and high/low labels.
C
          CALL CTLBDR (RPNT,IEDG,ITRI,RWRK,IWRK)
C
C Advance the frame.
C
          CALL FRAME
C
        END IF
C
C
C D R A W   A   C O L O R - F I L L E D   C O N T O U R   P L O T ------
C
C
        IF (ICOL.NE.0) THEN
C
          PRINT * , 'DRAWING A COLOR-FILLED CONTOUR PLOT'
C
C Label the frame.
C
          CALL PLCHHQ (CFUX(.5),CFUY(.960), 'Colored Contour Bands on Tr
     +iangular Mesh',.018,0.,0.)
C
          CALL PLCHHQ (CFUX(.9),CFUY(.11),LABL,.014,0.,1.)
C
C Draw the lat/lon grid, label it, and draw geopolitical outlines on it.
C
          CALL MAPGRD
          CALL MAPLBL
          CALL MPLNDR ('Earth..2',3)
C
C Initialize an area map.
C
          CALL ARINAM (IAMB,LAMB)
C
C Put contour lines in the area map.
C
          CALL CTCLAM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMB)
C
C Scan the area map and deliver the areas defined by it to the routine
C DCFOCB.
C
          CALL ARSCAM (IAMB,XCRB,YCRB,NCRB,IAAI,IAGI,NGPS,DCFOCB)
          PRINT * , 'LENGTH OF AREA MAP B:',IAMB(1)-(IAMB(6)-IAMB(5)-1)
C
C Reset the fill color.
C
          CALL GSFACI (1)
C
C Advance the frame.
C
          CALL FRAME
C
        END IF
C
C Reduce the value of SIDE and, if it's not too small, loop back for
C another series of plots.
C
        SIDE=SIDE/SDIV
C
        IF (SIDE.GE.SLST) GO TO 101
C
C
C C L O S E   G K S   A N D   Q U I T ----------------------------------
C
C
        PRINT * , 'CLOSING GKS AND QUITTING'
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


      SUBROUTINE DRWMSH (RPNT,NPNT,LOPN,
     +                   IEDG,NEDG,LOEN,
     +                   ITRI,NTRI,LOTN,
     +                   ICI1,ICI2)
C
        DIMENSION RPNT(NPNT),IEDG(NEDG),ITRI(NTRI)
C
C Draw the edges of the triangular mesh defined by the arguments.  Use
C color index ICI1 to draw edges in areas that are unblocked and color
C index ICI2 to draw edges in areas that are blocked.
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
            CALL GSPLCI (ICI1)
          ELSE
            CALL PLOTIT (0,0,2)
            CALL GSPLCI (ICI2)
          END IF
C
          CALL LINE   (RPNT(IEDG(IPTE+1)+1),RPNT(IEDG(IPTE+1)+2),
     +                 RPNT(IEDG(IPTE+2)+1),RPNT(IEDG(IPTE+2)+2))
C
  101   CONTINUE
C
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (1)
C
        RETURN
C
      END


      SUBROUTINE LABLIT (INUM,SIZE,ANGR,XPOS,YPOS)
C
        CHARACTER*8 CNUM
C
C Generate a label expressing the value of the integer INUM and place it
C on the frame at the position (XPOS,YPOS) in the current user system,
C using characters of size SIZE (in the fractional coordinate system),
C and an angle of ANGR (in radians).
C
C Conversion factor, radians to degrees.
C
        DATA RTOD / 57.2957795130823 /
C
C Encode the integer into CNUM.
C
        WRITE (CNUM,'(I8)') INUM
C
C Write the label and return.  The loop finds the first non-blank in the
C label so that we can use sub-string notation in the call to PLCHMQ.
C
        DO 101 I=1,8
          IF (CNUM(I:I).NE.' ') THEN
            CALL PLCHMQ (XPOS,YPOS,CNUM(I:8),SIZE,RTOD*ANGR,0.)
            RETURN
          END IF
  101   CONTINUE
C
C If no non-blank character is found, return, having done nothing.
C
        RETURN
C
      END


      SUBROUTINE DCFOCB (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
C This routine fills the area defined by the points (XCRA(I),YCRA(I)),
C for I = 1 to NCRA, if and only if none of the area identifiers for
C the area are negative.  The color used is determined from the area
C identifier of the area relative to group 3; we assume that 100 colors
C are defined having color indices 151 through 250 and interpolate to
C determine which of these colors to use.
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
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


      FUNCTION ISPOTM (X,Y)
C
C The value of the function ISPOTM is non-zero if and only if the point
C (X,Y) is part of the area to be filled by the mesh.  It uses an area
C map created by the caller.
C
        PARAMETER (LAMA=200000)
C
        COMMON /CTEXCO/ IAMA(LAMA)
C
        CALL ARGTAI (IAMA,X,Y,IAAI,IAGI,1,NAIR,1)
C
        ISPOTM=0
C
        IF (NAIR.EQ.1) THEN
          IF (IAGI.EQ.1) THEN
            IF (MDIPAN(IAAI,'Conterminous US').NE.0) THEN
              ISPOTM=1
            END IF
          END IF
        END IF
C
        RETURN
C
      END


      SUBROUTINE PRANET (TBUF,NBUF,ESQR)
C
        DIMENSION TBUF(12,*)
C
C PRANET (PRocess AN Equilateral Triangle) is called once for each of
C the equilateral triangles from a set "tiling" (completely covering)
C the current GKS window.  It checks whether any vertex of the triangle
C lies in the area to be covered by the mesh.  If not, it deletes the
C triangle.  Otherwise, it either leaves the triangle alone (if all of
C the vertices are inside the area to be covered) or it adjusts the
C triangle to better fit into the area to be covered, which may include
C breaking the triangle into two triangles.
C
C The triangle to be examined is as defined by elements of TBUF(I,NBUF)
C for I = 1 and 2 (the X and Y coordinates of point 1), 5 and 6 (the X
C and Y coordinates of point 2), and 9 and 10 (the X and Y coordinates
C of point 3).  Zeroed Z coordinates (I = 3, 7, and 11) and contour
C field data values (I = 4, 8, and 12) are supplied for the vertices of
C triangles left in the list.
C
C ESQR is the square of an "epsilon" ELEN that defines how close to a
C vertex a point must be to be considered coincident with it.  PRANET
C will not generate any triangle with a side of length less than ELEN;
C it may allow a triangle's vertex to lie as much as ELEN outside the
C desired area.  ELEN should be in the open interval (0,SLEN/2), where
C SLEN is the length of the equilateral triangle's side; a recommended
C value for ELEN is SLEN/10.
C
C The function DSQR computes the square of the distance between the two
C points (X1,Y1) and (X2,Y2).
C
        DSQR(X1,Y1,X2,Y2)=(X2-X1)**2+(Y2-Y1)**2
C
C Get in/out flags for the three vertices of the triangle.
C
        INF1=ISPOTM(TBUF( 1,NBUF),TBUF( 2,NBUF))
        INF2=ISPOTM(TBUF( 5,NBUF),TBUF( 6,NBUF))
        INF3=ISPOTM(TBUF( 9,NBUF),TBUF(10,NBUF))
C
C There are eight cases, depending on whether or not each of the three
C points is inside or outside the area to be covered by the mesh.
C
C Case 1: If all points are outside, remove the triangle from the list.
C
        IF      (INF1.EQ.0.AND.INF2.EQ.0.AND.INF3.EQ.0) THEN
C
          NBUF=NBUF-1
          RETURN
C
        END IF
C
C Case 2: If all points are inside, do nothing; just leave the triangle
C in the list.  The following test actually checks for the opposite of
C the case 2 condition (i.e., "some point outside"); if case 2 obtains,
C control drops through the last END IF below.
C
        IF      (INF1.EQ.0.OR.INF2.EQ.0.OR.INF3.EQ.0) THEN
C
C Case 3: If points 1 and 2 are outside and point 3 is inside, move
C points 1 and 2 toward point 3 (but keep them outside).  If a moved
C point would be within epsilon of the inside point, remove the
C triangle; if a moved point would be within epsilon of its original
C position, leave it alone.
C
        IF      (INF1.EQ.0.AND.INF2.EQ.0.AND.INF3.NE.0) THEN
C
          XINT=TBUF( 9,NBUF)
          YINT=TBUF(10,NBUF)
          XEXT=TBUF( 1,NBUF)
          YEXT=TBUF( 2,NBUF)
          CALL MVINEX (XINT,YINT,XEXT,YEXT)
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXT,YEXT).LE.ESQR) THEN
            NBUF=NBUF-1
            RETURN
          END IF
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXT,YEXT).GT.ESQR) THEN
            TBUF( 1,NBUF)=XEXT
            TBUF( 2,NBUF)=YEXT
          END IF
C
          XINT=TBUF( 9,NBUF)
          YINT=TBUF(10,NBUF)
          XEXT=TBUF( 5,NBUF)
          YEXT=TBUF( 6,NBUF)
          CALL MVINEX (XINT,YINT,XEXT,YEXT)
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXT,YEXT).LE.ESQR) THEN
            NBUF=NBUF-1
            RETURN
          END IF
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXT,YEXT).GT.ESQR) THEN
            TBUF( 5,NBUF)=XEXT
            TBUF( 6,NBUF)=YEXT
          END IF
C
C Case 4 (similar to case 3): If points 1 and 3 are outside and point 2
C is inside, move points 1 and 3 toward point 2 (but keep them outside).
C If a moved point would be within epsilon of the inside point, remove
C the triangle; if a moved point would be within epsilon of its original
C position, leave it alone.
C
        ELSE IF (INF1.EQ.0.AND.INF2.NE.0.AND.INF3.EQ.0) THEN
C
          XINT=TBUF( 5,NBUF)
          YINT=TBUF( 6,NBUF)
          XEXT=TBUF( 1,NBUF)
          YEXT=TBUF( 2,NBUF)
          CALL MVINEX (XINT,YINT,XEXT,YEXT)
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXT,YEXT).LE.ESQR) THEN
            NBUF=NBUF-1
            RETURN
          END IF
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXT,YEXT).GT.ESQR) THEN
            TBUF( 1,NBUF)=XEXT
            TBUF( 2,NBUF)=YEXT
          END IF
C
          XINT=TBUF( 5,NBUF)
          YINT=TBUF( 6,NBUF)
          XEXT=TBUF( 9,NBUF)
          YEXT=TBUF(10,NBUF)
          CALL MVINEX (XINT,YINT,XEXT,YEXT)
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXT,YEXT).LE.ESQR) THEN
            NBUF=NBUF-1
            RETURN
          END IF
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXT,YEXT).GT.ESQR) THEN
            TBUF( 9,NBUF)=XEXT
            TBUF(10,NBUF)=YEXT
          END IF
C
C Case 5: If point 1 is outside and points 2 and 3 are inside, find
C points A (between 1 and 2) and B (between 1 and 3) that are almost
C inside and replace the triangle 123 with triangles A23 and A3B or
C B23 and A2B (whichever is better).
C
        ELSE IF (INF1.EQ.0.AND.INF2.NE.0.AND.INF3.NE.0) THEN
C
          XINT=TBUF( 5,NBUF)
          YINT=TBUF( 6,NBUF)
          XEXA=TBUF( 1,NBUF)
          YEXA=TBUF( 2,NBUF)
          CALL MVINEX (XINT,YINT,XEXA,YEXA)
          IFLA=0
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXA,YEXA).LE.ESQR) THEN
            IFLA=1
            XEXA=TBUF( 1,NBUF)
            YEXA=TBUF( 2,NBUF)
          END IF
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXA,YEXA).LE.ESQR) THEN
            IFLA=2
            XEXA=TBUF( 5,NBUF)
            YEXA=TBUF( 6,NBUF)
          END IF
C
          XINT=TBUF( 9,NBUF)
          YINT=TBUF(10,NBUF)
          XEXB=TBUF( 1,NBUF)
          YEXB=TBUF( 2,NBUF)
          CALL MVINEX (XINT,YINT,XEXB,YEXB)
          IFLB=0
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXB,YEXB).LE.ESQR) THEN
            IFLB=1
            XEXB=TBUF( 1,NBUF)
            YEXB=TBUF( 2,NBUF)
          END IF
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXB,YEXB).LE.ESQR) THEN
            IFLB=2
            XEXB=TBUF( 9,NBUF)
            YEXB=TBUF(10,NBUF)
          END IF
C
          IF (IFLA.EQ.2.AND.IFLB.EQ.2) THEN
            NBUF=NBUF-1
            RETURN
          END IF
C
          TBUF( 3,NBUF)=0.
          TBUF( 4,NBUF)=0.
          TBUF( 7,NBUF)=0.
          TBUF( 8,NBUF)=0.
          TBUF(11,NBUF)=0.
          TBUF(12,NBUF)=0.
C
          IF (IFLA.NE.0.AND.IFLB.NE.0) RETURN
C
          DSQA=DSQR(XEXA,YEXA,
     +              .5*(TBUF( 5,NBUF)+TBUF( 1,NBUF)),
     +              .5*(TBUF( 6,NBUF)+TBUF( 2,NBUF)))
          DSQB=DSQR(XEXB,YEXB,
     +              .5*(TBUF( 9,NBUF)+TBUF( 1,NBUF)),
     +              .5*(TBUF(10,NBUF)+TBUF( 2,NBUF)))
          IF (DSQA.LE.DSQB) THEN
            TBUF( 1,NBUF)=XEXA
            TBUF( 2,NBUF)=YEXA
            IF (IFLB.EQ.2) RETURN
            NBUF=NBUF+1
            TBUF( 1,NBUF)=XEXA
            TBUF( 2,NBUF)=YEXA
            TBUF( 5,NBUF)=TBUF( 9,NBUF-1)
            TBUF( 6,NBUF)=TBUF(10,NBUF-1)
            TBUF( 9,NBUF)=XEXB
            TBUF(10,NBUF)=YEXB
          ELSE
            TBUF( 1,NBUF)=XEXB
            TBUF( 2,NBUF)=YEXB
            IF (IFLA.EQ.2) RETURN
            NBUF=NBUF+1
            TBUF( 1,NBUF)=XEXA
            TBUF( 2,NBUF)=YEXA
            TBUF( 5,NBUF)=TBUF( 5,NBUF-1)
            TBUF( 6,NBUF)=TBUF( 6,NBUF-1)
            TBUF( 9,NBUF)=XEXB
            TBUF(10,NBUF)=YEXB
          END IF
C
C Case 6 (similar to case 3): If points 2 and 3 are outside and point 1
C is inside, move points 2 and 3 toward point 1 (but keep them outside).
C If a moved point would be within epsilon of the inside point, remove
C the triangle; if a moved point would be within epsilon of its original
C position, leave it alone.
C
        ELSE IF (INF1.NE.0.AND.INF2.EQ.0.AND.INF3.EQ.0) THEN
C
          XINT=TBUF( 1,NBUF)
          YINT=TBUF( 2,NBUF)
          XEXT=TBUF( 5,NBUF)
          YEXT=TBUF( 6,NBUF)
          CALL MVINEX (XINT,YINT,XEXT,YEXT)
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXT,YEXT).LE.ESQR) THEN
            NBUF=NBUF-1
            RETURN
          END IF
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXT,YEXT).GT.ESQR) THEN
            TBUF( 5,NBUF)=XEXT
            TBUF( 6,NBUF)=YEXT
          END IF
C
          XINT=TBUF( 1,NBUF)
          YINT=TBUF( 2,NBUF)
          XEXT=TBUF( 9,NBUF)
          YEXT=TBUF(10,NBUF)
          CALL MVINEX (XINT,YINT,XEXT,YEXT)
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXT,YEXT).LE.ESQR) THEN
            NBUF=NBUF-1
            RETURN
          END IF
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXT,YEXT).GT.ESQR) THEN
            TBUF( 9,NBUF)=XEXT
            TBUF(10,NBUF)=YEXT
          END IF
C
C Case 7 (similar to case 5): If point 2 is outside and points 3 and 1
C are inside, find points A (between 2 and 3) and B (between 2 and 1)
C that are almost inside and replace the triangle 123 with triangles
C 1A3 and A1B or 1B3 and A3B (whichever is better).
C
        ELSE IF (INF1.NE.0.AND.INF2.EQ.0.AND.INF3.NE.0) THEN
C
          XINT=TBUF( 9,NBUF)
          YINT=TBUF(10,NBUF)
          XEXA=TBUF( 5,NBUF)
          YEXA=TBUF( 6,NBUF)
          CALL MVINEX (XINT,YINT,XEXA,YEXA)
          IFLA=0
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXA,YEXA).LE.ESQR) THEN
            IFLA=1
            XEXA=TBUF( 5,NBUF)
            YEXA=TBUF( 6,NBUF)
          END IF
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXA,YEXA).LE.ESQR) THEN
            IFLA=2
            XEXA=TBUF( 9,NBUF)
            YEXA=TBUF(10,NBUF)
          END IF
C
          XINT=TBUF( 1,NBUF)
          YINT=TBUF( 2,NBUF)
          XEXB=TBUF( 5,NBUF)
          YEXB=TBUF( 6,NBUF)
          CALL MVINEX (XINT,YINT,XEXB,YEXB)
          IFLB=0
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXB,YEXB).LE.ESQR) THEN
            IFLB=1
            XEXB=TBUF( 5,NBUF)
            YEXB=TBUF( 6,NBUF)
          END IF
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXB,YEXB).LE.ESQR) THEN
            IFLB=2
            XEXB=TBUF( 1,NBUF)
            YEXB=TBUF( 2,NBUF)
          END IF
C
          IF (IFLA.EQ.2.AND.IFLB.EQ.2) THEN
            NBUF=NBUF-1
            RETURN
          END IF
C
          TBUF( 3,NBUF)=0.
          TBUF( 4,NBUF)=0.
          TBUF( 7,NBUF)=0.
          TBUF( 8,NBUF)=0.
          TBUF(11,NBUF)=0.
          TBUF(12,NBUF)=0.
C
          IF (IFLA.NE.0.AND.IFLB.NE.0) RETURN
C
          DSQA=DSQR(XEXA,YEXA,
     +              .5*(TBUF( 9,NBUF)+TBUF( 5,NBUF)),
     +              .5*(TBUF(10,NBUF)+TBUF( 6,NBUF)))
          DSQB=DSQR(XEXB,YEXB,
     +              .5*(TBUF( 1,NBUF)+TBUF( 5,NBUF)),
     +              .5*(TBUF( 2,NBUF)+TBUF( 6,NBUF)))
          IF (DSQA.LE.DSQB) THEN
            TBUF( 5,NBUF)=XEXA
            TBUF( 6,NBUF)=YEXA
            IF (IFLB.EQ.2) RETURN
            NBUF=NBUF+1
            TBUF( 1,NBUF)=XEXA
            TBUF( 2,NBUF)=YEXA
            TBUF( 5,NBUF)=TBUF( 1,NBUF-1)
            TBUF( 6,NBUF)=TBUF( 2,NBUF-1)
            TBUF( 9,NBUF)=XEXB
            TBUF(10,NBUF)=YEXB
          ELSE
            TBUF( 5,NBUF)=XEXB
            TBUF( 6,NBUF)=YEXB
            IF (IFLA.EQ.2) RETURN
            NBUF=NBUF+1
            TBUF( 1,NBUF)=XEXA
            TBUF( 2,NBUF)=YEXA
            TBUF( 5,NBUF)=TBUF( 9,NBUF-1)
            TBUF( 6,NBUF)=TBUF(10,NBUF-1)
            TBUF( 9,NBUF)=XEXB
            TBUF(10,NBUF)=YEXB
          END IF
C
C Case 8 (similar to case 5): If point 3 is outside and points 1 and 2
C are inside, find points A (between 3 and 1) and B (between 3 and 2)
C that are almost inside and replace the triangle 123 with triangles
C 12A and A2B or 12B and A1B (whichever is better).
C
        ELSE
C
C that is, if (INF1.NE.0.AND.INF2.NE.0.AND.INF3.EQ.0) ...
C
          XINT=TBUF( 1,NBUF)
          YINT=TBUF( 2,NBUF)
          XEXA=TBUF( 9,NBUF)
          YEXA=TBUF(10,NBUF)
          CALL MVINEX (XINT,YINT,XEXA,YEXA)
          IFLA=0
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXA,YEXA).LE.ESQR) THEN
            IFLA=1
            XEXA=TBUF( 9,NBUF)
            YEXA=TBUF(10,NBUF)
          END IF
          IF (DSQR(TBUF( 1,NBUF),TBUF( 2,NBUF),XEXA,YEXA).LE.ESQR) THEN
            IFLA=2
            XEXA=TBUF( 1,NBUF)
            YEXA=TBUF( 2,NBUF)
          END IF
C
          XINT=TBUF( 5,NBUF)
          YINT=TBUF( 6,NBUF)
          XEXB=TBUF( 9,NBUF)
          YEXB=TBUF(10,NBUF)
          CALL MVINEX (XINT,YINT,XEXB,YEXB)
          IFLB=0
          IF (DSQR(TBUF( 9,NBUF),TBUF(10,NBUF),XEXB,YEXB).LE.ESQR) THEN
            IFLB=1
            XEXB=TBUF( 9,NBUF)
            YEXB=TBUF(10,NBUF)
          END IF
          IF (DSQR(TBUF( 5,NBUF),TBUF( 6,NBUF),XEXB,YEXB).LE.ESQR) THEN
            IFLB=2
            XEXB=TBUF( 5,NBUF)
            YEXB=TBUF( 6,NBUF)
          END IF
C
          IF (IFLA.EQ.2.AND.IFLB.EQ.2) THEN
            NBUF=NBUF-1
            RETURN
          END IF
C
          TBUF( 3,NBUF)=0.
          TBUF( 4,NBUF)=0.
          TBUF( 7,NBUF)=0.
          TBUF( 8,NBUF)=0.
          TBUF(11,NBUF)=0.
          TBUF(12,NBUF)=0.
C
          IF (IFLA.NE.0.AND.IFLB.NE.0) RETURN
C
          DSQA=DSQR(XEXA,YEXA,
     +              .5*(TBUF( 1,NBUF)+TBUF( 9,NBUF)),
     +              .5*(TBUF( 2,NBUF)+TBUF(10,NBUF)))
          DSQB=DSQR(XEXB,YEXB,
     +              .5*(TBUF( 5,NBUF)+TBUF( 9,NBUF)),
     +              .5*(TBUF( 6,NBUF)+TBUF(10,NBUF)))
          IF (DSQA.LE.DSQB) THEN
            TBUF( 9,NBUF)=XEXA
            TBUF(10,NBUF)=YEXA
            IF (IFLB.EQ.2) RETURN
            NBUF=NBUF+1
            TBUF( 1,NBUF)=XEXA
            TBUF( 2,NBUF)=YEXA
            TBUF( 5,NBUF)=TBUF( 5,NBUF-1)
            TBUF( 6,NBUF)=TBUF( 6,NBUF-1)
            TBUF( 9,NBUF)=XEXB
            TBUF(10,NBUF)=YEXB
          ELSE
            TBUF( 9,NBUF)=XEXB
            TBUF(10,NBUF)=YEXB
            IF (IFLA.EQ.2) RETURN
            NBUF=NBUF+1
            TBUF( 1,NBUF)=XEXA
            TBUF( 2,NBUF)=YEXA
            TBUF( 5,NBUF)=TBUF( 1,NBUF-1)
            TBUF( 6,NBUF)=TBUF( 2,NBUF-1)
            TBUF( 9,NBUF)=XEXB
            TBUF(10,NBUF)=YEXB
          END IF
C
        END IF
C
        END IF
C
C Zero Z coordinates and contour field data.
C
        TBUF( 3,NBUF)=0.
        TBUF( 4,NBUF)=0.
        TBUF( 7,NBUF)=0.
        TBUF( 8,NBUF)=0.
        TBUF(11,NBUF)=0.
        TBUF(12,NBUF)=0.
C
C Done.
C
        RETURN
C
      END


      SUBROUTINE MVINEX (XINT,YINT,XEXT,YEXT)
C
C Given an interior point (XINT,YINT), an exterior point (XEXT,YEXT),
C and an integer value specifying a number of iterative steps to be
C performed, this routine slides the two points together along the line
C joining them, keeping (XINT,YINT) interior and (XEXT,YEXT) exterior.
C The "interior" and "exterior" are defined by a function ISPOTM(X,Y),
C which has the value 1 if (X,Y) is an interior point and 0 if it is
C an exterior point.
C
        DO 101 I=1,10
C
          XTMP=.5*(XINT+XEXT)
          YTMP=.5*(YINT+YEXT)
C
          IF (ISPOTM(XTMP,YTMP).NE.0) THEN
            XINT=XTMP
            YINT=YTMP
          ELSE
            XEXT=XTMP
            YEXT=YTMP
          END IF
C
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
