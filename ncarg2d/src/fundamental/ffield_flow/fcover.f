
      PROGRAM FCOVER
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
C This program requires the input data file 'fcover.dat'
C It reads the data from standard input, e.g.: fcover < fcover.dat
C
      PARAMETER (MSIZE=73, NSIZE=73)
C
      DIMENSION U(MSIZE,NSIZE), V(MSIZE,NSIZE), P(MSIZE,NSIZE)
C
C Conpack work space and Areas area map
C
      PARAMETER (ICPIWL=1000, ICPRWL=5000, IAREAL=20000)
      DIMENSION RWRK(ICPRWL),IWRK(ICPIWL)
      DIMENSION IAM(IAREAL)
C
C Arrays for drawing masked grids
C
      PARAMETER (MCRA=64, NOGI=64)
      DIMENSION XCRA(MCRA),YCRA(MCRA),IAAI(NOGI),IAGI(NOGI)
C
C map filling arrays
C
      PARAMETER (LMAP=150000,NWRK=10000,ISIZ=5)
      INTEGER MAP(LMAP), IAREA(ISIZ), IGRP(ISIZ)
      REAL XWRK(NWRK), YWRK(NWRK)
C
C External subroutine declarations
C
      EXTERNAL FILL
      EXTERNAL DRAWCL
      EXTERNAL VVUDMV
      PARAMETER (NCLRS1=15, NCLRS2=9)
      DIMENSION RGB1(3,NCLRS1)
      DIMENSION ICLR1(NCLRS1)
C
      DIMENSION RGB2(3,NCLRS2)
      DIMENSION ICLR2(NCLRS2)
      COMMON /CBFILL/ IFILIX(2), RLWFAC
C
C Empirically determined vector thinning data
C Starting from the pole, each row of vectors is weeded, mod the
C value of this data.
C
      PARAMETER (NROWS = 11)
      DIMENSION ITHIN(NROWS)
      DATA ITHIN /90,15,5,5,4,4,3,3,2,2,2/
C
C Indices used for vector coloring
C
      DATA ICLR1 / 10, 17, 24, 31, 38, 45, 52, 60, 67, 
     +             74, 81, 88, 95, 102, 109 /
C
C Define a set of RGB color triples for vector colors
C
C
      DATA RGB1 /
     +       0.00000,   1.00000,   0.00000,
     +       0.14286,   1.00000,   0.00000,
     +       0.28571,   1.00000,   0.00000,
     +       0.42857,   1.00000,   0.00000,
     +       0.57143,   1.00000,   0.00000,
     +       0.71429,   1.00000,   0.00000,
     +       0.85714,   1.00000,   0.00000,
     +       1.00000,   1.00000,   0.00000,
     +       1.00000,   0.85714,   0.00000,
     +       1.00000,   0.71429,   0.00000,
     +       1.00000,   0.57143,   0.00000,
     +       1.00000,   0.42857,   0.00000,
     +       1.00000,   0.28571,   0.00000,
     +       1.00000,   0.14286,   0.00000,
     +       1.00000,   0.00000,   0.00000 /
C
C Indices used for coloring other plot features; be careful not
C to duplicate any color indices in ICLR1
C
      DATA ICLR2 / 0, 1, 2, 149, 225, 175, 176, 200, 3 /
C
C RGB values for other plot features
C
      DATA RGB2 / 
     +     1.0,1.0,1.0,
     +     0.0,0.0,0.0,
     +     0.9,0.9,0.9,
     +     0.6,0.6,0.6,
     +     0.3,0.3,0.3,
     +     0.8,0.9,1.0,
     +     0.5,0.0,0.5, 
C
C This is the ocean color; the color actually used on the 
C Fundamentals cover is the following commented out value --
C for viewing on a workstation a bit darker color makes the
C lettering easier to read.
C
C     +     0.0,0.9,1.0,
     +     0.0,0.5,0.7,
     +     0.0,0.0,0.0  /
C
C -----------------------------------------------------------------
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Give initial value to fill color index stored common block CBFILL
C
      IFILIX(1) = 149
      IFILIX(2) = 200
      RLWFAC=1.0
C
C Set up auxiliary colors
C
      DO 10 I=1,NCLRS2,1
         CALL GSCR(IWKID,ICLR2(I),RGB2(1,I),RGB2(2,I),RGB2(3,I))
 10   CONTINUE
C
C Read the input array data
C
      CALL RDDATA(U,V,P,MSIZE,NSIZE)
C
C Message the data to eliminate surplus of vectors near the pole
C
      DO 50 J=NSIZE,NSIZE-NROWS+1,-1
         DO 49 I=1,MSIZE
            IF (MOD(I,ITHIN(NSIZE-J+1)).NE.0) THEN
               U(I,J) = -9999.0
            ENDIF
 49      CONTINUE
 50   CONTINUE
C
C Set up the EZMAP projection
C         
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','CO')
      CALL MAPSET('CO',10.0,-180.0,10.0,0.0)
      CALL MAPROJ('ST',90.0,180.0,45.0)
      CALL MAPINT
C
C Initialize Maps and Areas
C
      CALL MAPINT
      CALL ARINAM (MAP,LMAP)
      CALL MAPBLA (MAP)
C
C Tell Vectors to use the mapping established by EZMAP
C
      CALL VVSETI('MAP -- Mapping Flag', 1)
      CALL VVSETI('SET -- Set Call Flag', 0)
C
C Set up data coordinate boundaries and special value processing 
C appropriately for the dataset 
C
      CALL VVSETR('XC1 -- Lower X Bound', -180.0)
      CALL VVSETR('XCM -- Upper X Bound', 180.0)
      CALL VVSETR('YC1 -- Lower X Bound', -90.0)
      CALL VVSETR('YCN -- Upper Y Bound', 90.0)
C
      CALL VVSETI('SVF -- Special Values Flag', 3)
      CALL VVSETR('USV -- U Special Value', -9999.0)
      CALL VVSETR('VSV -- V Special Value', -9999.0)
      CALL VVSETR('PSV - P Special Value', -9999.0)
      CALL VVSETI('SPC - P Special Color', 1)
C
C Do the equivalent Conpack setup; note that the special value
C parameter works a bit differently, and also Conpack requires
C an Out of Range value to be set whenever the data grid extends
C outside the map boundaries. The standard value for the default
C version of CPMPXY is 1.0E12
C
      CALL CPSETI('MAP -- Mapping Flag', 1)
      CALL CPSETI('SET -- Set Call Flag', 0)
      CALL CPSETR('XC1 -- Lower X Bound', -180.0)
      CALL CPSETR('XCM -- Upper X Bound', 180.0)
      CALL CPSETR('YC1 -- Lower X Bound', -90.0)
      CALL CPSETR('YCN -- Upper Y Bound', 90.0)
      CALL CPSETR('SPV -- Special Value',-9999.0)
      CALL CPSETR('ORV -- Out of Range Value',1.0E12)
C
C Set Conpack graphics text parameters
C
      CALL SETCGT
C
C Turn on statistics reporting, turn off vector text blocks
C
      CALL VVSETI('VST -- Vector Statistics', 1)
      CALL VVSETC('MNT - Minimum Vector Text Block', ' ')
      CALL VVSETC('MXT - Maximum Vector Text Block', ' ')
C
C Initialize the drawing of the contour plot, and tell Conpack
C to pick contour levels.
C
      CALL CPRECT (P,MSIZE,MSIZE,NSIZE,RWRK,ICPRWL,IWRK,ICPIWL)
      CALL CPPKCL (P,RWRK,IWRK)
C
C Set up contour line attributes
C
      CALL CPGETI('NCL - Number Of Contour Levels',NCLV)
      CALL SETCLA(NCLV, ICLR2(6),ICLR2(3))
C
C Initialize the area map, and add the Conpack labels to the area map
C
      CALL ARINAM (IAM,IAREAL)
      CALL CPLBAM (P,RWRK,IWRK,IAM)
C
C Set up vector color processing
C
      CALL VVSETI('MSK -- Vector Masking',1)
      CALL VVSETI('CTV -- Color Thresholds Value', 2)
      CALL VVSETI('NLV -- Number Of Levels', NCLRS1)
      DO 100 J=1,NCLRS1,1
         CALL GSCR(IWKID,ICLR1(J),RGB1(1,J),RGB1(2,J),RGB1(3,J))
         CALL VVSETI('PAI -- Parameter Array Index', J)
         CALL VVSETI('CLR -- GKS Color Index', ICLR1(J))
 100  CONTINUE
C
C Modify the color table for a blue background
C and modify the contour attributes
C
      CALL GSCR(IWKID,ICLR2(1),RGB2(1,8),RGB2(2,8),RGB2(3,8))
      CALL GSCR(IWKID,ICLR2(2),RGB2(1,1),RGB2(2,1),RGB2(3,1))
      CALL SETCLA(NCLV, ICLR2(7),ICLR2(3))
C
C Draw four frames showing first the complete picture, then the
C plot decomposed into 1) Ezmap components 2) Conpack components
C and 3) Vectors components
C
      DO 1000 I=1,4
C
C Solid file continental boundaries
C
         IF (I.EQ.1 .OR. I.EQ.2) THEN
C    
C Color fill land masses using a gray scale value
C
            CALL GSFAIS (1)
            CALL ARSCAM (MAP, XWRK, YWRK, NWRK, IAREA, IGRP, ISIZ, FILL)
C
C Draw boundaries, including the limb
C
            CALL MAPSTI('C5 - Continental Outline Color',ICLR2(9))
            CALL MAPLOT
         END IF
C
C Draw the masked contour lines
C
         IF (I.EQ.1 .OR. I.EQ.3) THEN
            CALL CPCLDM (P,RWRK,IWRK,IAM,DRAWCL)
         END IF
C
C Draw the map grid
C
         IF (I.EQ.1 .OR. I.EQ.2) THEN
            CALL MAPSTI('C2 - Grid',ICLR2(9))
            CALL GSPLCI(ICLR2(9))
            CALL MAPGRM (IAM,XCRA,YCRA,MCRA,IAAI,IAGI,NOGI,DRAWCL)
            CALL GSPLCI(1)
         END IF
C
         IF (I.EQ.1 .OR. I.EQ.4) THEN
C
C Initialize Vectors
C
            CALL VVINIT (U,MSIZE,V,MSIZE,P,MSIZE,MSIZE,NSIZE,IDM,IDM)
C
C Adjust vector rendering options
C
            CALL VVSETR('AMN -- Arrow Minimum Size',0.007)
            CALL VVSETR('LWD -- Vector Line Width',3.00*RLWFAC)
            CALL VVGETR('VMN -- Minimum Vector',VMN)
            CALL VVGETR('VMX -- Maximum Vector',VMX)
            CALL VVSETR('VLC -- Vector Low Cutoff',VMN+0.1*(VMX-VMN))
            CALL VVGETR('DMX -- Device Maximum Vector Length',DMX)
            CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
            CALL VVSETR('VRL - Vector Realized Length',4.0*DMX/(VR-VL))
            CALL VVSETR('VFR -- Vector Fractional Minimum',0.4)
C
C Draw the vector field plot
C
            CALL VVECTR (U,V,P,IAM,VVUDMV,IDM)
         END IF
C
C Draw labels last
C
         IF (I.EQ.1 .OR. I.EQ.3) THEN
            CALL CPSETI('HLB',1)
            CALL GSFAIS(0)
            CALL CPLBDR (P,RWRK,IWRK)
         END IF
C
C Draw a perimeter boundary and eject the frame
C
         CALL PERIM(1,0,1,0)
         CALL FRAME
C
 1000 CONTINUE
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
      SUBROUTINE RDDATA(U,V,P,M,N)
C
C Read the data arrays from the standard input 
C
      DIMENSION U(M,N), V(M,N), P(M,N)
C     
      READ (*,*) U
      READ (*,*) V
      READ (*,*) P
C
      RETURN
      END
C
C =====================================================================
C
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
C
C Routine for masked drawing of contour and grid lines
C
C This version of DRAWCL draws the polyline defined by the points
C ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
C for the area containing the polyline are negative.  The dash package
C routine CURVED is called to do the drawing.
C
      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Turn on drawing.
C
      IDR=1
C
C If any area identifier is negative, turn off drawing.
C
      DO 101 I=1,NAI
         IF (IAI(I).LT.0) IDR=0
 101  CONTINUE
C
C If drawing is turned on, draw the polyline.
C
      IF (IDR.NE.0) CALL CURVED (XCS,YCS,NCS)
C
C Done.
C
      RETURN
C
      END
C
C =====================================================================
C
      SUBROUTINE SETCGT
C
C Sets Conpack Graphics Text Parameters
C
      CALL CPSETI ('LLP - LINE LABEL POSITIONING',0)
      CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
      CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
      CALL CPSETI ('LLB - line label box flag',0)
      CALL CPSETC ('ILT - INFORMATION LABEL TEXT',' ')
      CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
      CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',1.25)
C
      RETURN
      END
C
C =====================================================================
C
      SUBROUTINE SETCLA (NCLV, ICLCLR, IAXCLR)
C
      COMMON /CBFILL/ IFILIX(2), RLWFAC
C
C Sets Contour Line Attributes
C
      DO 100 ICLV=1,NCLV
C
         CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
         CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
         CALL CPSETR ('CLL - CONTOUR-LINE LINE WIDTH',3.0*RLWFAC)
         CALL CPSETI ('CLC - CONTOUR-LINE COLOR', ICLCLR)
         IF (ICLU.EQ.3) THEN
            CALL CPSETR ('CLL - CONTOUR-LINE LINE WIDTH',6.0*RLWFAC)
         END IF
C
 100  CONTINUE
C
C 'Special' contour lines - grid, special values, and out of range
C boundaries 
C
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
      CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
      CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
      CALL CPSETI ('CLC - CONTOUR LEVEL LINE COLOR',IAXCLR)
C
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
      CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
      CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
      CALL CPSETI ('CLC - CONTOUR LEVEL LINE COLOR',IAXCLR)
C
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-3)
      CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
      CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
      CALL CPSETI ('CLC - CONTOUR LEVEL LINE COLOR',IAXCLR)
C
      RETURN
      END
C
      SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,IDSIZ)
C
      DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)
C
      COMMON /CBFILL/ IFILIX(2), RLWFAC
C
C Retrieve area id for geographic area
C
      ID = 0
      DO 10, I=1,IDSIZ
         IF (IGRP(I) .EQ. 1) ID = IAREA(I)
 10   CONTINUE
C
C If it's not water, draw it
C
      IF (ID .GE. 1) THEN
         IF (MAPACI(ID).NE.1) THEN
            CALL GSFACI(IFILIX(1))
         ELSE
            CALL GSFACI(IFILIX(2))
         END IF
         CALL GFA(NWRK,XWRK,YWRK)
      ENDIF
C
C Otherwise, do nothing
C
      RETURN
      END
