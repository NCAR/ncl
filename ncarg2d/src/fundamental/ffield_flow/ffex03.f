
      PROGRAM FFEX03
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
C This program requires the input data file 'ffex02.dat'
C It reads the data from standard input, e.g.: ffex03 < ffex02.dat
C
      PARAMETER (MSIZE=36, NSIZE=33)
      PARAMETER (ISTWKL=2*MSIZE*NSIZE)
C
      DIMENSION U(MSIZE,NSIZE), V(MSIZE,NSIZE), P(MSIZE,NSIZE)
      DIMENSION WRK(ISTWKL)      
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
C External subroutine declarations
C
      EXTERNAL DRAWCL
      EXTERNAL STUMSL
C
C Define a set of RGB color triples
C
      PARAMETER (NCLRS=3)
      COMMON ICLR(NCLRS)
C
      DIMENSION RGBV(3,NCLRS)
C
      DATA RGBV / 
     +     1.0,0.0,0.0,
     +     0.0,0.0,1.0,
     +     0.5,0.5,0.5 /
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Read the input array data
C
      CALL RDDATA(U,V,P,MSIZE,NSIZE)
C
C Set up colors for fixed table grayscale and color workstations
C Colors allocated as follows: 1, map outline; 2, contour lines;
C  3, grid lines; (streamlines and labels use default black or white.
C
      DO 100 I=1,NCLRS,1
         ICLRIX=64+(I-1)*196/NCLRS
         CALL GSCR(IWKID,ICLRIX,RGBV(1,I),RGBV(2,I),RGBV(3,I))
         ICLR(I) = ICLRIX
 100  CONTINUE
C
C Set up the EZMAP transformation
C
      CALL MAPSET('CO',60.0,-120.0,23.0,-60.0)
      CALL MAPROJ('LC',0.0,-75.0,45.0)
      CALL MAPINT
      CALL MAPSTI('C5 - Continental Outline Color',ICLR(1))
C
C Tell Streamlines to use the mapping established by EZMAP
C
      CALL STSETI('MAP -- Mapping Flag', 1)
      CALL STSETI('SET -- Set Call Flag', 0)
C
C Set up data coordinate boundaries and special value processing 
C appropriately for the dataset 
C
      CALL STSETR('XC1 -- Lower X Bound', -140.0)
      CALL STSETR('XCM -- Upper X Bound', -52.5)
      CALL STSETR('YC1 -- Lower X Bound', 20.0)   
      CALL STSETR('YCN -- Upper Y Bound', 60.0)
C
      CALL STSETI('SVF -- Special Values Flag', 1)
      CALL STSETR('USV -- U Special Value', -9999.0)
      CALL STSETR('VSV -- V Special Value', -9999.0)
C
C Do the equivalent Conpack setup; note that the special value
C parameter works a bit differently, and also Conpack requires
C an Out of Range value to be set whenever the data grid extends
C outside the map boundaries. The standard value for the default
C version of CPMPXY is 1.0E12
C
      CALL CPSETI('MAP -- Mapping Flag', 1)
      CALL CPSETI('SET -- Set Call Flag', 0)
      CALL CPSETR('XC1 -- Lower X Bound', -140.0)
      CALL CPSETR('XCM -- Upper X Bound', -52.5)
      CALL CPSETR('YC1 -- Lower X Bound', 20.0)   
      CALL CPSETR('YCN -- Upper Y Bound', 60.0)
      CALL CPSETR('SPV -- Special Value',-9999.0)
      CALL CPSETR('ORV -- Out of Range Value',1.0E12)
C
C Set Conpack graphics text parameters
C
      CALL SETCGT
C
C Draw the continental outline using a wide line
C
      CALL GSLWSC(4.0)
      CALL MAPLOT
      CALL GSLWSC(1.0)
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
      CALL SETCLA(NCLV)
C
C Initialize the area map, add the Conpack labels to the area map,
C then draw the labels, draw masked contour lines, and finally
C draw the masked map grids. Note that there is currently no way
C to draw a masked continental outline.
C
      CALL ARINAM (IAM,IAREAL)
      CALL CPLBAM (P,RWRK,IWRK,IAM)
      CALL CPLBDR (P,RWRK,IWRK)
      CALL CPCLDM (P,RWRK,IWRK,IAM,DRAWCL)
      CALL MAPSTI('C2 - Grid',ICLR(3))
      CALL GSPLCI(ICLR(3))
      CALL MAPGRM (IAM,XCRA,YCRA,MCRA,IAAI,IAGI,NOGI,DRAWCL)
      CALL GSPLCI(1)
C
C Adjust streamline rendering options and turn on statistics
C
      CALL STSETR('LWD -- Streamline Line Width',1.75)
      CALL STSETI('MSK -- Streamline Masking',1)
      CALL STSETR('SSP -- Stream Spacing', 0.012)
      CALL STSETR('DFM -- Differential Magnitude', 0.012)
      CALL STSETI('SST -- Streamline Statistics', 1)
C
C Initialize Streamlines
C
      CALL STINIT (U,MSIZE,V,MSIZE,IDM,IDM,MSIZE,NSIZE,WRK,ISTWKL)
C
C Draw the streamline field plot
C
      CALL STREAM (U,V,IDM,IAM,STUMSL,WRK)
C
C Draw a perimeter boundary and eject the frame
C
      CALL PERIM(1,0,1,0)
      CALL FRAME
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
C =====================================================================
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
      CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
      CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
      CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
      CALL CPSETI ('LLB - HIGH/LOW LABEL BOX FLAG',0)
      CALL CPSETC ('ILT - INFORMATION LABEL TEXT',' ')
      CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
      CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',1.25)
C
      RETURN
      END
C
C =====================================================================
C
      SUBROUTINE SETCLA (NCLV)
C
C Sets Contour Line Attributes
C
      PARAMETER (NCLRS=3)
      COMMON ICLR(NCLRS)
C
      DO 100 ICLV=1,NCLV
C
         CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
         CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
         CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',3)
         CALL CPSETI ('CLC - CONTOUR-LINE COLOR', ICLR(2))
         IF (ICLU.EQ.3) THEN
            CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',6)
         END IF
C
 100  CONTINUE
C
C 'Special' contour lines - grid, special values, and out of range
C boundaries
C
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
      CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
      CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
      CALL CPSETI ('CLC - CONTOUR LEVEL LINE COLOR',ICLR(3))
C
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
      CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
      CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
      CALL CPSETI ('CLC - CONTOUR LEVEL LINE COLOR',ICLR(3))
C
      CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-3)
      CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
      CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
      CALL CPSETI ('CLC - CONTOUR LEVEL LINE COLOR',ICLR(3))
C
      RETURN
      END
