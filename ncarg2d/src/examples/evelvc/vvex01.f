
      PROGRAM VVEX01
C
C This example overlays vectors on a polar contour plot using 
C data generated with a randomizing algorithm. The first frame colors
C the vectors according to the data used to generate the contour plot,
C with the result that the color of the vectors corresponds to the 
C contour level at each location. In the second frame the vectors 
C are colored by magnitude.
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
C The contour, vector field component, and area map array declarations:
C
      PARAMETER (MSIZE=33, NSIZE=33)
      PARAMETER (LAMA=25000)
      DIMENSION ZDAT(MSIZE,NSIZE)
      DIMENSION U(60,60), V(60,60)
      DIMENSION IAMA(LAMA)
C
C Workspace arrays for Conpack:
C
      DIMENSION RWRK(5000),IWRK(1000)
C
C ARSCAM arrays:
C
      DIMENSION XCRA(1000),YCRA(1000)
      DIMENSION IARA(10),IGRA(10)
C
C Declare the masked rendering routines for drawing and shading the
C contour plot, as well as for drawing the vectors
C
      EXTERNAL DRAWCL
      EXTERNAL SHADER
      EXTERNAL VVUDMV
C
      DATA U / 3600 * 0.0 /
C
C Initialization
C ==================================================================
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
C
C Generate a scalar data array, then create a special value region
C
      CALL GENDAT (ZDAT,MSIZE,MSIZE,NSIZE,20,20,-1.,1.)
C
      DO 102 I=1,MSIZE
         DO 101 J=1,NSIZE
            IF (REAL(I-20)**2+REAL(J-10)**2.LT.25) ZDAT(I,J)=1.E36
 101     CONTINUE
 102  CONTINUE
C
C Subroutine GENARA generates smoothly varying random data in its 
C second array argument based upon the contents of the first. Call it
C twice to randomize both the U and V vector component data arrays.
C Then set up the color table.
C
      CALL GENARA(U,V,60,60)
      CALL GENARA(V,U,60,60)
      CALL DFCLRS(IWKID) 
C
C Conpack setup:
C ===============================================================
C Set up a polar coordinate system mapping for Conpack
C
      CALL SET    (0.05,0.95,0.05,0.95,-1.0,1.0,-1.0,1.0,1)
      CALL CPSETI ('MAP - Mapping Function',2)
      CALL CPSETI ('SET - Do-Set-Call Flag',0)
      CALL CPSETR ('XC1 - Rho At I = 1',.1)
      CALL CPSETR ('XCM - Rho At I = M',1.)
      CALL CPSETR ('YC1 - Theta At J = 1',0.0)
      CALL CPSETR ('YCN - Theta At J = N',360.0)
C
C Enable special value processing and outline special value regions
C
      CALL CPSETR ('SPV - Special Value',1.E36)
      CALL CPSETI ('PAI - Parameter Array Index',-2)
      CALL CPSETI ('CLU - Contour Level Use Flag',1)
      CALL CPSETR ('CLL - Contour Level Line Width',2.)
C
C Adjust Conpack labelling and outline the data field.
C
      CALL CPSETI ('LLP - Line Label Positioning',3)
      CALL CPSETI ('RWC - Real Workspace For Contours',200)
      CALL CPSETI ('HLB - High/Low Label Box Flag',1)
      CALL CPSETI ('HLO - High/Low Label Overlap Flag',11)
      CALL CPSETR ('CWM - Character Width Multiplier',1.25)
      CALL CPSETI ('PAI - Parameter Array Index',-1)
      CALL CPSETI ('CLU - Contour Level Use Flag',1)
      CALL CPSETR ('CLL - Contour Level Line Width',2.)
C
C Initialize the drawing of the contour plot, and tell Conpack 
C to pick the contour levels.
C
      CALL CPRECT (ZDAT,MSIZE,MSIZE,NSIZE,RWRK,5000,IWRK,1000)
      CALL CPPKCL (ZDAT,RWRK,IWRK)
C
C Set the attributes of the contour lines
C
      CALL CPGETI ('NCL - Number Of Contour Levels',NCLV)
      DO 107 ICLV=1,NCLV
         CALL CPSETI ('PAI - Parameter Array Index',ICLV)
         CALL CPGETI ('CLU - Contour Level Use Flag',ICLU)
         IF (ICLU.EQ.3) THEN
            CALL CPSETI ('CLL - Contour-Line Line Width',2)
         END IF
         CALL CPSETI ('AIA - Area Identifier Above Level',0)
         CALL CPSETI ('AIB - Area Identifier Below Level',0)
 107  CONTINUE
C
C Add two new levels for which no contour lines are to be drawn, but
C between which shading is to be done.
C
      NCLV=NCLV+2
      CALL CPSETI ('NCL - Number Of Contour Levels',NCLV)
      CALL CPSETI ('PAI - Parameter Array Index',NCLV-1)
      CALL CPSETR ('CLV - Contour Level Value',-.15)
      CALL CPSETI ('CLU - Contour Level Use Flag',0)
      CALL CPSETI ('AIA - Area Identifier Above Level',1)
      CALL CPSETI ('AIB - Area Identifier Below Level',2)
      CALL CPSETI ('PAI - Parameter Array Index',NCLV)
      CALL CPSETR ('CLV - Contour Level Value',+.15)
      CALL CPSETI ('CLU - Contour Level Use Flag',0)
      CALL CPSETI ('AIA - Area Identifier Above Level',3)
      CALL CPSETI ('AIB - Area Identifier Below Level',1)
C
C Initialize the area map and draw the contour labels into it.
C
      CALL ARINAM (IAMA,LAMA)
      CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Vectors setup:
C ==================================================================
C Set the mapping flag for the polar transformation.
C Set the X,Y array index mapping parameters to the same values used
C bythe Conpack CPSET routines above. Turn on masking and turn the
C Set call flag off.
C
      CALL VVSETI('MAP -- Mapping Flag', 2)
      CALL VVSETI('SET -- Set Call Flag', 0)
      CALL VVSETR('XC1 -- Lower X Bound', 0.1)
      CALL VVSETR('XCM -- Upper X Bound', 1.0)
      CALL VVSETR('YC1 -- Lower X Bound', 0.0)
      CALL VVSETR('YCN -- Upper Y Bound', 360.0)
      CALL VVSETI('MSK -- Area Mask Flag', 1)
C     
C Enable special value processing for the P array to eliminate
C vectors from the special value region. This is not really required
C in this case since the masking routine eliminates these vectors.
C     
      CALL VVSETI('SPC -- P Special Value Color', 0)
      CALL VVSETR('PSV -- P Special Value', 1.E36)
C
C Enable vector coloring
C
      CALL VVSETI('NLV - Number of Levels', 14)
      DO 800 I=1,14,1
         CALL VVSETI('PAI -- Parameter Array Index', I)
         CALL VVSETI('CLR -- GKS Color Index', I+1)
 800  CONTINUE
C
C Set up miscellaneous attribute parameters
C     
      CALL VVSETR('LWD -- Vector Linewidth', 2.0)
      CALL VVSETR('AMN -- Arrow Minimum Size', 0.01)
      CALL VVSETI('VPO -- Vector Position Method', 0)
      CALL VVSETI('XIN - X Grid Increment', 2)
C     
C Move the minimum and maximum vector text blocks out of the
C way of the text that Conpack puts out.
C
      CALL VVSETR('MNX - Minimum Vector X Pos', 0.0)
      CALL VVSETR('MXX - Maximum Vector X Pos', 0.33)
      CALL VVSETI('MNP - Minimum Vector Justification', 2)
      CALL VVSETI('MXP - Maximum Vector Justification', 4)
C
C Turn on statistics
C
      CALL VVSETI('VST - Vector statistics', 1)
C
C Drawing loop
C ===================================================================
C Draw the contour plot with vectors overlaid twice. In the first
C plot Vectors uses the same data as Conpack for the independent
C scalar array. Therefore the colors of the vectors correspond to the
C contours. The second plot shows the vectors colored by magnitude.
C
      DO 2000 K=1,2
C
C First draw masked contour lines, then labels, then put the
C contour lines in the area map for shading by ARSCAM
C
         CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
         CALL CPLBDR (ZDAT,RWRK,IWRK)
         CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
         CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Choose between vector magnitude and scalar array coloring
C
         IF (K .EQ. 1) THEN
            CALL VVSETI('CTV -- Color Threshold Value', 2)
         ELSE
            CALL VVSETI('CTV -- Color Threshold Value', 1)
         END IF
C
C Initialize Vectors
C
         CALL VVINIT (U,60,V,60,ZDAT,MSIZE,MSIZE,NSIZE,0,0)
C
C Remove the bottom 05% of the vectors
C
         CALL VVGETR('VMX -- Max Vector Magnitude',VMX)
         CALL VVGETR('VMN -- Min Vector Magnitude',VMN)
         CALL VVSETR('VLC -- Vector Low Cutoff', 
     +        VMN+0.05*(VMX-VMN))
C
C Increase the size of the longest vector by 50% from its default
C value and make the shortest one fourth the length of the longest.
C
         CALL VVGETR('DMX -- Max Vector Device Magnitude',DMX)
         CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
         VRL = 1.5 * DMX / (VR - VL)
         CALL VVSETR('VRL - Vector Realized Length', VRL)
         CALL VVSETR('VFR -- Vector Fractional Minimum', 0.25)
C
C Call VVECTR to draw the vectors, using the same area map that
C the Conpack routines used. The 'Draw Masked Vector' routine 
C used is the one supplied with the Velocity Vector Utility.
C
         CALL VVECTR (U,V,ZDAT,IAMA,VVUDMV,0)
C
C Put a boundary line at the edge of the plotter frame.
C
         CALL BNDARY
C
C Advance the frame.
C     
         CALL FRAME
C
 2000 CONTINUE
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
C
      SUBROUTINE DRAWCL (XCS,YCS,NCS,IAI,IAG,NAI)
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
      SUBROUTINE SHADER (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADER shades the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
C group 3, its area identifier is a 1.  The package SOFTFILL is used
C to do the shading.
C
      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Define workspaces for the shading routine.
C
      DIMENSION DST(1100),IND(1200)
C
C Turn off shading.
C
      ISH=0
C
C If the area identifier for group 3 is a 1, turn on shading.
C
      DO 101 I=1,NAI
         IF (IAG(I).EQ.3.AND.IAI(I).EQ.1) ISH=1
 101  CONTINUE
C
C If shading is turned on, shade the area.  The last point of the
C edge is redundant and may be omitted.
C
      IF (ISH.NE.0) THEN
         CALL SFSETI ('ANGLE',45)
         CALL SFSETR ('SPACING',.006)
         CALL SFWRLD (XCS,YCS,NCS-1,DST,1100,IND,1200)
         CALL SFSETI ('ANGLE',135)
         CALL SFNORM (XCS,YCS,NCS-1,DST,1100,IND,1200)
      END IF
C
C Done.
C
      RETURN
C
      END
