      PROGRAM EXMPL3
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(33,33)
        DIMENSION A(60,60), B(60,60)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(20000)
        common /acom/ iama
        save /acom/
C
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
        DIMENSION XCRA(1000),YCRA(1000)
C
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Declare the analogous routine for drawing vectors
C
        EXTERNAL VVUDMV
C
C Declare the routine which does the shading.
C
        EXTERNAL SHADER
C
C Declare common blocks required for communication with CPMPXY.
C
        COMMON /CPMPC1/ XFOI(33),YFOJ(33)
        COMMON /CPMPC2/ XFIJ(33,33),YFIJ(33,33)
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Open GKS.
C
        CALL OPNGKS
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Turn on the positioning of labels by the penalty scheme and provide a
C little more room for X and Y coordinates defining contour lines, so
C as not to have labels right next to each other on a contour line.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
C
C Turn on the drawing of the high and low label boxes.
C
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label or another high/low label, but to move those which overlap the
C edge inward a little.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
C
C Make all CONPACK-written characters a little bigger.
C
        CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',1.25)
C
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Tell CONPACK to do no SET call.
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Turn on the special-value feature and the outlining of special-value
C areas ("contour line number -2"), using a double-width line.
C
        CALL CPSETR ('SPV - SPECIAL VALUE',1.E36)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-2)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,33,33,33,20,20,-1.,1.)
C
C Put special values in a roughly circular area.
C
        DO 102 I=1,33
          DO 101 J=1,33
            IF (REAL(I-20)**2+REAL(J-10)**2.LT.25) ZDAT(I,J)=1.E36
  101     CONTINUE
  102   CONTINUE
C
        XVPL=.05
        XVPR=.95
        YVPB=.05
        YVPT=.95
C
C Polar map
C
        CALL CPSETI ('MAP - MAPPING FUNCTION',2)
C
C Polar coordinates.
C
        CALL CPSETR ('XC1 - RHO AT I = 1',.1)
        CALL CPSETR ('XCM - RHO AT I = M',1.)
        CALL CPSETR ('YC1 - THETA AT J = 1',0.)
        CALL CPSETR ('YCN - THETA AT J = N',360.)
        CALL SET    (XVPL,XVPR,YVPB,YVPT,-1.,1.,-1.,1.,1)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,33,33,33,RWRK,5000,IWRK,1000)
C
C Force the selection of contour levels, so that associated quantities
C may be tweaked.
C
        CALL CPPKCL (ZDAT,RWRK,IWRK)
C
C Increase the line width for labelled levels and turn off the area
C identifiers for all levels.
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        DO 107 ICLV=1,NCLV
           CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
           CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
           IF (ICLU.EQ.3) THEN
              CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',2)
           END IF
           CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',0)
           CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',0)
 107    CONTINUE
C
C Add two new levels for which no contour lines are to be drawn, but
C between which shading is to be done.
C
        NCLV=NCLV+2
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV-1)
        CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',-.15)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
        CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',1)
        CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',2)
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV)
        CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',+.15)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
        CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',3)
        CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',1)
C
C Genara is called twice because the second set of data looks
C better
C
        CALL GENARA(B,A,60,60)
        CALL GENARA(B,A,60,60)
        CALL DFCLRS 
C
C Draw the contour plot with vectors overlaid twice. The first
C plot uses the same data as the contour plot for the independent
C scalar array. Therefore the colors of the vectors match the
C contours. The second plot shows the vectors colored by magnitude.
C
        DO 2000 K=1,2
           CALL ARINAM (IAMA,20000)
           CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
           CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
           CALL CPLBDR (ZDAT,RWRK,IWRK)
           CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
           CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C
C choose between vector magnitude and scalar array coloring
C
           CALL VVSETI('NLV - Number of Levels', 14)
           IF (K .EQ. 1) THEN
              CALL VVSETI('CTV -- Color Threshold Value', 2)
           ELSE
              CALL VVSETI('CTV -- Color Threshold Value', 1)
           END IF
C
C set up the color array
C
           DO 800 I=1,14,1
              CALL VVSETI('PAI -- Parameter Array Index', I)
              CALL VVSETI('CLR -- GKS Color Index', I+1)
 800       CONTINUE
C
C Set up some miscellaneous parameters
C     
           CALL VVSETR('LWD -- Vector Linewidth', 2.0)
           CALL VVSETR('AMN -- Arrow Minimum Size', 0.01)
           CALL VVSETI('VPO -- Vector Position Method', 0)
C
C this causes the P array special values to be filtered out, 
C although in this case they would not be drawn anyway because
C they are masked by the area routine
C
           CALL VVSETI('SPC -- P Special Value Color', 0)
           CALL VVSETR('PSV -- P Special Value', 1.E36)
C
C Set the mapping flag to the Polar transformation, and the 
C transformation type to 1 so that vectors will be oriented relative
C to the polar system.
C Set the X,Y array index mapping parameters to the same values used
C bythe Conpack CPSET routines above. Turn on masking and turn the
C Set call flag off.
C
           MA = 33
           NA = 33
           CALL VVSETI('MAP -- Mapping Flag', 2)
           CALL VVSETI('TRT -- Transformation Type', 1)
           CALL VVSETR('XC1 -- Lower X Bound', 0.1)
           CALL VVSETR('XCM -- Upper X Bound', 1.0)
           CALL VVSETR('YC1 -- Lower X Bound', 0.0)
           CALL VVSETR('YCN -- Upper Y Bound', 360.0)
           CALL VVSETI('SET -- Set Call Flag', 0)
           CALL VVSETI('XIN - X Grid Increment', 2)
           CALL VVSETI('MSK -- Area Mask Flag', 1)
C
C Call the initialization routine
C
           CALL VVINIT (A,60,B,60,ZDAT,33,MA,NA,0,0)
C
C Remove the bottom 05% of the vectors
C
           CALL VVGETR('VMX -- Max Vector Magnitude',VMX)
           CALL VVGETR('VMN -- Min Vector Magnitude',VMN)
           CALL VVSETR('VLC -- Vector Low Cutoff', 
     +                 VMN+0.05*(VMX-VMN))
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
C Move the Minimum and Maximum Vector text blocks out of the
C way of the text that CONPACK puts out.
C
           CALL VVSETR('MNX - Minimum Vector X Pos', 0.0)
           CALL VVSETR('MXX - Maximum Vector X Pos', 0.33)
           CALL VVSETI('MNP - Minimum Vector Justification', 2)
           CALL VVSETI('MXP - Maximum Vector Justification', 4)
C
C Turn on statistics
C
           CALL VVSETI('VST - Vector statistics', 1)

C Call VVECTR to draw the vectors, using the same area map that
C the CONPACK routines used. The 'Draw Masked Vector' routine 
C used is the one supplied with the Velocity Vector Utility.
C
           CALL VVECTR (A,B,ZDAT,IAMA,VVUDMV,0)
C
C
C Compute and print statistics for the plot and label it.
C
C           LABL='EXAMPLE 3-'//CHAR(ICHAR('0')+2)
C           CALL CAPSAP (LABL,TIME,IAMA,20000)
C           CALL LABTOP (LABL,.017)
C
C Put a boundary line at the edge of the plotter frame.
C
           CALL BNDARY
C
C Advance the frame.
C     
           CALL FRAME
C
 2000   CONTINUE
C     
C Close GKS.
C
        CALL CLSGKS
C
C Done.
C
        STOP
C
      END
      SUBROUTINE CPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
C
C This version of CPMPXY implements four different mappings:
C
C   IMAP = 1 implies an EZMAP mapping.  XINP and YINP are assumed to be
C   the longitude and latitude, in degrees, of a point on the globe.
C
C   IMAP = 2 implies a polar coordinate mapping.  XINP and YINP are
C   assumed to be values of rho and theta (in degrees).
C
C   IMAP = 3 implies an orthogonal, but unequally-spaced mapping.  XINP
C   is assumed to lie in the range from 1 to M, YINP in the range from
C   1 to N, where M and N are the dimensions of the grid.  The common
C   block CPMPC1 contains arrays XFOI and YFOJ giving the X coordinates
C   associated with I = 1 to M and the Y coordinates associated with
C   J = 1 to N.
C
C   IMAP = 4 implies a generalized distortion.  XINP is assumed to lie
C   in the range from 1 to M, YINP in the range from 1 to N, where M
C   and N are the dimensions of the grid.  The common block CPMPC2
C   contains arrays XFIJ and YFIJ, giving the X and Y coordinates
C   associated with index pairs (I,J).
C
C Declare common blocks.
C
        COMMON /CPMPC1/ XFOI(33),YFOJ(33)
        COMMON /CPMPC2/ XFIJ(33,33),YFIJ(33,33)
C
C Do the mapping.
C
        IF (IMAP.EQ.1) THEN
          CALL MAPTRN (YINP,XINP,XOTP,YOTP)
        ELSE IF (IMAP.EQ.2) THEN
          XOTP=XINP*COS(.017453292519943*YINP)
          YOTP=XINP*SIN(.017453292519943*YINP)
        ELSE IF (IMAP.EQ.3) THEN
          I=MAX(1,MIN(32,INT(XINP)))
          J=MAX(1,MIN(32,INT(YINP)))
          XOTP=(REAL(I+1)-XINP)*XFOI(I)+(XINP-REAL(I))*XFOI(I+1)
          YOTP=(REAL(J+1)-YINP)*YFOJ(J)+(YINP-REAL(J))*YFOJ(J+1)
        ELSE IF (IMAP.EQ.4) THEN
          I=MAX(1,MIN(32,INT(XINP)))
          J=MAX(1,MIN(32,INT(YINP)))
          XOTP=(REAL(J+1)-YINP)*
     +    ((REAL(I+1)-XINP)*XFIJ(I,J  )+(XINP-REAL(I))*XFIJ(I+1,J  ))
     +    +(YINP-REAL(J))*
     +    ((REAL(I+1)-XINP)*XFIJ(I,J+1)+(XINP-REAL(I))*XFIJ(I+1,J+1))
          YOTP=(REAL(J+1)-YINP)*
     +    ((REAL(I+1)-XINP)*YFIJ(I,J  )+(XINP-REAL(I))*YFIJ(I+1,J  ))
     +    +(YINP-REAL(J))*
     +    ((REAL(I+1)-XINP)*YFIJ(I,J+1)+(XINP-REAL(I))*YFIJ(I+1,J+1))
        ELSE
          XOTP=XINP
          YOTP=YINP
        END IF
C
C Done.
C
        RETURN
C
      END
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
  101   CONTINUE
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
  101   CONTINUE
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
