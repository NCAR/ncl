
        PROGRAM CPEX05
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
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(53,37)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(40000)
C
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
        DIMENSION XCRA(1000),YCRA(1000)
C
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare arrays to hold the list of indices and the list of labels
C required by the label-bar routine.
C
        DIMENSION LIND(20)
        CHARACTER*2 LLBS(21)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C Declare the routine which does the shading of areas from an area map.
C
        EXTERNAL SHADAM
C
C Define the list of indices and the list of labels required by the
C label-bar routine.
C
        DATA LIND / 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19 /
C
        DATA LLBS / '0',' ','1',' ','2',' ','3',' ','4',' ','5',
     +              ' ','6',' ','7',' ','8',' ','9',' ','10' /
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Generate a relatively smooth array of test data, with values ranging
C from 0 to 10.
C
        CALL GENDAT (ZDAT,53,53,37,10,10,0.,10.)
C
C Initialize the software fill package to do the desired type of fill.
C
        CALL SFSETI ('TYPE OF FILL',-4)
        CALL SFSETI ('ANGLE OF FILL LINES',15)
        CALL SFSETR ('SPACING OF FILL LINES',.000625)
C
C Turn off line labels.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',0)
C
C Turn off the drawing of high and low labels.
C
        CALL CPSETC ('HLT - HIGH/LOW LABEL TEXT',' ')
C
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Turn off the informational label.
C
        CALL CPSETC ('ILT - INFORMATIONAL LABEL TEXT',' ')
C
C Force the use of contour lines at the values 1, 2, 3, ... 9 and
C values half-way between.  Arrange for the areas between contour lines
C to be shaded.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',0)
C
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',19)
C
        DO 101 I=1,19
          CALL CPSETI ('PAI - PARAMETER ARRAY IDENTIFIER',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',REAL(I)/2.)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',1)
          CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',I+1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',I)
  101   CONTINUE
C
C Force the plot into the upper portion of the frame.
C
        CALL CPSETR ('VPB - VIEWPORT BOTTOM',.25)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,53,53,37,RWRK,5000,IWRK,1000)
C
C Draw the contour plot.
C
        CALL ARINAM (IAMA,40000)
        CALL CPCLDR (ZDAT,RWRK,IWRK)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADAM)
C
C Put a horizontal label bar at the bottom of the frame.
C
        CALL LBSETR ('WBL - WIDTH OF BOX LINES',2.)
        CALL LBLBAR (0,.05,.95,.15,.25,20,1.,.5,LIND,0,LLBS,21,1)
C
C Compute and print statistics for the plot and label it.
C
        CALL CAPSAP ('EXAMPLE 5',IAMA,40000)
        CALL LABTOP ('EXAMPLE 5',.017)
C
C Put a boundary line at the edge of the plotter frame.
C
        CALL BNDARY
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
C Done.
C
        STOP
C
      END


      SUBROUTINE SHADAM (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADAM shades the area-map polygon whose edge is
C defined by the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative
C to edge group 3, its area identifier is between 1 and 20.  The density
C of the shading increases with the area identifier.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Declare the workspaces required by SFSGFA.
C
        DIMENSION DST(1100),IND(1200)
C
C Turn off shading.
C
        ISH=0
C
C If the area identifier for group 3 is in the right range, turn on
C shading.
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.3.AND.IAI(I).GE.1.AND.IAI(I).LE.20) ISH=IAI(I)
  101   CONTINUE
C
C If shading is turned on, shade the area.  The last point of the
C edge is redundant and may be omitted.
C
        IF (ISH.NE.0) CALL SFSGFA (XCS,YCS,NCS,DST,1100,IND,1200,ISH-1)
C
C Done.
C
        RETURN
C
      END
