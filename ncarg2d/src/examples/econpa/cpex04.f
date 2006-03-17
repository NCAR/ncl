
        PROGRAM CPEX04
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
        DIMENSION IAMA(30000)
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
C Declare an array to hold line labels.
C
        CHARACTER*4 LABL(10)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Declare the routine which does the shading.
C
        EXTERNAL SHADAM
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IASF / 13*1 /
C
C Define a set of line labels.
C
        DATA LABL / '0-1 ','1-2 ','2-3 ','3-4 ','4-5 ',
     +              '5-6 ','6-7 ','7-8 ','8-9 ','9-10' /
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
        CALL SFSETI ('TYPE OF FILL',-2)
        CALL SFSETI ('ANGLE OF FILL LINES',45)
        CALL SFSETR ('SPACING OF FILL LINES',.000625)
C
C Turn on the positioning of labels by the penalty scheme and provide a
C little more room for X and Y coordinates defining contour lines, so
C as not to have labels right next to each other on a contour line.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
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
C Turn on line-label boxes.
C
        CALL CPSETI ('LLB - LINE LABEL BOXES',1)
C
C Force the use of contour lines at the values 1, 2, 3, ... 9 and
C provide for labelling, but not drawing, contour lines at levels
C .5, 1.5, 2.5, ... 9.5, with labels of the form "0-1", "1-2", ...,
C "9-10".  Arrange for the areas between contour lines drawn to be
C shaded.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',0)
C
        CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',19)
C
        DO 101 I=1,9
          CALL CPSETI ('PAI - PARAMETER ARRAY IDENTIFIER',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',REAL(I))
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',1)
          CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',I+1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',I)
  101   CONTINUE
C
        DO 102 I=10,19
          CALL CPSETI ('PAI - PARAMETER ARRAY IDENTIFIER',I)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',REAL(I-10)+.5)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE',2)
          CALL CPSETC ('LLT - LINE LABEL TEXT',LABL(I-9))
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',0)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',0)
  102   CONTINUE
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,53,53,37,RWRK,5000,IWRK,1000)
C
C Draw the contour plot.
C
        CALL ARINAM (IAMA,30000)
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDAT,RWRK,IWRK)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADAM)
C
C Compute and print statistics for the plot and label it.
C
        CALL CAPSAP ('EXAMPLE 4',IAMA,30000)
        CALL LABTOP ('EXAMPLE 4',.017)
C
  106   CONTINUE
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


      SUBROUTINE SHADAM (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADAM shades the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS) if and only, relative to edge
C group 3, its area identifier is between 1 and 10.  The package
C SOFTFILL is used to do the shading; the density of the shading
C increases with the value of the area identifier.
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
C If the area identifier for group 3 is in the right range, turn on
C shading.
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.3.AND.IAI(I).GE.1.AND.IAI(I).LE.10) ISH=IAI(I)
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
