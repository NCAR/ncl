
        PROGRAM CPEX02
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
        DIMENSION ZDAT(33,33)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(20000)
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
C Declare the routine which does the shading.
C
        EXTERNAL SHADER
C
C Dimension a character variable to hold plot labels.
C
        CHARACTER*11 LABL
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
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
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
C Move the informational label into the lower left-hand corner and
C turn on the box around it, making the box thicker than normal.
C
        CALL CPSETR ('ILX - INFORMATIONAL LABEL X POSITION',.02)
        CALL CPSETR ('ILY - INFORMATIONAL LABEL Y POSITION',.02)
        CALL CPSETI ('ILP - INFORMATIONAL LABEL POSIIONING',-4)
        CALL CPSETI ('ILB - INFORMATIONAL LABEL BOX',1)
        CALL CPSETR ('ILL - INFORMATIONAL LABEL LINE WIDTH',2.)
C
C Change the text of the informational label.
C
        CALL CPSETC ('ILT - INFORMATIONAL LABEL TEXT',
     +               'CONTOUR FROM $CMN$ TO $CMX$ BY $CIU$ (X $SFU$)')
C
C Do four different plots, one in each quadrant.
C
        DO 102 IPLT=1,4
C
C Generate an array of test data.
C
          CALL GENDAT (ZDAT,33,33,33,20,20,.000025,.000075)
C
C Move the viewport to the proper quadrant.
C
          CALL CPSETR ('VPL - VIEWPORT LEFT EDGE',
     +                 .0250+.4875*REAL(MOD(IPLT-1,2)))
          CALL CPSETR ('VPR - VIEWPORT RIGHT EDGE',
     +                 .4875+.4875*REAL(MOD(IPLT-1,2)))
          CALL CPSETR ('VPB - VIEWPORT BOTTOM EDGE',
     +                 .0250+.4875*REAL((4-IPLT)/2))
          CALL CPSETR ('VPT - VIEWPORT TOP EDGE',
     +                 .4875+.4875*REAL((4-IPLT)/2))
C
C Specify how the scale factor is to be selected.
C
          CALL CPSETI ('SFS - SCALE FACTOR SELECTION',-IPLT)
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
          DO 101 ICLV=1,NCLV
            CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
            CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
            IF (ICLU.EQ.3) THEN
              CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',2)
            END IF
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',0)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',0)
  101     CONTINUE
C
C Add two new levels for which no contour lines are to be drawn, but
C between which shading is to be done.
C
          NCLV=NCLV+2
          CALL CPSETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV-1)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',.000045)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',1)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',2)
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV)
          CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',.000055)
          CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
          CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',3)
          CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',1)
C
C Draw the contour plot.
C
          CALL ARINAM (IAMA,20000)
          CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
          CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
          CALL CPLBDR (ZDAT,RWRK,IWRK)
          CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
          CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot and label it.
C
          LABL='EXAMPLE 2-'//CHAR(ICHAR('0')+IPLT)
          CALL CAPSAP (LABL,IAMA,20000)
          CALL LABTOP (LABL,.017)
C
  102   CONTINUE
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
