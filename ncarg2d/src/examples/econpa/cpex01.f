
        PROGRAM CPEX01
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
        DIMENSION ZDAT(23,14)
C
C Declare an array to hold dense data (5152=4x4x23x14).
C
        DIMENSION ZDNS(5152)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.
C
        DIMENSION IAMA(25000)
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
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,23,23,14,20,20,-136.148,451.834)
C
C Example 1-1 ---------------------------------------------------------
C
C Force PLOTCHAR to use characters of the lowest quality.
C
        CALL PCSETI ('QU - QUALITY FLAG',2)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
C
C Draw the default background.
C
        CALL CPBACK (ZDAT,RWRK,IWRK)
C
C Draw contour lines and labels.
C
        CALL CPCLDR (ZDAT,RWRK,IWRK)
C
C Add the informational label and the high/low labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-1',IAMA,0)
        CALL LABTOP ('EXAMPLE 1-1',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-2 ---------------------------------------------------------
C
C Make PLOTCHAR use medium-quality characters.
C
        CALL PCSETI ('QU - QUALITY FLAG',1)
C
C Turn on the positioning of labels by the penalty scheme.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
C
C Turn on the drawing of the high and low label boxes.
C
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label, another high/low label, or the edge.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',7)
C
C Tell CONPACK not to choose contour levels, so that the ones chosen
C for example 1-1 will be used.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',0)
C
C Increase the line width for labelled levels.
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        DO 101 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
          IF (ICLU.EQ.3) THEN
            CALL CPSETI ('CLL - CONTOUR-LINE LINE WIDTH',2)
          END IF
  101   CONTINUE
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
C
C Draw the default background, using a wider line than normal.
C
        CALL GSLWSC (2.)
        CALL CPBACK (ZDAT,RWRK,IWRK)
        CALL GSLWSC (1.)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,25000)
C
C Put label boxes into the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Draw contour lines, avoiding drawing them through the label boxes.
C
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C Draw all the labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-2',IAMA,25000)
        CALL LABTOP ('EXAMPLE 1-2',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-3 ---------------------------------------------------------
C
C Make PLOTCHAR use high-quality characters.
C
        CALL PCSETI ('QU - QUALITY FLAG',0)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label or another high/low label, but to move those which overlap the
C edge inward a little.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
C
C Turn off the area identifiers for all except the zero contour and set
C its identifiers in such a way that we can shade the areas "below" that
C contour.
C
        DO 102 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETR ('CLV - CONTOUR LEVEL VALUE',CLEV)
          IF (CLEV.NE.0.) THEN
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',0)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',0)
          ELSE
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',2)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',1)
          END IF
  102   CONTINUE
C
C Draw the contour plot, using the same calls as for example 1-2.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
        CALL GSLWSC (2.)
        CALL CPBACK (ZDAT,RWRK,IWRK)
        CALL GSLWSC (1.)
        CALL ARINAM (IAMA,25000)
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Now, add the zero contour line to the area map.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Scan the area map.  The routine SHADER will be called to shade the
C areas below the zero contour line.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-3',IAMA,25000)
        CALL LABTOP ('EXAMPLE 1-3',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-4 ---------------------------------------------------------
C
C Turn on the 2D smoother.
C
        CALL CPSETR ('T2D - TENSION ON THE 2D SPLINES',1.)
C
C Draw the contour plot, using the same calls as for example 1-3.
C
        CALL CPRECT (ZDAT,23,23,14,RWRK,5000,IWRK,1000)
        CALL GSLWSC (2.)
        CALL CPBACK (ZDAT,RWRK,IWRK)
        CALL GSLWSC (1.)
        CALL ARINAM (IAMA,25000)
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDAT,RWRK,IWRK)
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-4',IAMA,25000)
        CALL LABTOP ('EXAMPLE 1-4',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-5 ---------------------------------------------------------
C
C Make CONPACK set up the contour levels again (the range of the data
C may be increased by 3D interpolation), but force it to use the same
C contour interval and label interval that were used for the first four
C plots.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',16)
        CALL CPGETR ('CIU - CONTOUR INTERVAL USED',CINU)
        CALL CPSETR ('CIS - CONTOUR INTERVAL SPECIFIER',CINU)
        CALL CPGETI ('LIU - LABEL INTERVAL USED',LINU)
        CALL CPSETI ('LIS - LABEL INTERVAL SPECIFIER',LINU)
C
C Provide more room for storing coordinates used to trace contour
C lines.  The default is slightly too small to hold a complete line,
C and this causes some lines to have a couple of labels right next to
C one another.
C
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOUR TRACING',200)
C
C Turn off the 2D smoother.
C
        CALL CPSETR ('T2D - TENSION ON THE 2D SPLINES',0.)
C
C Initialize the drawing of the contour plot.
C
        CALL CPSPRS (ZDAT,23,23,14,RWRK,5000,IWRK,1000,ZDNS,5152)
C
C Force the selection of contour levels and tweak associated parameters.
C
        CALL CPPKCL (ZDNS,RWRK,IWRK)
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        DO 103 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETI ('CLU - CONTOUR LEVEL USE FLAG',ICLU)
          IF (ICLU.EQ.3) THEN
            CALL CPSETI ('CLL - CONTOUR LINE LINE WIDTH',2)
          END IF
          CALL CPGETR ('CLV - CONTOUR LEVEL VALUE',CLEV)
          IF (CLEV.NE.0.) THEN
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',0)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',0)
          ELSE
            CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LINE',2)
            CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LINE',1)
          END IF
  103   CONTINUE
C
C The rest is pretty much the same as for example 1-4, but the array
C ZDNS is used in place of ZDAT.
C
        CALL GSLWSC (2.)
        CALL PERIM (0,0,0,0)
        CALL GSLWSC (1.)
        CALL ARINAM (IAMA,25000)
        CALL CPLBAM (ZDNS,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDNS,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDNS,RWRK,IWRK)
        CALL CPCLAM (ZDNS,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-5',IAMA,25000)
        CALL LABTOP ('EXAMPLE 1-5',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C Example 1-6 ---------------------------------------------------------
C
C Turn off the selection of contour levels, so that the set picked for
C example 1-5 will be used.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION FLAG',0)
C
C Draw an EZMAP background.  The perimeter and the grid are turned off.
C The "political + U.S. states" dataset is used and it is dotted.  We
C use a satellite-view projection, centered over the U.S., showing
C maximal area.
C
        CALL MAPSTI ('PE - PERIMETER',0)
        CALL MAPSTI ('GR - GRID',0)
        CALL MAPSTC ('OU - OUTLINE DATASET','PS')
        CALL MAPSTI ('DO - DOTTING OF OUTLINES',1)
        CALL MAPSTR ('SA - SATELLITE HEIGHT',1.13)
        CALL MAPROJ ('SV - SATELLITE-VIEW',40.,-95.,0.)
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
        CALL MAPDRW
C
C Tell CONPACK that the SET call has been done, force it to generate X
C coordinates that are longitudes and Y coordinates that are latitudes,
C turn on mapping to an EZMAP background, define the out-of-range value
C (returned by MAPTRN for an unprojectable point), and put the
C informational label in a different place.
C
        CALL CPSETI ('SET - DO SET-CALL FLAG',0)
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',-130.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',-60.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',10.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',70.)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
        CALL CPSETI ('ILP - INFORMATIONAL LABEL POSITIONING',3)
        CALL CPSETR ('ILX - INFORMATIONAL LABEL X POSITION',.5)
        CALL CPSETR ('ILY - INFORMATIONAL LABEL Y POSITION',-.02)
C
C The rest of the calls are just as in example 1-5, except that the
C perimeter is not drawn.
C
        CALL CPSPRS (ZDAT,23,23,14,RWRK,5000,IWRK,1000,ZDNS,5152)
        CALL ARINAM (IAMA,25000)
        CALL CPLBAM (ZDNS,RWRK,IWRK,IAMA)
        CALL CPCLDM (ZDNS,RWRK,IWRK,IAMA,DRAWCL)
        CALL CPLBDR (ZDNS,RWRK,IWRK)
        CALL CPCLAM (ZDNS,RWRK,IWRK,IAMA)
        CALL ARSCAM (IAMA,XCRA,YCRA,1000,IARA,IGRA,10,SHADER)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line around the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 1-6',IAMA,25000)
        CALL LABTOP ('EXAMPLE 1-6',.017)
        CALL BNDARY
C
C Advance the frame.
C
        CALL FRAME
C
C ---------------------------------------------------------------------
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
