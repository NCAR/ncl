
        PROGRAM CPEX06
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
C Declare required data arrays and workspace arrays.
C
        DIMENSION ZDAT(27,23),RWRK(5000),IWRK(1000),IAMA(10000)
        DIMENSION IASF(13)
C
C Declare the routine which will draw contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Define an array for GKS aspect source flags.
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
C Set all aspect source flags to "individual".
C
        CALL GSASF (IASF)
C
C Define color indices.
C
        CALL DFCLRS(IWKID)
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,27,27,23,25,25,-362.362E11,451.834E11)
C
C Increase the approximate number of contour levels used.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTION',25)
C
C Turn on the positioning of labels by the penalty scheme.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',3)
C
C Label highs and low with just the number, boxed and colored green.
C
        CALL CPSETC ('HLT - HIGH/LOW TEXT','$ZDV$')
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',1)
        CALL CPSETI ('HLC - HIGH/LOW LABEL COLOR INDEX',9)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label, another high/low label, or the edge.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',7)
C
C Turn on the drawing of the grid edge ("contour line number -1"),
C thicken it somewhat, and make it white.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
        CALL CPSETI ('CLC - CONTOUR LINE COLOR',1)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,27,27,23,RWRK,5000,IWRK,1000)
C
C Force the selection of contour levels by CONPACK.
C
        CALL CPPKCL (ZDAT,RWRK,IWRK)
C
C Force the color of the negative contours to blue, the color of the
C positive contours to red, and the color of the zero contour to white.
C If a positive or negative contour is labelled, use a darker shade and
C make the color of the label match.
C
        CALL CPGETI ('NCL - NUMBER OF CONTOUR LEVELS',NCLV)
C
        DO 102 ICLV=1,NCLV
          CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',ICLV)
          CALL CPGETR ('CLV - CONTOUR LEVEL',CLEV)
          CALL CPGETI ('CLU - CONTOUR LEVEL USE',ICLU)
          IF (CLEV.LT.0.) THEN
            IF (ICLU.EQ.1) THEN
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',7)
            ELSE
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',6)
              CALL CPSETI ('LLC - LINE LABEL COLOR',6)
            END IF
          ELSE IF (CLEV.EQ.0.) THEN
            CALL CPSETI ('CLC - CONTOUR LINE COLOR',1)
            CALL CPSETI ('LLC - LINE LABEL COLOR',1)
          ELSE
            IF (ICLU.EQ.1) THEN
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',13)
            ELSE
              CALL CPSETI ('CLC - CONTOUR LINE COLOR',14)
              CALL CPSETI ('LLC - LINE LABEL COLOR',14)
            END IF
          END IF
  102   CONTINUE
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,10000)
C
C Put label boxes in the area map.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Draw contour lines, avoiding drawing through label boxes.
C
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C Fill in the labels.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
C
C Compute and print statistics for the plot, label it, and put a
C boundary line at the edge of the plotter frame.
C
        CALL CAPSAP ('EXAMPLE 6',IAMA,10000)
        CALL LABTOP ('EXAMPLE 6',.017)
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
