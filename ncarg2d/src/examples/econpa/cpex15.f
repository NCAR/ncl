
      PROGRAM CPEX15
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
C This program demonstrates how user-supplied versions of the "user
C callback" routines CPCHHL and CPCHLL may be used to change various
C aspects of individual high/low labels and contour-line labels on a
C contour plot.  In particular, it shows how to prevent such labels
C from appearing in a portion of the plotter frame where contour lines
C are suppressed (by masking against the contents of an area map).
C
C Declare an array to hold the data to be contoured.
C
        DIMENSION ZDAT(70,70)
C
C Declare the required real and integer workspaces.
C
        DIMENSION RWRK(5000),IWRK(1000)
C
C Declare an array to hold an area map.  Put in a common block so we
C can get at it from the routines CPCHHL and CPCHLL.
C
        COMMON /USRAMA/ IAMA(200000)
C
C Declare the arrays needed by ARSCAM and MAPGRM for x/y coordinates.
C
        DIMENSION XCRA(10000),YCRA(10000)
C
C Declare the arrays needed by ARSCAM and MAPGRM for area and group
C identifiers.
C
        DIMENSION IARA(10),IGRA(10)
C
C Declare the routine that draws contour lines, avoiding labels.
C
        EXTERNAL DRAWCL
C
C Declare the routine that does the shading of a contour band.
C
        EXTERNAL SHADER
C
C Declare the routine that fills the EZMAP background.
C
        EXTERNAL FILLEB
C
C Open GKS.
C
        CALL GOPKS (IERRF,ISZDM)
        CALL GOPWK (IWKID,LUNIT,IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Define colors to use for various purposes (0 = black, for the
C background; 1 = white, for the foreground; 2 = light blue, for
C labels; 3 = light yellow, for labels; 4 = light red, for labels;
C 5 = white, for land areas; and 6 = gray, for ocean areas).
C
        CALL GSCR   (IWKID,0,0.,0.,0.)
        CALL GSCR   (IWKID,1,1.,1.,1.)
        CALL GSCR   (IWKID,2,.5,.5,1.)
        CALL GSCR   (IWKID,3,1.,1.,.5)
        CALL GSCR   (IWKID,4,1.,.5,.5)
        CALL GSCR   (IWKID,5,1.,1.,1.)
        CALL GSCR   (IWKID,6,.6,.6,.6)
C
C Generate an array of test data.
C
        CALL GENDAT (ZDAT,70,70,70,21,21,-1.,1.)
C
C Put some labels at the top of the plot.
C
        CALL PLCHHQ (.5,.982,'CONPACK EXAMPLE 15',.018,0.,0.)
C
        CALL PLCHHQ (.5,.952,'The routines CPCHHL and CPCHLL are used be
     +low to suppress labels over land.',.012,0.,0.)
C
        CALL PLCHHQ (.5,.928,'They are also used to modify colors and li
     +ne widths used for the labels.',.012,0.,0.)
C
C Set EZMAP parameters.
C
C       labels (none):
        CALL MAPSTI ('LA - LABELS',0)
C       perimeter (none):
        CALL MAPSTI ('PE - PERIMETER',0)
C       map position:
        CALL MAPPOS (.05,.95,.01,.91)
C       projection:
        CALL MAPROJ ('OR - ORTHOGRAPHIC',40.,-135.,0.)
C       portion of the map to be used:
        CALL MAPSET ('MA - MAXIMAL AREA',0.,0.,0.,0.)
C
C Initialize EZMAP.
C
        CALL MAPINT
C
C Tell CONPACK to do no SET call (EZMAP has done it).
C
        CALL CPSETI ('SET - DO-SET-CALL FLAG',0)
C
C Tell CONPACK to use more contour levels.
C
        CALL CPSETI ('CLS - CONTOUR LEVEL SELECTOR',32)
C
C Tell CONPACK to position labels using the "regular" scheme.
C
        CALL CPSETI ('LLP - LINE LABEL POSITIONING',2)
C
C Tweak constants so as to get more labels on each labeled contour.
C
        CALL CPSETR ('RC1 - REGULAR SCHEME CONSTANT 1',.05)
        CALL CPSETR ('RC2 - REGULAR SCHEME CONSTANT 2',.1)
        CALL CPSETR ('RC3 - REGULAR SCHEME CONSTANT 3',0.)
C
C Provide a little more workspace for X and Y coordinates defining
C contour lines, so as not to have labels right next to each other
C on a contour line.
C
        CALL CPSETI ('RWC - REAL WORKSPACE FOR CONTOURS',200)
C
C Turn on drawing and filling of the high and low label boxes.
C
        CALL CPSETI ('HLB - HIGH/LOW LABEL BOX FLAG',3)
C
C Tell CONPACK to delete high/low labels which overlap the informational
C label or another high/low label, but to move those which overlap the
C edge inward a little.
C
        CALL CPSETI ('HLO - HIGH/LOW LABEL OVERLAP FLAG',11)
C
C Turn on drawing and filling of the contour line label boxes.
C
        CALL CPSETI ('LLB - LINE LABEL BOX FLAG',3)
C
C Make all CONPACK-written characters a little smaller.
C
        CALL CPSETR ('CWM - CHARACTER WIDTH MULTIPLIER',.8)
C
C Turn on the drawing of the grid edge ("contour line number -1") and
C thicken it somewhat.
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',-1)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',1)
        CALL CPSETR ('CLL - CONTOUR LEVEL LINE WIDTH',2.)
C
C Tell CONPACK to use EZMAP for mapping and what the out-of-range
C signal is.
C
        CALL CPSETI ('MAP - MAPPING FUNCTION',1)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
C
C Tell CONPACK what range of coordinates to use.
C
        CALL CPSETR ('XC1 - LONGITUDE AT I = 1',-230.)
        CALL CPSETR ('XCM - LONGITUDE AT I = M',- 40.)
        CALL CPSETR ('YC1 - LATITUDE AT J = 1' ,- 35.)
        CALL CPSETR ('YCN - LATITUDE AT J = N' ,  75.)
C
C Initialize the drawing of the contour plot.
C
        CALL CPRECT (ZDAT,70,70,70,RWRK,5000,IWRK,1000)
C
C Force the selection of contour levels so that associated quantities
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
  101   CONTINUE
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
C
        CALL CPSETI ('PAI - PARAMETER ARRAY INDEX',NCLV)
        CALL CPSETR ('CLV - CONTOUR LEVEL VALUE',+.15)
        CALL CPSETI ('CLU - CONTOUR LEVEL USE FLAG',0)
        CALL CPSETI ('AIA - AREA IDENTIFIER ABOVE LEVEL',3)
        CALL CPSETI ('AIB - AREA IDENTIFIER BELOW LEVEL',1)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,200000)
C
C Put EZMAP boundary lines into the area map (in edge group 1).
C
        CALL MAPBLA (IAMA)
C
C Put label boxes into the area map (in edge group 3).  One of the first
C things this routine does is generate a list of labels (high/low and
C contour line labels).  For each such label, one of the routines CPCHHL
C or CPCHLL is called, giving the user an opportunity to suppress the
C positioning of a label there.  The versions of these routines supplied
C later in this file use the contents of the area map array IAMA to
C suppress labels that are over land areas.
C
        CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Fill land and ocean areas in different shades, avoiding label boxes.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,10000,IARA,IGRA,10,FILLEB)
C
C Set the polyline color index to zero, so that lines drawn from this
C point on will be drawn in black over the filled background.
C
        CALL GSPLCI (0)
C
C Draw the EZMAP grid lines (lines of constant latitude and longitude)
C over the oceans.
C
        CALL MAPGRM (IAMA,XCRA,YCRA,10000,IARA,IGRA,10,DRAWCL)
C
C Put the contour lines at contour levels -.15 and +.15 into the area
C map.
C
        CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
C
C Cross-hatch the area between contour levels -.15 and +.15.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,10000,IARA,IGRA,10,SHADER)
C
C Draw contour lines over the oceans.
C
        CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,DRAWCL)
C
C Draw labels.  Because the versions of the routines CPCHHL and CPCHLL
C supplied later in this file are used instead of the default ones in
C CONPACK, the appearance of the labels is changed in various ways.
C See the commenting in those routines for further information.
C
        CALL CPLBDR (ZDAT,RWRK,IWRK)
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
C This subroutine is used to draw EZMAP grid lines and contour lines
C over the ocean only.  It draws the polyline defined by the points
C ((XCS(I),YCS(I)),I=1,NCS) if and only if none of the area identifiers
C for the area containing the polyline are negative and it's over ocean.
C
C The dash package routine CURVED is called to do the drawing so that
C the grid lines will be properly dashed as specified by internal
C parameters of EZMAP.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Find the area identifiers of the polyline relative to group 1 (EZMAP
C background) and group 3 (CONPACK-supplied edges).
C
        IA1=-1
        IA3=-1
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.1) IA1=IAI(I)
          IF (IAG(I).EQ.3) IA3=IAI(I)
  101   CONTINUE
C
C Draw the polyline if and only if neither area identifier is negative
C and it's over the ocean.
C
        IF (IA1.GE.0.AND.IA3.GE.0.AND.MAPACI(IA1).EQ.1)
     +                                         CALL CURVED (XCS,YCS,NCS)
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE FILLEB (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of FILLEB fills the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS-1) if it's a land area (in which
C case color index 6 is used) or an ocean area (in which case color
C index 7 is used), but it avoids filling areas for which, relative
C to edge group 3, the area identifier is negative (CONPACK label
C boxes).  The GKS routine GFA is used to do the fills.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Find the area identifiers of the polygon relative to group 1 (EZMAP
C background) and group 3 (CONPACK-supplied edges).
C
        IA1=-1
        IA3=-1
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.1) IA1=IAI(I)
          IF (IAG(I).EQ.3) IA3=IAI(I)
  101   CONTINUE
C
C Fill land areas in white, using GFA and color index 6.
C
       IF (IA1.GT.0.AND.MAPACI(IA1).EQ.2.AND.IA3.GE.0) THEN
         CALL GSFACI (5)
         CALL GFA    (NCS-1,XCS,YCS)
       END IF
C
C Fill ocean areas in gray, using GFA and color index 7.
C
       IF (IA1.GT.0.AND.MAPACI(IA1).EQ.1.AND.IA3.GE.0) THEN
         CALL GSFACI (6)
         CALL GFA    (NCS-1,XCS,YCS)
       END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE SHADER (XCS,YCS,NCS,IAI,IAG,NAI)
C
C This version of SHADER shades the polygon whose edge is defined by
C the points ((XCS(I),YCS(I)),I=1,NCS-1) if and only if it's over ocean
C and, relative to edge group 3, its area identifier is a 1 (the area
C between contours at levels -.15 and .15).  The package SOFTFILL is
C used to do the shading.
C
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C Define workspaces for the shading routine.
C
        DIMENSION DST(2200),IND(2400)
C
C Find the area identifiers of the polygon relative to group 1 (EZMAP
C background) and group 3 (CONPACK-supplied edges).
C
        IA1=-1
        IA3=-1
C
        DO 101 I=1,NAI
          IF (IAG(I).EQ.1) IA1=IAI(I)
          IF (IAG(I).EQ.3) IA3=IAI(I)
  101   CONTINUE
C
C If appropriate, crosshatch the area.
C
        IF (IA1.GT.0.AND.MAPACI(IA1).EQ.1.AND.IA3.EQ.1) THEN
          CALL SFSETI ('ANGLE',45)
          CALL SFSETR ('SPACING',.003)
          CALL SFWRLD (XCS,YCS,NCS-1,DST,2200,IND,2400)
          CALL SFSETI ('ANGLE',135)
          CALL SFNORM (XCS,YCS,NCS-1,DST,2200,IND,2400)
        END IF
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE GENDAT (DATA,IDIM,M,N,MLOW,MHGH,DLOW,DHGH)
C
C This is a routine to generate test data for two-dimensional graphics
C routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
C the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
C of data having approximately "MLOW" lows and "MHGH" highs, a minimum
C value of exactly "DLOW" and a maximum value of exactly "DHGH".
C
C "MLOW" and "MHGH" are each forced to be greater than or equal to 1
C and less than or equal to 25.
C
C The function used is a sum of exponentials.
C
        DIMENSION DATA(IDIM,1),CCNT(3,50)
C
        FOVM=9./REAL(M)
        FOVN=9./REAL(N)
C
        NLOW=MAX(1,MIN(25,MLOW))
        NHGH=MAX(1,MIN(25,MHGH))
        NCNT=NLOW+NHGH
C
        DO 101 K=1,NCNT
          CCNT(1,K)=1.+(REAL(M)-1.)*FRAN()
          CCNT(2,K)=1.+(REAL(N)-1.)*FRAN()
          IF (K.LE.NLOW) THEN
            CCNT(3,K)=-1.
          ELSE
            CCNT(3,K)=+1.
          END IF
  101   CONTINUE
C
        DMIN=+1.E36
        DMAX=-1.E36
        DO 104 J=1,N
          DO 103 I=1,M
            DATA(I,J)=.5*(DLOW+DHGH)
            DO 102 K=1,NCNT
              TEMP=-((FOVM*(REAL(I)-CCNT(1,K)))**2+
     +               (FOVN*(REAL(J)-CCNT(2,K)))**2)
              IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     +            .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
  102       CONTINUE
            DMIN=MIN(DMIN,DATA(I,J))
            DMAX=MAX(DMAX,DATA(I,J))
  103     CONTINUE
  104   CONTINUE
C
        DO 106 J=1,N
          DO 105 I=1,M
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
  105     CONTINUE
  106   CONTINUE
C
        RETURN
C
      END



      FUNCTION FRAN ()
C
C This routine generates "random" numbers for GENDAT.
C
        DIMENSION RSEQ (100)
C
        SAVE ISEQ
C
        DATA RSEQ / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     +              .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     +              .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     +              .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     +              .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     +              .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     +              .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     +              .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     +              .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     +              .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
C
        DATA ISEQ / 0 /
C
        ISEQ=MOD(ISEQ,100)+1
        FRAN=RSEQ(ISEQ)
C
        RETURN
C
      END



      SUBROUTINE CPCHHL (IFLG)
C
C This routine is called just before and just after each action
C involving a high/low label.  A user version may take action to change
C the label.  This version also looks to see if the label is in an
C allowed position and, if not, blanks it out.
C
C IFLG is positive if an action is about to be taken, negative if the
C action has just been completed.  The action in question is implied
C by the absolute value of IFLG, as follows:
C
C   1 - deciding whether to put a high label at a given point
C   2 - filling the box around the label for a high
C   3 - drawing the label for a high
C   4 - outlining the box around the label for a high
C   5 - deciding whether to put a low label at a given point
C   6 - filling the box around the label for a low
C   7 - drawing the label for a low
C   8 - outlining the box around the label for a low
C
C CPCHHL may retrieve the value of the internal parameter 'ZDV', which
C is the value associated with the high or low being labelled.
C
C CPCHHL may retrieve the values of the internal parameters 'LBX' and
C 'LBY', which are the coordinates of the center point of the label,
C in the current user coordinate system.
C
C When IFLG is 1, 3, 5, or 7, CPCHHL is permitted to change the value
C of the internal parameter 'CTM' (a character string); if IFLG is 1 or
C 5 and 'CTM' is made blank, the label is suppressed; otherwise, the
C new value of 'CTM' will replace whatever CONPACK was about to use.
C If this is done for either IFLG = 1 or IFLG = 3, it must be done for
C both, and the same replacement label must be supplied in both cases.
C Similarly, if it is done for either IFLG = 5 or IFLG = 7, it must be
C done for both, and the same replacement label must be specified in
C both cases.
C
C When IFLG = 2, 3, 4, 6, 7, or 8, CPCHHL may make GKS calls to change
C color or line width; during the following call with IFLG = -2, -3,
C -4, -6, -7, or -8, such changes should be undone.
C
C Declare the common block containing the area map array that will be
C used in deciding where labels ought to be suppressed.
C
        COMMON /USRAMA/ IAMA(200000)
C
        DIMENSION IAAI(10),IAGI(10)
C
C Define quantities that will be used to generate the coordinates of
C five points to be tested in making the decision whether a label is
C over land or water.
C
        DIMENSION XSTP(5),YSTP(5)
C
        DATA XSTP / 0., -.01, .01,   0.,  0. /
        DATA YSTP / 0.,   0.,  0., -.01, .01 /
C
C If IFLG = 1, we have to decide whether we want a label at the point
C ('LBX','LBY') or not, and, if not, reset the value of 'CTM' to a
C single blank to signal that fact to the calling routine.  The decision
C is made by looking at an area map previously created in the array
C IAMA to see if the label point is over land or water.  We actually
C test the point itself and four other points around it; it any of
C the five is over land, we suppress the label (by setting 'CTM'=' ').
C
        IF (IFLG.EQ.1.OR.IFLG.EQ.5) THEN
          CALL CPGETR ('LBX',RLBX)
          CALL CPGETR ('LBY',RLBY)
          FLBX=CUFX(RLBX)
          FLBY=CUFY(RLBY)
          DO 102 I=1,5
            CALL ARGTAI (IAMA,CFUX(FLBX+XSTP(I)),CFUY(FLBY+YSTP(I)),
     +                                          IAAI,IAGI,10,NIDS,1)
            IAID=-1
            DO 101 J=1,NIDS
              IF (IAGI(J).EQ.1) IAID=IAAI(J)
  101       CONTINUE
            IF (MAPACI(IAID).EQ.2) THEN
              CALL CPSETC ('CTM - CHARACTER TEMPORARY',' ')
              RETURN
            END IF
  102     CONTINUE
        END IF
C
C Now, if the label box is being filled, make the fill color depend
C on whether the label is for a high or a low.
C
        IF (ABS(IFLG).EQ.2.OR.ABS(IFLG).EQ.6) THEN
          IF (IFLG.GT.0) THEN
            IF (IFLG.EQ.2) THEN
              CALL GSFACI (4)
            ELSE
              CALL GSFACI (2)
            END IF
          ELSE
            CALL GSFACI (1)
          END IF
          RETURN
        END IF
C
C Put the text on the filled background in a contrasting color.
C
        IF (ABS(IFLG).EQ.3.OR.ABS(IFLG).EQ.7) THEN
          IF (IFLG.GT.0) THEN
            IF (IFLG.EQ.3) THEN
              CALL PCSETI ('CC', 0)
            ELSE
              CALL PCSETI ('CC', 1)
            END IF
          ELSE
            CALL PCSETI ('CC',-1)
          END IF
          RETURN
        END IF
C
C If the box is being outlined, do it in a contrasting color and widen
C the lines.
C
        IF (ABS(IFLG).EQ.4.OR.ABS(IFLG).EQ.8) THEN
          IF (IFLG.GT.0) THEN
            IF (IFLG.EQ.4) THEN
C             CALL GSPLCI (2)
              CALL GSPLCI (0)
            ELSE
C             CALL GSPLCI (4)
              CALL GSPLCI (0)
            END IF
            CALL GSLWSC (2.)
          ELSE
            CALL GSPLCI (1)
            CALL GSLWSC (1.)
          END IF
          RETURN
        END IF
C
C In all other cases, just return.
C
        RETURN
C
      END



      SUBROUTINE CPCHLL (IFLG)
C
C This routine is called just before and just after each action
C involving a contour line label.  A user version may take action to
C change the label.  This version also looks to see if the label is
C in an allowed position and, if not, blanks it out.
C
C IFLG is positive if an action is about to be taken, negative if an
C action has just been completed.  The action in question is implied
C by the absolute value of IFLG, as follows:
C
C   1 - deciding whether to put a line label at a given point
C   2 - filling the box around a line label
C   3 - drawing a line label
C   4 - outlining the box around a line label
C
C When CPCHLL is called, the internal parameter 'PAI' will have been
C set to the index of the appropriate contour level.  Thus, parameters
C associated with that level may easily be retrieved by calls to CPGETx.
C
C CPCHLL may retrieve the value of the internal parameter 'ZDV', which
C is the contour level associated with the contour line being labelled.
C
C CPCHLL may retrieve the values of the internal parameters 'LBX' and
C 'LBY', which are the coordinates of the center point of the label,
C in the current user coordinate system.
C
C When IFLG is 1 or 3, CPCHLL is permitted to change the value of the
C internal parameter 'CTM' (a character string); if IFLG is 1 and 'CTM'
C is made blank, the label is suppressed; otherwise, the new value of
C 'CTM' will replace whatever CONPACK was about to use.  If this is
C done for either IFLG = 1 or IFLG = 3, it must be done for both, and
C the same replacement label must be supplied in both cases.
C
C When IFLG = 2, 3, or 4, CPCHLL may make GKS calls to change color
C or line width; during the following call with IFLG = -2, -3, or -4,
C such changes should be undone.
C
C Declare the common block containing the area map array that will be
C used in deciding where labels ought to be suppressed.
C
        COMMON /USRAMA/ IAMA(200000)
C
        DIMENSION IAAI(10),IAGI(10)
C
C Define quantities that will be used to generate the coordinates of
C five points to be tested in making the decision whether a label is
C over land or water.
C
        DIMENSION XSTP(5),YSTP(5)
C
        DATA XSTP / 0., -.01, .01,   0.,  0. /
        DATA YSTP / 0.,   0.,  0., -.01, .01 /
C
C If IFLG = 1, we have to decide whether we want a label at the point
C ('LBX','LBY') or not, and, if not, reset the value of 'CTM' to a
C single blank to signal that fact to the calling routine.  The decision
C is made by looking at an area map previously created in the array
C IAMA to see if the label point is over land or water.  We actually
C test the point itself and four other points around it; it any of
C the five is over land, we suppress the label (by setting the value
C of 'CTM' to ' ').
C
        IF (IFLG.EQ.1) THEN
          CALL CPGETR ('LBX',RLBX)
          CALL CPGETR ('LBY',RLBY)
          FLBX=CUFX(RLBX)
          FLBY=CUFY(RLBY)
          DO 102 I=1,5
            CALL ARGTAI (IAMA,CFUX(FLBX+XSTP(I)),CFUY(FLBY+YSTP(I)),
     +                                          IAAI,IAGI,10,NIDS,1)
            IAID=-1
            DO 101 J=1,NIDS
              IF (IAGI(J).EQ.1) IAID=IAAI(J)
  101       CONTINUE
            IF (MAPACI(IAID).EQ.2) THEN
              CALL CPSETC ('CTM - CHARACTER TEMPORARY',' ')
              RETURN
            END IF
  102     CONTINUE
        END IF
C
C Otherwise, see what the contour value is on the line being labelled
C and, if it's zero, reset the value of 'CTM' to 'Z' so that will be
C used as the label.
C
        CALL CPGETR ('ZDV - Z DATA VALUE',CLEV)
        IF (CLEV.EQ.0.) THEN
          CALL CPSETC ('CTM - CHARACTER TEMPORARY','Z')
        END IF
C
C Now, if the label box is being filled, make the fill color depend
C on the contour level.
C
        IF (ABS(IFLG).EQ.2) THEN
          IF (IFLG.GT.0) THEN
            IF (CLEV.LT.0.) THEN
              CALL GSFACI (2)
            ELSE IF (CLEV.EQ.0.) THEN
              CALL GSFACI (3)
            ELSE
              CALL GSFACI (4)
            END IF
          ELSE
            CALL GSFACI (1)
          END IF
          RETURN
        END IF
C
C Put the text on the filled background in a contrasting color.
C
        IF (ABS(IFLG).EQ.3) THEN
          IF (IFLG.GT.0) THEN
            IF (CLEV.LT.0.) THEN
              CALL PCSETI ('CC', 1)
            ELSE IF (CLEV.EQ.0.) THEN
              CALL PCSETI ('CC', 0)
            ELSE
              CALL PCSETI ('CC', 0)
            END IF
          ELSE
            CALL PCSETI ('CC',-1)
          END IF
          RETURN
        END IF
C
C If the box is being outlined, do it in a contrasting color and widen
C the lines.
C
        IF (ABS(IFLG).EQ.4) THEN
          IF (IFLG.GT.0) THEN
            IF (CLEV.LT.0.) THEN
C             CALL GSPLCI (4)
              CALL GSPLCI (0)
            ELSE IF (CLEV.EQ.0.) THEN
              CALL GSPLCI (0)
            ELSE
C             CALL GSPLCI (2)
              CALL GSPLCI (0)
            END IF
            CALL GSLWSC (2.)
          ELSE
            CALL GSPLCI (1)
            CALL GSLWSC (1.)
          END IF
          RETURN
        END IF
C
C In all other cases, just return.
C
        RETURN
C
      END
