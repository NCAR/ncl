C
C       $Id: cmpita.f,v 1.2 1992-10-02 22:45:04 ncargd Exp $
C
C
C Declare fill routine external, or crash program
C
        EXTERNAL FILL

        PARAMETER(IGRD=15,IMAP=220000,NWRK=15000)
        PARAMETER(M=180/IGRD,N=360/IGRD)

        COMMON /DAT1/ ZDAT(M,N)

        INTEGER MAP(IMAP), IGRP(5), IAREA(5), ISPACE
        REAL XWRK(NWRK),YWRK(NWRK),ZDAT
        REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

        DATA PLIM1 /0.,0./
        DATA PLIM2 /0.,0./
        DATA PLIM3 /0.,0./
        DATA PLIM4 /0.,0./
C
C Print out a warning about how time consuming this example is
C
	WRITE (6,*) ' WARNING: This example may take 20 minutes'
	WRITE (6,*) '          to execute on some machines.'
C
C Generate some data to base color fill on
C
        CALL GENDAT(ZDAT,M,M,N,1.,15.,1.,15.)
C
C Make sure that data at -180 is the same as data at +180
C
	DO 10, I=1,M
	   ZDAT(I,N) = ZDAT (I,1)
 10	CONTINUE
C
C Open GKS.
C
        CALL OPNGKS
C
C Set up color table
C
        CALL COLOR
C
C Set the outline-dataset parameter.
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
C
C Set the projection-type parameters.
C
      CALL MAPROJ ('CE',0.,0.,0.)
C
C Set the limits parameters.
C
      CALL MAPSET ('MA',PLIM1,PLIM2,PLIM3,PLIM4)
C
C Initalize areas, initialize ezmap
C
        CALL ARINAM (MAP, IMAP)
        CALL MAPINT
C
C Add geographic outlines to area map
C
        CALL MAPBLA (MAP)

C
C Add longitude lines at 2 degree intervals over the states to area map
C
        DO 1, I=-90,90,IGRD
          DO 2 J=-180, 180-IGRD,IGRD
            LEFT  = (J+181)*1000 + (I+91)
            CALL MAPITA(REAL(I),REAL(J)     ,0,MAP,5,LEFT,0)
            CALL MAPITA(REAL(I),REAL(J+IGRD),1,MAP,5,LEFT,0)
            CALL MAPIQA(MAP,5,LEFT,0)
 2        CONTINUE
 1      CONTINUE

C
C Add latitude lines at 2 degree intervals over the states to area map
C
        DO 3, I=-180, 180,IGRD
          DO 4 J=-90,90-IGRD,IGRD
            CALL MAPITA(REAL(J),     REAL(I),0,MAP,5,0,0)
            CALL MAPITA(REAL(J+IGRD),REAL(I),1,MAP,5,0,0)
            CALL MAPIQA(MAP,5,0,0)
 4        CONTINUE
 3      CONTINUE
C
C Fill in areas over land with colors
C
        CALL GSFAIS(1)
        CALL ARSCAM(MAP,XWRK,YWRK,NWRK,IAREA,IGRP,5,FILL)
C
C Draw perimeter
C
        CALL MAPSTI('LA - LABEL FLAG',0)
        CALL MAPLBL
C
C Draw map over area plot
C
        CALL MAPLOT
C
C Report how much space was used in the area map
C
        ISPACE=MAP(1) - MAP(6) + MAP(5)
        WRITE (6,*) 'Area Map Workspace Used: ',ISPACE
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ (.5,.85,'Filling Gridded Data over Landmasses',
     +          .017,0.,0.)
C
C Advance the frame.
C
      CALL FRAME
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

        SUBROUTINE FILL(XWRK,YWRK,NWRK,IAREA,IGRP,NSIZE)

        PARAMETER(IGRD=15,M=180/IGRD,N=360/IGRD)

C bring in the data array to define colors
        COMMON /DAT1/ ZDAT(M,N)

        REAL XWRK(NWRK), YWRK(NWRK), ZDAT
        INTEGER IAREA(NSIZE),IGRP(NSIZE)

C
C Group 5 is the group of 2 degree grid lines, group 1 are political and
C continental outlines.
C
        IAREA1=-1
        IAREA5=-1
C
C If there are less than 3 points defining the area, return to arscam
C
        IF (NWRK.LE.3) RETURN
C
C Check each of the group and area identifiers for the current area
C
        DO 10, I=1,NSIZE
           IF (IGRP(I).EQ.1) IAREA1=IAREA(I)
           IF (IGRP(I).EQ.5) IAREA5=IAREA(I)
 10     CONTINUE
C
C If the area identifier is over the globe
C
        IF (IAREA1.GT.0) THEN
C
C If the color id for the area is 1, then the area is over ocean and
C don't fill area
C
          IF (MAPACI(IAREA1).NE.1.AND.IAREA5.GT.0) THEN
C
C At this point you need to invert your area identifier function to 
C retrieve your latitude and longitude values (or your data array
C indicies) so that you can color fill based on them.
C
             LAT = MOD(IAREA5,1000)
             I = LAT/IGRD + 1
             LON = IAREA5/1000
             J = LON/IGRD + 1
C
C Our data is predefined to have values between 1. and 15 (chosen
C because we have 15 colors defined in subroutine COLOR.
C color index 1 is white, so we set the color index to start at 2.
C
             ICLR = INT(ZDAT(I,J))+1
             CALL GSFACI(ICLR)
             CALL GFA(NWRK-1,XWRK,YWRK)
          ENDIF
        ENDIF

        RETURN
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
        FOVM=9./FLOAT(M)
        FOVN=9./FLOAT(N)
C
        NLOW=MAX0(1,MIN0(25,MLOW))
        NHGH=MAX0(1,MIN0(25,MHGH))
        NCNT=NLOW+NHGH
C
        DO 101 K=1,NCNT
          CCNT(1,K)=1.+(FLOAT(M)-1.)*FRAN()
          CCNT(2,K)=1.+(FLOAT(N)-1.)*FRAN()
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
              TEMP=-((FOVM*(FLOAT(I)-CCNT(1,K)))**2+
     +               (FOVN*(FLOAT(J)-CCNT(2,K)))**2)
              IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     +            .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
  102       CONTINUE
            DMIN=AMIN1(DMIN,DATA(I,J))
            DMAX=AMAX1(DMAX,DATA(I,J))
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
        DIMENSION RSEQ (100)
        SAVE ISEQ
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
        DATA ISEQ / 0 /
        ISEQ=MOD(ISEQ,100)+1
        FRAN=RSEQ(ISEQ)
        RETURN
      END
      SUBROUTINE COLOR
C
C     BACKGROUND COLOR
C     BLACK
      CALL GSCR(1,0,0.,0.,0.)
C
C     FORGROUND COLORS
C White
      CALL GSCR(1,  1, 1.0, 1.0, 1.0)
C Red
      CALL GSCR(1,  2, 0.9, 0.25, 0.0)
C OrangeRed
      CALL GSCR(1,  3, 1.0, 0.0, 0.2)
C Orange
      CALL GSCR(1,  4, 1.0, 0.65, 0.0)
C Yellow
      CALL GSCR(1,  5, 1.0, 1.0, 0.0)
C GreenYellow
      CALL GSCR(1,  6, 0.7, 1.0, 0.2)
C Chartreuse
      CALL GSCR(1,  7, 0.5, 1.0, 0.0)
C Celeste
      CALL GSCR(1,  8, 0.2, 1.0, 0.5)
C Green
      CALL GSCR(1,  9, 0.2, 0.8, 0.2)
C Aqua
      CALL GSCR(1, 10, 0.0, 0.9, 1.0)
C DeepSkyBlue
      CALL GSCR(1, 11, 0.0, 0.75, 1.0)
C RoyalBlue
      CALL GSCR(1, 12, 0.25, 0.45, 0.95)
C SlateBlue
      CALL GSCR(1, 13, 0.4, 0.35, 0.8)
C DarkViolet
      CALL GSCR(1, 14, 0.6, 0.0, 0.8)
C Orchid
      CALL GSCR(1, 15, 0.85, 0.45, 0.8)
C Lavender
      CALL GSCR(1, 16, 0.8, 0.8, 1.0)
C Gray
      CALL GSCR(1, 17, 0.7, 0.7, 0.7)
C Done.
C
        RETURN
C
      END

