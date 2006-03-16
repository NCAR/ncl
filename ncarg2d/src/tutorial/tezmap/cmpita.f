
      PROGRAM CMPITA
C
C Declare fill routine external so compiler will know that it's a
C subroutine instead of a real variable.
C
      EXTERNAL FILL
C
C IGRD specifies the number of degrees between each grid line and the
C next.  IMAP specifies the size of the area-map array.  If you make
C IGRD smaller than shown here, use a value which evenly divides both
C 180 and 360 and remember to put the same value in the PARAMETER
C statement in the routine FILL, below; in this case,  a larger value
C of IMAP will be required and the program will run a lot longer.  For
C IGRD=10, try IMAP=160000; for IGRD=5, try IMAP=220000; for IGRD=3,
C try IMAP=390000; for IGRD=2, try IMAP=520000; for IGRD=1, IMAP=1500000
C was not enough (and the program may have been stuck in a loop).
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
C NWRK specifies the sizes of the work arrays XWRK and YWRK, which are
C passed to ARSCAM.
C
      PARAMETER(IGRD=15,IMAP=150000,NWRK=15000)
      PARAMETER(M=180/IGRD,N=360/IGRD)
C
C The global data array ZDAT is put in a common block so that it can be
C accessed from the subroutine FILL.
C
      COMMON /DAT1/ ZDAT(M,N)
C
C Declare the area-map array and the various other arrays required.
C
      INTEGER MAP(IMAP),IGRP(5),IAREA(5)
      REAL XWRK(NWRK),YWRK(NWRK),ZDAT
      REAL PLIM1(2),PLIM2(2),PLIM3(2),PLIM4(2)
C
C Define the contents of the map limits arrays.
C
      DATA PLIM1 /0.,0./
      DATA PLIM2 /0.,0./
      DATA PLIM3 /0.,0./
      DATA PLIM4 /0.,0./
C
C If appropriate, print a warning about how time consuming this
C example can be.
C
      ITMP=IGRD
C
      IF (ITMP.LT.15) THEN
         WRITE (6,*) ' WARNING: This example may take 20 minutes'
         WRITE (6,*) '          to execute on some machines.'
      END IF
C
C Generate some data to base color fill on.  Note that these data are
C unrealistic in that no attempt has been made to achieve continuity
C at the longitudes -180 and +180 or at the poles.
C
      CALL GENDAT(ZDAT,M,M,N,25,25,1.,15.)
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Define the color table.
C
      CALL COLOR(IWKID)
C
C Set the projection-type parameters.
C
      CALL MAPROJ ('CE',0.,0.,0.)
C
C Set the Ezmap "limits" parameters.
C
      CALL MAPSET ('MA',PLIM1,PLIM2,PLIM3,PLIM4)
C
C Change the Ezmap parameter 'MV' to prevent points from being dropped
C when the final outline is drawn.
C
      CALL MPSETI ('MV',1)
C
C Initalize Areas and Ezmap.
C
      CALL ARINAM (MAP,IMAP)
      CALL MAPINT
C
C Add geographic outlines to area map, using only continental outlines.
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','CO')
      CALL MAPBLA (MAP)
C
C Add latitude lines to the area map.
C
      DO 102 I=-90,90,IGRD
         DO 101 J=-180,180-IGRD,IGRD
            IDLFT = (I     +91) * 1000 + (J+181)
            IDRGT = (I-IGRD+91) * 1000 + (J+181)
            IF (I.EQ.-90) IDRGT=0
            IF (I.EQ. 90) IDLFT=0
            CALL MAPITA(REAL(I),REAL(J)     ,0,MAP,5,IDLFT,IDRGT)
            CALL MAPITA(REAL(I),REAL(J+IGRD),1,MAP,5,IDLFT,IDRGT)
            CALL MAPIQA(MAP,5,IDLFT,IDRGT)
 101     CONTINUE
 102  CONTINUE

C
C Add longitude lines to the area map.
C
      DO 104 J=-180,180,IGRD
         DO 103 I=-90,90-IGRD,IGRD
            IDLFT = (I+91) * 1000 + (J-IGRD+181)
            IDRGT = (I+91) * 1000 + (J     +181)
            IF (J.EQ.-180) IDLFT=0
            IF (J.EQ. 180) IDRGT=0
            CALL MAPITA(REAL(I),     REAL(J),0,MAP,5,IDLFT,IDRGT)
            CALL MAPITA(REAL(I+IGRD),REAL(J),1,MAP,5,IDLFT,IDRGT)
            CALL MAPIQA(MAP,5,IDLFT,IDRGT)
 103     CONTINUE
 104  CONTINUE
C
C Fill in areas over land with colors.
C
      CALL GSFAIS(1)
      CALL ARSCAM(MAP,XWRK,YWRK,NWRK,IAREA,IGRP,5,FILL)
C
C Draw the perimeter.
C
      CALL MAPSTI('LA - LABEL FLAG',0)
      CALL MAPLBL
C
C Draw a map over the output from Areas, using political outlines.
C
      CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR','PO')
      CALL MAPLOT
C
C Report how much space was used in the area map.
C
      ITMP=MAP(1)-MAP(6)+MAP(5)+1
      WRITE (6,*) 'Area Map Workspace Used: ',ITMP
C
C Put the label at the top of the plot.
C
      CALL GETSET (VPL,VPR,VPB,VPT,WDL,WDR,WDB,WDT,LOG)
      CALL GSLWSC(2.)
      CALL PLCHHQ (CFUX(.5),CFUY(VPT+.02),
     +     'Filling Gridded Data over Landmasses',.017,0.,0.)
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

      SUBROUTINE FILL(XWRK,YWRK,NWRK,IAREA,IGRP,NSIZE)
C
      PARAMETER(IGRD=15,M=180/IGRD,N=360/IGRD)
C
C Bring in the data array to define colors.
C
      COMMON /DAT1/ ZDAT(M,N)
C
      REAL XWRK(NWRK),YWRK(NWRK),ZDAT
      INTEGER IAREA(NSIZE),IGRP(NSIZE)
C
C Group 5 is the group containing grid lines and group 1 contains
C continental outlines.
C
      IAREA1=-1
      IAREA5=-1
C
C If there are less than 3 points defining the area, return to ARSCAM.
C
      IF (NWRK.LE.3) RETURN
C
C Check each of the group and area identifiers for the current area.
C
      DO 101 I=1,NSIZE
         IF (IGRP(I).EQ.1) IAREA1=IAREA(I)
         IF (IGRP(I).EQ.5) IAREA5=IAREA(I)
 101  CONTINUE
C
C If the area identifier is over the globe ...
C
      IF (IAREA1.GT.0) THEN
C
C If the color id for the area is 1, then the area is over ocean, so
C we don't fill it; otherwise ...
C
         IF (MAPACI(IAREA1).NE.1.AND.IAREA5.GT.0) THEN
C
C At this point you need to invert your area identifier function to 
C retrieve your latitude and longitude values (or your data array
C indices) so that you can color fill based on them.
C
            LAT = IAREA5/1000
            I = LAT/IGRD + 1
C
            LON = MOD(IAREA5,1000)
            J = LON/IGRD + 1
C
C Our data is predefined to have values between 1 and 15 (chosen
C because we have 15 colors defined in subroutine COLOR).  Color
C index 1 is white, so we set the color index to start at 2.
C
            ICLR = INT(ZDAT(I,J))+1
            CALL GSFACI(ICLR)
            CALL GFA(NWRK-1,XWRK,YWRK)
C
         ENDIF
C
      ENDIF
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
 101  CONTINUE
C
      DMIN=+1.E36
      DMAX=-1.E36
      DO 104 J=1,N
         DO 103 I=1,M
            DATA(I,J)=.5*(DLOW+DHGH)
            DO 102 K=1,NCNT
               TEMP=-((FOVM*(REAL(I)-CCNT(1,K)))**2+
     +              (FOVN*(REAL(J)-CCNT(2,K)))**2)
               IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     +              .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
 102        CONTINUE
            DMIN=MIN(DMIN,DATA(I,J))
            DMAX=MAX(DMAX,DATA(I,J))
 103     CONTINUE
 104  CONTINUE
C
      DO 106 J=1,N
         DO 105 I=1,M
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
 105     CONTINUE
 106  CONTINUE
C
      RETURN
C
      END

      FUNCTION FRAN ()
      DIMENSION RSEQ (100)
      SAVE ISEQ
      DATA RSEQ / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     +     .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     +     .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     +     .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     +     .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     +     .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     +     .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     +     .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     +     .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     +     .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
      DATA ISEQ / 0 /
      ISEQ=MOD(ISEQ,100)+1
      FRAN=RSEQ(ISEQ)
      RETURN
      END

      SUBROUTINE COLOR(IWKID)
C
C Background color (black):
C
      CALL GSCR(1,0,0.,0.,0.)
C
C Foreground colors
C
C White
C
      CALL GSCR (IWKID,  1, 1.00, 1.00, 1.00)
C
C Red
C
      CALL GSCR (IWKID,  2, 0.90, 0.25, 0.00)
C
C OrangeRed
C
      CALL GSCR (IWKID,  3, 1.00, 0.00, 0.20)
C
C Orange
C
      CALL GSCR (IWKID,  4, 1.00, 0.65, 0.00)
C
C Yellow
C
      CALL GSCR (IWKID,  5, 1.00, 1.00, 0.00)
C
C GreenYellow
C
      CALL GSCR (IWKID,  6, 0.70, 1.00, 0.20)
C
C Chartreuse
C
      CALL GSCR (IWKID,  7, 0.50, 1.00, 0.00)
C
C Celeste
C
      CALL GSCR (IWKID,  8, 0.20, 1.00, 0.50)
C
C Green
C
      CALL GSCR (IWKID,  9, 0.20, 0.80, 0.20)
C
C Aqua
C
      CALL GSCR (IWKID, 10, 0.00, 0.90, 1.00)
C
C DeepSkyBlue
C
      CALL GSCR (IWKID, 11, 0.00, 0.75, 1.00)
C
C RoyalBlue
C
      CALL GSCR (IWKID, 12, 0.25, 0.45, 0.95)
C
C SlateBlue
C
      CALL GSCR (IWKID, 13, 0.40, 0.35, 0.80)
C
C DarkViolet
C
      CALL GSCR (IWKID, 14, 0.60, 0.00, 0.80)
C
C Orchid
C
      CALL GSCR (IWKID, 15, 0.85, 0.45, 0.80)
C
C Lavender
C
      CALL GSCR (IWKID, 16, 0.80, 0.80, 1.00)
C
C Gray
C
      CALL GSCR (IWKID, 17, 0.70, 0.70, 0.70)
C
C Done.
C
      RETURN
C
      END

