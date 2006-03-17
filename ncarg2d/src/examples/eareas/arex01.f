
      PROGRAM AREX01
C
C This example code forces ARPRAM to produce, for a simple area map, a
C set of plots showing all the edge segments in the area map at each of
C the steps that are done to preprocess the area map.  The user may wish
C to modify this example to look at what happens to other simple area
C maps.  For a complicated area map, though, the technique will probably
C produce an unreadable set of plots.
C
C The last plot produced by this example shows "contour bands" over a
C simple "map".  The colors used over "water" range from green at one
C end of the contour range to red at the other end.  Over "land", we
C use a color defined by an RGB triple in which the red and green
C components are what they would have been over "water", but we add
C a maximal blue component to wash out the color into a pastel shade.
C A rectangular area (in which, one might imagine, data were missing)
C is masked out so as to be left uncolored.
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
C Define the size of the area map to be used.
C
        PARAMETER (LAMA=10000)
C
C Define the sizes of the work arrays to be used by ARSCAM.
C
        PARAMETER (NCRA=1000,NGPS=10)
C
C Declare the area map array.
C
        DIMENSION IAMA(LAMA)
C
C Declare the work arrays to be used by ARSCAM.
C
        DIMENSION XCRA(NCRA),YCRA(NCRA),IAAI(NGPS),IAGI(NGPS)
C
C Declare arrays to hold a perimeter, to be used for all three groups
C of edges.
C
        DIMENSION PERIX(5),PERIY(5)
C
C Declare arrays for edge group 1, which may be thought of as defining
C a simple geographic map, showing a single continent, in the middle of
C which is is a lake.
C
        DIMENSION G1E1X(9),G1E1Y(9)
        DIMENSION G1E2X(5),G1E2Y(5)
C
C Declare arrays for edge group 3, which may be thought of as defining
C a contour map.  A "contour line" that lies on top of one of the others
C and has contradictory area identifiers is thrown in to show what AREAS
C does in such cases.
C
        DIMENSION G3E1X(2),G3E1Y(2)
        DIMENSION G3E2X(2),G3E2Y(2)
        DIMENSION G3E3X(2),G3E3Y(2)
        DIMENSION G3E4X(2),G3E4Y(2)
        DIMENSION G3E5X(2),G3E5Y(2)
        DIMENSION G3E6X(2),G3E6Y(2)
        DIMENSION G3E7X(2),G3E7Y(2)
        DIMENSION G3E8X(2),G3E8Y(2)
        DIMENSION G3E9X(2),G3E9Y(2)
C
C Declare arrays for edge group 5, which may be thought of as defining
C a "blocked" region, inside of which we don't want to show anything.
C This edge group is also used to show examples of coincident edge
C segments and "dangling" edge segments.
C
        DIMENSION G5E1X(7),G5E1Y(7)
C
C Declare the routine that will color the areas defined by the area map.
C
        EXTERNAL COLRAM
C
C Define the perimeter for all three groups of edges.
C
        DATA PERIX / .90,.90,.10,.10,.90 /
        DATA PERIY / .10,.90,.90,.10,.10 /
C
C Define the group 1 edges.
C
        DATA G1E1X / .75,.68,.50,.32,.25,.32,.50,.68,.75 /
        DATA G1E1Y / .50,.68,.75,.68,.50,.32,.25,.32,.50 /
C
        DATA G1E2X / .60,.60,.40,.40,.60 /
        DATA G1E2Y / .40,.60,.60,.40,.40 /
C
C Define the group 3 edges.
C
        DATA G3E1X / .10,.20 /
        DATA G3E1Y / .80,.90 /
C
        DATA G3E2X / .10,.40 /
        DATA G3E2Y / .60,.90 /
C
        DATA G3E3X / .10,.60 /
        DATA G3E3Y / .40,.90 /
C
        DATA G3E4X / .10,.80 /
        DATA G3E4Y / .20,.90 /
C
        DATA G3E5X / .20,.90 /
        DATA G3E5Y / .10,.80 /
C
        DATA G3E6X / .40,.90 /
        DATA G3E6Y / .10,.60 /
C
        DATA G3E7X / .60,.90 /
        DATA G3E7Y / .10,.40 /
C
        DATA G3E8X / .80,.90 /
        DATA G3E8Y / .10,.20 /
C
        DATA G3E9X / .40,.20 /
        DATA G3E9Y / .70,.50 /
C
C Define the group 5 edges.
C
        DATA G5E1X / .50,.80,.80,.50,.50,.20,.35 /
        DATA G5E1Y / .50,.50,.80,.80,.50,.20,.35 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Change the GKS "fill area interior style" to be "solid".
C
        CALL GSFAIS (1)
C
C Define the colors to be used.  Color indices 0 and 1 are the default
C background and foreground colors (black and white, respectively).
C Color indices 11 through 19 are to be used over "ocean" and color
C indices 21 through 29 are to be used over "land".
C
        CALL GSCR (IWKID,0,0.,0.,0.)
        CALL GSCR (IWKID,1,1.,1.,1.)
C
        DO 101 I=1,9
          S=MAX(0.,MIN(1.,REAL(I)/9.))
          T=MAX(0.,MIN(1.,1.-S))
          CALL GSCR (IWKID,10+I,S,T,0.)
          CALL GSCR (IWKID,20+I,S,T,1.)
  101   CONTINUE
C
C Define the mapping from the user system to the plotter frame.
C
        CALL SET (.05,.95,.05,.95,0.,1.,0.,1.,1)
C
C Turn on the AREAS debugging flag.  This will cause the routine
C ARDBPA to be called by ARPRAM.  For the purposes of this example,
C we supply our own version of ARDBPA, which has been hacked to show
C the contents of the area map at each breakpoint position for groups
C 1, 3, and 5, and to make the area identifiers big enough to be read
C without zooming.  It has also been hacked to omit any edge segment
C that has an endpoint outside the viewport defined by the lines
C "X=.05", "X=.95", "Y=.05", and "Y=.95", in the fractional coordinate
C system.
C
        CALL ARSETI ('DB',1)
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,LAMA)
C
C Put group 1 edges into the area map.
C
        CALL AREDAM (IAMA,PERIX,PERIY,5,1, 0,-1)
        CALL AREDAM (IAMA,G1E1X,G1E1Y,9,1, 2, 1)
        CALL AREDAM (IAMA,G1E2X,G1E2Y,5,1, 1, 2)
C
C Put group 3 edges into the area map.
C
        CALL AREDAM (IAMA,PERIX,PERIY,5,3, 0,-1)
        CALL AREDAM (IAMA,G3E1X,G3E1Y,2,3, 1, 2)
        CALL AREDAM (IAMA,G3E2X,G3E2Y,2,3, 2, 3)
        CALL AREDAM (IAMA,G3E3X,G3E3Y,2,3, 3, 4)
        CALL AREDAM (IAMA,G3E4X,G3E4Y,2,3, 4, 5)
        CALL AREDAM (IAMA,G3E5X,G3E5Y,2,3, 5, 6)
        CALL AREDAM (IAMA,G3E6X,G3E6Y,2,3, 6, 7)
        CALL AREDAM (IAMA,G3E7X,G3E7Y,2,3, 7, 8)
        CALL AREDAM (IAMA,G3E8X,G3E8Y,2,3, 8, 9)
        CALL AREDAM (IAMA,G3E9X,G3E9Y,2,3, 9,10)
C
C Put group 5 edges into the area map.
C
        CALL AREDAM (IAMA,PERIX,PERIY,5,5, 0,-1)
        CALL AREDAM (IAMA,G5E1X,G5E1Y,7,5,-1, 0)
C
C Scan the area map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,NCRA,IAAI,IAGI,NGPS,COLRAM)
C
C Put a label at the top of the final plot.
C
        CALL PLCHLQ (.5,.98,'THE COLORED AREAS',.014,0.,0.)
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



      SUBROUTINE ARDBPA (IAMA,IGIP,LABL)
C
        DIMENSION IAMA(*)
C
        CHARACTER*(*) LABL
C
C This is a version of the AREAS routine ARDBPA that has been modified
C to produce plots for each of edge groups 1, 3, and 5.
C
        CHARACTER*80 LABM
C
C Compute the length of the original label.
C
        LENL=LEN(LABL)
C
C Do a plot showing the contents of the area map for group 1.
C
        LABM=LABL // ' - GROUP 1'
        CALL ARDBPB (IAMA,1,LABM(1:LENL+10))
        CALL FRAME
C
C Do a plot showing the contents of the area map for group 3.
C
        LABM=LABL // ' - GROUP 3'
        CALL ARDBPB (IAMA,3,LABM(1:LENL+10))
        CALL FRAME
C
C Do a plot showing the contents of the area map for group 5.
C
        LABM=LABL // ' - GROUP 5'
        CALL ARDBPB (IAMA,5,LABM(1:LENL+10))
        CALL FRAME
C
C Do a plot showing the contents of the area map for all groups.
C
        LABM=LABL // ' - ALL GROUPS'
        CALL ARDBPB (IAMA,1,LABM(1:LENL+13))
        CALL ARDBPB (IAMA,3,LABM(1:LENL+13))
        CALL ARDBPB (IAMA,5,LABM(1:LENL+13))
        CALL FRAME
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE ARDBPB (IAMA,IGIP,LABL)
C
        DIMENSION IAMA(*)
C
        CHARACTER*(*) LABL
C
C This is a version of the AREAS routine ARDBPA that has been modified
C to use larger characters for the area identifiers and to omit edge
C segments that are too close to the edge of the plotter frame.  If
C the common block in AREAS is changed or if the structure of an area
C map is changed, this routine may need to be changed to match.
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
        COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                  IDC,IDI,IRC(16),RLA,RWA,RDI,RSI
        SAVE   /ARCOMN/
C
C The common block ARCOM1 is used to communicate with the arrow-drawing
C routine ARDBDA.
C
        COMMON /ARCOM1/ DT
C
C Save the current state of the SET call and switch to the fractional
C coordinate system.
C
        CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
        CALL    SET (  0.,  1.,  0.,  1.,  0.,  1.,  0.,  1.,   1)
C
C Put a label at the top of the plot.
C
        CALL PLCHLQ (.5,.98,LABL,.014,0.,0.)
C
C Trace the edges in the area map, drawing arrows as we go.
C
        DT=0.
        INDX=8
        RXCN=.5
        RYCN=.5
C
  101   RXCO=RXCN
        RYCO=RYCN
        RXCN=REAL(IAMA(INDX+1))/RLC
        RYCN=REAL(IAMA(INDX+2))/RLC
C
        IF (IAMA(INDX+7).NE.0) THEN
          IGID=ABS(IAMA(INDX+7))
          IF (IGID.LT.IAMA(6)) THEN
            IGID=IAMA(IAMA(1)-IGID)/2
          ELSE
            IGID=IAMA(IGID)/2
          END IF
          IF (IGID.EQ.IGIP) THEN
            IAIL=IAMA(INDX+8)
            IF (IAIL.GT.0) IAIL=IAMA(IAIL)/2
            IAIR=IAMA(INDX+9)
            IF (IAIR.GT.0) IAIR=IAMA(IAIR)/2
            CALL ARDBDA (RXCO,RYCO,RXCN,RYCN,IAIL,IAIR)
          END IF
        ELSE
          DT=0.
        END IF
C
        IF (IAMA(INDX+3).NE.0) THEN
          INDX=IAMA(INDX+3)
          GO TO 101
        END IF
C
C Restore the original SET call.
C
        CALL SET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE ARDBDA (X1,Y1,X2,Y2,IL,IR)
C
C The routine ARDBDA is called by ARDBPB, above, to draw an arrow from
C the point (X1,Y1) to the point (X2,Y2), in the fractional coordinate
C system.  The left and right area identifiers IL and IR are written
C in the proper positions relative to the arrow.  In order to prevent
C too many arrowheads from appearing, we keep track of the cumulative
C distance along edges being drawn (in DT).
C
        COMMON /ARCOM1/ DT
C
C Define character variables required to write the area identifiers.
C
        CHARACTER*6 CS
        CHARACTER*1 IC
C
C Define some temporary X and Y coordinate arrays, to be used in marking
C the head and tail of the arrow.
C
        DIMENSION TX(8),TY(8)
C
C If either end of the arrow is too close to the edge of the frame,
C don't draw anything.
C
        IF (X1.LT..05.OR.X1.GT..95) RETURN
        IF (Y1.LT..05.OR.Y1.GT..95) RETURN
        IF (X2.LT..05.OR.X2.GT..95) RETURN
        IF (Y2.LT..05.OR.Y2.GT..95) RETURN
C
C Draw the body of the arrow.
C
        CALL PLOTIF(X1,Y1,0)
        CALL PLOTIF(X2,Y2,1)
C
C Compute the length of the arrow.  If it's zero, quit.
C
        DX=X2-X1
        DY=Y2-Y1
        DP=SQRT(DX*DX+DY*DY)
C
        IF (DP.EQ.0.) RETURN
C
C If the area identifiers are in a reasonable range (less than 100,000
C in absolute value), write them on either side of the arrow.
C
        IF (ABS(IL).LT.100000.AND.ABS(IR).LT.100000) THEN
C
          XC=.5*(X1+X2)
          YC=.5*(Y1+Y2)
          XL=XC-.025*DY/DP
          YL=YC+.025*DX/DP
          WRITE (CS,'(I6)') IL
          NC=0
          DO 101 I=1,6
            IC=CS(I:I)
            IF (IC.NE.' ') THEN
              NC=NC+1
              CS(NC:NC)=IC
            END IF
  101     CONTINUE
          CALL PLCHLQ (XL,YL,CS(1:NC),.015,0.,0.)
C
          XR=XC+.025*DY/DP
          YR=YC-.025*DX/DP
          WRITE (CS,'(I6)') IR
          NC=0
          DO 102 I=1,6
            IC=CS(I:I)
            IF (IC.NE.' ') THEN
              NC=NC+1
              CS(NC:NC)=IC
            END IF
  102     CONTINUE
          CALL PLCHLQ (XR,YR,CS(1:NC),.015,0.,0.)
C
        END IF
C
C Put a dot at the tail of the arrow.  This will help identify arrows
C that are drawn tail-to-tail.
C
        TX(1)=X1-.0060
        TY(1)=Y1
        TX(2)=X1-.0042
        TY(2)=Y1+.0042
        TX(3)=X1
        TY(3)=Y1+.0060
        TX(4)=X1+.0042
        TY(4)=Y1+.0042
        TX(5)=X1+.0060
        TY(5)=Y1
        TX(6)=X1+.0042
        TY(6)=Y1-.0042
        TX(7)=X1
        TY(7)=Y1-.0060
        TX(8)=X1-.0042
        TY(8)=Y1-.0042
C
        CALL GSFACI (1)
        CALL GFA    (8,TX,TY)
C
C If the cumulative length of the edge being drawn is too little,
C quit ...
C
        DT=DT+DP
        IF(DT.LE..008) RETURN
C
C ... otherwise, zero the cumulative length ...
C
        DT=0.
C
C and draw an solid-filled arrowhead.
C
        B=(DP-.04)/DP
        A=1.0-B
        XT=A*X1+B*X2
        YT=A*Y1+B*Y2
        B=(DP-.015)/DP
        A=1.0-B
C
        TX(1)=X2
        TY(1)=Y2
        TX(2)=XT-.01*DY/DP
        TY(2)=YT+.01*DX/DP
        TX(3)=A*X1+B*X2
        TY(3)=A*Y1+B*Y2
        TX(4)=XT+.01*DY/DP
        TY(4)=YT-.01*DX/DP
C
        CALL GSFACI (1)
        CALL GFA    (4,TX,TY)
C
C Done.
C
        RETURN
C
      END



      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
C This routine colors the areas defined by the area map.
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C Pick off the individual group identifiers.
C
        IAI1=0
        IAI3=0
        IAI5=0
C
        DO 101 I=1,NGPS
          IF (IAGI(I).EQ.1) IAI1=IAAI(I)
          IF (IAGI(I).EQ.3) IAI3=IAAI(I)
          IF (IAGI(I).EQ.5) IAI5=IAAI(I)
  101   CONTINUE
C
C Skip coloring if either of the first two area identifiers is zero or
C negative or if the final one is negative.
C
        IF (IAI1.LE.0.OR.IAI3.LE.0.OR.IAI5.LT.0) GO TO 102
C
C Otherwise, color the area, using a color index which is obtained by
C combining the area identifiers for groups 1 and 3.
C
        CALL GSFACI (10*IAI1+IAI3)
        CALL GFA    (NCRA-1,XCRA,YCRA)
C
C Done.
C
  102   RETURN
C
      END
