C
C	$Id: mapaci.f,v 1.1.1.1 1992-04-17 22:32:15 ncargd Exp $
C
C ---------------------------------------------------------------------
C I N T R O D U C T I O N
C ---------------------------------------------------------------------
C
C This file contains a write-up, implementation instructions, and the
C code for a set of routines allowing one to direct EZMAP output (lines
C of latitude and longitude and boundary outlines) to routines in the
C package AREAS.  Among other things, this gives one the capability to
C create solid-color global maps.  Routines included are MAPBLA, MAPGRM,
C MAPITA, MAPIQA, MAPITM, MAPIQM, and MAPACI.  The reader is expected
C to be familiar with the packages EZMAP and AREAS.
C
C The routine MAPBLA may be used to add to an "area map" the boundary
C lines produced by projecting a selected EZMAP dataset according to a
C specified EZMAP projection within a specified area.  The area map may
C then be used in various ways.  An obvious choice is to simply produce
C a colored map.
C
C The routine MAPGRM may be used, following a call to MAPBLA, to draw
C lines of latitude and longitude masked by the areas defined by the
C area map.  For example, one might choose to draw such lines only over
C water.
C
C The routines MAPITA and MAPIQA may be used to add arbitrary lines,
C defined by a set of latitude/longitude pairs, to an area map; they
C handle all of the problems of interpolating to the limb and/or to
C the perimeter.
C
C The routines MAPITM and MAPIQM may be used to draw arbitrary lines,
C defined by a set of latitude/longitude pairs, on a map, masked by the
C areas defined by the area map created by a call to MAPBLA; they
C handle all of the problems of interpolating to the limb and/or to
C the perimeter.
C
C The function MAPACI may be used to recover, for a given area, a color
C index appropriate for use in generating a color map having adjacent
C areas of different colors.
C
C
C ---------------------------------------------------------------------
C W R I T E - U P
C ---------------------------------------------------------------------
C
C To use the routines in this package, one must first execute the
C FORTRAN statement
C
C       CALL ARINAM (IAM,LAM)
C
C in order to provide the package AREAS with an integer array IAM, of
C length LAM, in which to construct an area map.  One may then call
C various EZMAP routines (MAPPOS, MAPROJ, MAPSET, MAPSTC, MAPSTI, and
C MAPSTR) to define the position of the map on the plotter frame, the
C projection to be used, the EZMAP dataset to be used, and the limits
C of the map, just as if one were going to call MAPDRW to draw a map.
C Then, one must execute the statement
C
C       CALL MAPINT
C
C to initialize EZMAP and the statement
C
C       CALL MAPBLA (IAM)
C
C to retrieve, project, and add to the area map the selected EZMAP
C boundary lines.
C
C One or two groups of boundary lines are added to the area map by a
C call to MAPBLA.  The first, having group identifier 'G1' (default
C value 1), consists of a perimeter (either rectangular or elliptical,
C depending on the value of the EZMAP parameter 'EL') and the set of
C projected boundary lines implied by the user's selection of an EZMAP
C dataset (some combination of continental, U.S., and world political
C outlines).  For certain projections, a limb line may also be included.
C If the parameter 'VS' has a value greater than zero, the group 'G2'
C is added to the area map; it consists of a copy of the perimeter and
C the limb line (if any) plus a set of vertical lines splitting the
C area inside the perimeter into 'VS' vertical strips.  (By default,
C the value of 'VS' is 1.)  The object of the group 'G2' is to split
C areas up, reducing the number of points required to define a typical
C area below the level at which the target hardware device begins to
C fail.
C
C The values of 'G1', 'G2', and 'VS' may be set by calling the EZMAP
C routine MAPSTI; current values of them may be retrieved by calling
C the EZMAP routine MAPGTI.  See the EZMAP write-up for descriptions
C of the routines MAPSTI and MAPGTI.
C
C After the call to MAPBLA, the other routines in the package may be
C called.  To draw masked lines of latitude and longitude, execute the
C following FORTRAN statement:
C
C       CALL MAPGRM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
C XCS and YCS are real arrays, dimensioned at least MCS, to be used in
C calls by MAPGRM to the AREAS routine ARDRLN and, ultimately, in calls
C by that routine to the user-provided routine LPR, to hold coordinates
C of points defining portions of latitude and longitude lines.  IAI and
C IAG are integer arrays, dimensioned at least MAI, to be used similarly
C to hold area-identifier/group-identifier pairs defining over what part
C of the globe the latitude/longitude line segment lies.  LPR is the
C name of a user-provided routine to draw (or to not draw) each such
C line segment; it will be called by a FORTRAN statement like
C
C       CALL LPR (XCS,YCS,NCS,IAI,IAG,NAI)
C
C where XCS and YCS hold the normalized device coordinates of NCS points
C defining a piece of a latitude/longitude line and IAI and IAG hold NAI
C area-identifier/group-identifier pairs for the area within which that
C piece of the line lies.  LPR must be declared "EXTERNAL" in the user
C routine which calls MAPGRM.  See the example below.
C
C The routines MAPITA and MAPIQA may be used to add lines defined by a
C set of user-specified latitudes and longitudes to the area map; they
C handle all of the problems of interpolating to the limb and/or to the
C perimeter.  MAPITA is called like the EZMAP routine MAPIT (which see,
C in the EZMAP write-up) but has some additional arguments:
C
C       CALL MAPITA (RLAT,RLON,IFST,IAM,IGI,IDL,IDR)
C
C Additional arguments are the area map array IAM, a group identifier
C IGI, and left and right area identifiers IDL and IDR.  MAPIQA is
C called like the EZMAP routine MAPIQ, to terminate a series of calls
C to MAPITA and flush the buffers; it has the same additional arguments:
C
C       CALL MAPIQA (IAM,IGI,IDL,IDR)
C
C The routines MAPITM and MAPIQM may be used to draw lines defined by a
C set of user-specified latitudes and longitudes on a map, masked by
C the areas defined by the area map created by a call to MAPBLA; they
C handle all of the problems of interpolating to the limb and/or to
C the perimeter.  MAPITM is called like the EZMAP routine MAPIT (which
C see, in the EZMAP write-up) but has some additional arguments:
C
C       CALL MAPITM (RLAT,RLON,IFST,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
C Additional arguments are the area-map array IAM, coordinate arrays
C XCS and YCS, dimensioned MCS, integer arrays IAI and IAG, dimensioned
C MAI, and a user-specified routine named LPR.  MAPIQM is called like
C the EZMAP routine MAPIQ, to terminate a series of calls to MAPITM and
C flush the buffers; it has the same additional arguments:
C
C       CALL MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
C See the description of MAPGRM, above, for more information about these
C arguments.
C
C The function MAPACI has as argument an area identifier IAI generated
C by MAPBLA.  Its value is a color index between 1 and 7, inclusive,
C which is appropriate for use in generating a color map having adjacent
C areas of different colors.
C
C
C ---------------------------------------------------------------------
C E X A M P L E
C ---------------------------------------------------------------------
C
C Following is the text of a program which uses the routines in this
C package to produce a colored map of a mercator projection of the
C globe, showing world political boundaries, with lines of latitude
C and longitude drawn only over water.  It should be noted that this
C example will work properly only on a device with a color overlay
C capability; if you attempt to send the output to a device like the
C color DICOMED, the attempt to draw black lines will not work.
C
C       PROGRAM COLRIT
C C
C C Define the array that holds the area map.  It is put in a labeled
C C common block only because, on some machines, having a local array
C C that large causes problems.
C C
C         COMMON /CLRCMN/ IAM(250000)
C C
C C Dimension the arrays needed by ARSCAM for edges.
C C
C         DIMENSION XCS(10000),YCS(10000)
C C
C C Dimension the arrays needed by ARSCAM and ARDRLN for area and group
C C ids.
C C
C         DIMENSION IAI(10),IAG(10)
C C
C C Define an array for RGB triples.
C C
C         DIMENSION RGB(3,14)
C C
C C Define an array for indices determining the ordering of the colors.
C C
C         DIMENSION IOC(14)
C C
C C Define an array for aspect source flags.
C C
C         DIMENSION IF(13)
C C
C C Declare the routine which will color the areas and the one which
C C will draw the lines of latitude and longitude over water.
C C
C         EXTERNAL COLRAM,COLRLN
C C
C C Define the required RGB triples and indices.
C C
C         DATA RGB / 0.70,0.70,0.70 , 0.75,0.50,1.00 , 0.50,0.00,1.00 ,
C      +             0.00,0.00,1.00 , 0.00,0.50,1.00 , 0.00,1.00,1.00 ,
C      +             0.00,1.00,0.60 , 0.00,1.00,0.00 , 0.70,1.00,0.00 ,
C      +             1.00,1.00,0.00 , 1.00,0.75,0.00 , 1.00,0.38,0.38 ,
C      +             1.00,0.00,0.38 , 1.00,0.00,0.00 /
C C
C         DATA IOC / 6,2,5,12,10,11,1,3,4,8,9,7,13,14 /
C C
C C Open GKS.
C C
C         CALL OPNGKS
C C
C C Re-set certain aspect source flags to "individual".
C C
C         CALL GQASF (IE,IF)
C         IF(11)=1
C         IF(12)=1
C         CALL GSASF (IF)
C C
C C Force solid fill.
C C
C         CALL GSFAIS (1)
C C
C C Define 15 different color indices.  The first 14 are spaced through
C C the color spectrum and the final one is black.
C C
C         DO 101 J=1,14
C           I=IOC(J)
C           CALL GSCR(1,J,RGB(1,I),RGB(2,I),RGB(3,I))
C   101   CONTINUE
C C
C         CALL GSCR(1,15,0.,0.,0.)
C C
C C Set up EZMAP, but don't draw anything.
C C
C         CALL MAPSTC ('OU','PO')
C         CALL MAPROJ ('ME',0.,0.,0.)
C         CALL MAPSET ('MA',0.,0.,0.,0.)
C C
C C Set the number of vertical strips and the group identifiers to
C C be used by MAPBLA.
C C
C         CALL MAPSTI ('VS',150)
C         CALL MAPSTI ('G1',1)
C         CALL MAPSTI ('G2',2)
C C
C C Initialize EZMAP.
C C
C         CALL MAPINT
C C
C C Initialize the area map.
C C
C         CALL ARINAM (IAM,250000)
C C
C C Add edges to the area map.
C C
C         CALL MAPBLA (IAM)
C C
C C Pre-process the area map.
C C
C         CALL ARPRAM (IAM,0,0,0)
C C
C C Compute and print the amount of space used in the area map.
C C
C         ISU=250000-(IAM(6)-IAM(5)-1)
C         PRINT * , 'SPACE USED IN AREA MAP IS ',ISU
C C
C C Set the background color.
C C
C         CALL GSCR (1,0,1.,1.,1.)
C C
C C Color the map.
C C
C         CALL ARSCAM (IAM,XCS,YCS,10000,IAI,IAG,10,COLRAM)
C C
C C In black, draw a perimeter and outline all the countries.  We turn
C C off the labels (since they seem to detract from the appearance of
C C the plot) and we reduce the minimum vector length so as to include
C C all of the points in the boundaries.
C C
C C Flush PLOTIT's buffers and set polyline color index to black.
C C
C         CALL PLOTIT(0,0,0)
C         CALL GSPLCI(15)
C C
C         CALL MAPSTI ('LA',0)
C         CALL MAPSTI ('MV',1)
C         CALL MAPLBL
C         CALL MAPLOT
C C
C C Draw lines of latitude and longitude over water.  They will be in
C C black because of the GSPLCI call above.
C C
C         CALL MAPGRM (IAM,XCS,YCS,10000,IAI,IAG,10,COLRLN)
C C
C C Advance the frame.
C C
C         CALL FRAME
C C
C C Close GKS.
C C
C         CALL CLSGKS
C C
C C Done.
C C
C         STOP
C C
C       END
C
C       SUBROUTINE COLRAM (XCS,YCS,NCS,IAI,IAG,NAI)
C       DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C C
C C For each area, one gets a set of points (using normalized device
C C coordinates), two group identifiers and their associated area
C C identifiers.  If both of the area identifiers are zero or negative,
C C the area need not be color-filled; otherwise, it is filled with
C C a color obtained from MAPACI.  If the area is defined by more than
C C 150 points, we'd like to know about it.  (I'm assuming that the
C C device being used won't handle polygons defined by more points than
C C that.)
C C
C         IF (IAI(1).GE.0.AND.IAI(2).GE.0) THEN
C           ITM=MAX0(IAI(1),IAI(2))
C           IF (ITM.GT.0) THEN
C             IF (NCS.GT.150) PRINT * , 'COLRAM - NCS TOO BIG - ',NCS
C C
C C Set area fill color index.
C C
C             CALL GSFACI(MAPACI(ITM))
C C
C             CALL GFA (NCS-1,XCS,YCS)
C           END IF
C         END IF
C C
C C Done.
C C
C         RETURN
C C
C       END
C
C       SUBROUTINE COLRLN (XCS,YCS,NCS,IAI,IAG,NAI)
C       DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C C
C C For each line segment, one gets a set of points (using normalized
C C device coordinates), two group identifiers and their associated
C C area identifiers.  If both of the area identifiers are zero or
C C negative, the segment is not drawn; otherwise, we use MAPACI to
C C see if the segment is over water and, if so, we draw the segment.
C C If the segment is defined by more than 150 points, we'd like to
C C know about it.
C C
C         IF (IAI(1).GE.0.AND.IAI(2).GE.0) THEN
C           ITM=MAX0(IAI(1),IAI(2))
C           IF (MAPACI(ITM).EQ.1) THEN
C             IF (NCS.GT.150) PRINT * , 'COLRLN - NCS TOO BIG - ',NCS
C             CALL GPL (NCS,XCS,YCS)
C           END IF
C         END IF
C C
C C Done.
C C
C         RETURN
C C
C       END
C
C Let the user beware:  these routines require lots of memory and lots
C of CPU time.  In this example, the area map was dimensioned 250000; a
C little over 225000 words were actually required.  The job ran about
C 22 seconds on the Cray X-MP.  Reducing the number of vertical strips
C used would cut both of these numbers significantly.
C
C ---------------------------------------------------------------------
C I M P L E M E N T A T I O N   I N S T R U C T I O N S
C ---------------------------------------------------------------------
C
C If this file begins with the two lines
C
C    .OP LS=10000 LI=1 CB RT ES=< ET=> OC UC=0
C    .EL I
C
C then it is the IFTRAN version, which must be run through the IFTRAN
C preprocessor to generate a FORTRAN version; otherwise, it is the
C FORTRAN version.  Nothing special need be done to run the FORTRAN
C version; however, both of the packages EZMAP and AREAS must be
C available and the former must be the latest version (July, 1987).
C
C ---------------------------------------------------------------------
C C O D E
C ---------------------------------------------------------------------
C
C The function MAPACI.
C --- -------- ------
C
      FUNCTION MAPACI (IAI)
C
C Given an integer area identifier IAI generated by MAPBLA, the value
C of MAPACI is an appropriate color index for the area.
C
C
C Define the array of color indices.
C
      DIMENSION ICI(1361)
C
      DATA (ICI(I),I=   1, 100) /
     +    2,1,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 ,
     +    1,2,2,2,2 , 1,2,2,2,1 , 2,2,2,2,2 , 1,2,1,1,2 , 1,2,2,2,2 ,
     +    1,2,2,2,1 , 2,2,2,1,2 , 2,2,2,2,2 , 1,2,2,2,2 , 2,2,2,2,2 ,
     +    2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 /
      DATA (ICI(I),I= 101, 200) /
     +    2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,1,2,2,2 , 2,2,1,2,1 ,
     +    1,1,2,1,1 , 1,2,2,1,2 , 2,2,2,2,2 , 2,2,1,2,2 , 2,1,1,2,2 ,
     +    2,2,2,2,1 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 ,
     +    2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 /
      DATA (ICI(I),I= 201, 300) /
     +    2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,2,2,2 , 2,2,7,2,3 ,
     +    4,2,2,2,2 , 2,2,2,4,4 , 1,5,4,4,4 , 4,6,5,1,2 , 1,3,1,4,5 ,
     +    6,2,5,2,3 , 3,4,2,2,4 , 1,6,2,1,1 , 4,2,4,4,5 , 4,4,1,1,1 ,
     +    1,3,1,4,1 , 1,4,2,4,4 , 2,4,2,2,1 , 4,2,2,1,3 , 4,4,4,6,4 /
      DATA (ICI(I),I= 301, 400) /
     +    4,4,4,4,4 , 4,2,2,4,5 , 5,1,4,2,2 , 2,2,2,1,6 , 2,4,1,4,2 ,
     +    2,1,4,1,2 , 2,2,1,2,2 , 1,2,3,1,4 , 4,2,4,4,4 , 4,1,4,4,4 ,
     +    4,6,4,2,2 , 3,1,4,4,4 , 4,4,4,4,4 , 4,4,4,4,4 , 2,4,4,4,2 ,
     +    4,2,4,2,4 , 4,2,4,4,2 , 1,2,4,2,2 , 4,4,5,4,4 , 4,4,4,4,2 /
      DATA (ICI(I),I= 401, 500) /
     +    2,1,4,4,2 , 3,1,3,5,4 , 1,2,2,1,1 , 1,1,6,3,3 , 6,6,6,6,6 ,
     +    1,5,4,4,1 , 5,5,5,5,5 , 5,5,5,5,7 , 1,6,6,3,6 , 3,3,3,3,3 ,
     +    3,3,7,3,3 , 3,3,3,3,7 , 3,6,6,6,6 , 1,2,3,4,6 , 2,2,2,2,2 ,
     +    2,2,4,4,1 , 5,4,4,6,6 , 4,4,6,6,1 , 6,5,6,1,2 , 1,3,6,6,1 /
      DATA (ICI(I),I= 501, 600) /
     +    1,4,6,5,6 , 2,6,6,6,6 , 5,2,3,1,3 , 6,4,1,1,6 , 1,2,2,4,6 ,
     +    1,6,6,6,2 , 1,1,4,2,6 , 4,4,5,4,4 , 1,1,1,1,3 , 1,4,1,3,1 ,
     +    4,2,3,4,4 , 2,4,2,2,1 , 4,6,2,2,1 , 3,4,6,4,4 , 4,5,6,4,4 ,
     +    4,4,4,4,4 , 2,2,2,4,5 , 5,1,6,4,6 , 2,2,2,2,2 , 2,1,6,2,4 /
      DATA (ICI(I),I= 601, 700) /
     +    1,4,4,2,2 , 1,4,1,2,6 , 2,2,1,2,2 , 4,5,1,2,3 , 1,4,4,6,2 ,
     +    4,4,4,4,5 , 1,3,4,6,4 , 4,4,6,4,2 , 2,6,6,3,1 , 4,2,4,4,5 ,
     +    4,4,4,4,4 , 4,4,4,4,4 , 6,2,4,4,4 , 7,2,4,7,2 , 4,2,4,4,2 ,
     +    4,4,2,1,2 , 4,2,2,4,4 , 5,4,4,4,4 , 4,4,2,2,2 , 1,4,4,2,6 /
      DATA (ICI(I),I= 701, 800) /
     +    2,2,3,1,6 , 3,5,4,4,3 , 1,2,3,2,1 , 1,1,2,1,3 , 3,3,6,3,6 ,
     +    6,6,6,1,5 , 4,4,1,4,3 , 5,5,5,3,5 , 5,5,5,5,5 , 3,6,7,6,7 ,
     +    5,6,3,3,4 , 5,3,7,7,6 , 2,5,7,3,7 , 5,7,2,3,6 , 4,2,4,2,2 ,
     +    5,2,2,6,5 , 6,3,4,4,2 , 2,4,5,2,2 , 2,5,2,3,3 , 5,6,5,3,4 /
      DATA (ICI(I),I= 801, 900) /
     +    2,6,4,3,4 , 5,5,4,6,5 , 2,6,3,3,3 , 5,6,4,4,3 , 2,3,2,4,2 ,
     +    5,2,2,4,4 , 2,6,6,1,4 , 2,5,5,5,5 , 2,3,6,2,6 , 3,3,2,6,3 ,
     +    5,6,5,2,6 , 3,3,6,1,2 , 5,1,4,2,3 , 1,4,6,1,3 , 2,4,5,1,2 ,
     +    1,1,3,5,3 , 4,6,2,3,5 , 3,6,5,6,4 , 1,6,2,4,6 , 4,6,6,6,3 /
      DATA (ICI(I),I= 901,1000) /
     +    6,4,3,1,6 , 3,4,6,2,3 , 1,5,1,4,5 , 3,3,4,3,6 , 3,5,5,5,5 ,
     +    6,6,2,4,1 , 5,5,5,5,5 , 6,6,5,3,3 , 5,5,4,5,5 , 5,4,2,4,4 ,
     +    4,5,5,5,4 , 2,5,5,4,5 , 5,5,3,3,5 , 5,3,5,3,5 , 5,3,6,6,3 ,
     +    6,6,3,6,3 , 6,6,3,6,3 , 3,3,6,3,3 , 6,3,7,6,3 , 3,6,3,6,6 /
      DATA (ICI(I),I=1001,1100) /
     +    3,3,2,7,1 , 6,6,3,6,3 , 3,3,3,3,3 , 3,7,3,3,3 , 3,3,3,7,3 ,
     +    6,6,6,6,1 , 3,6,6,6,6 , 1,6,6,6,6 , 1,7,6,6,6 , 6,1,6,1,1 ,
     +    6,1,6,6,6 , 6,3,1,3,6 , 4,6,5,2,1 , 6,6,2,4,6 , 1,6,4,5,6 ,
     +    5,3,6,6,6 , 1,2,6,6,5 , 6,6,7,7,2 , 2,6,2,6,3 , 4,3,3,2,3 /
      DATA (ICI(I),I=1101,1200) /
     +    4,3,3,3,6 , 7,6,7,5,6 , 3,3,4,5,3 , 7,7,6,2,5 , 7,3,7,5,7 ,
     +    2,3,6,4,2 , 4,2,2,5,2 , 2,6,5,6,3 , 4,4,2,2,4 , 5,2,2,2,5 ,
     +    2,3,3,5,6 , 5,3,4,2,6 , 4,3,4,5,5 , 4,6,5,2,6 , 3,3,3,5,6 ,
     +    4,4,3,2,3 , 2,4,2,5,2 , 2,4,4,2,6 , 6,1,4,2,5 , 5,5,5,2,3 /
      DATA (ICI(I),I=1201,1300) /
     +    6,2,6,3,3 , 2,6,3,5,6 , 5,2,6,3,3 , 6,1,2,5,1 , 4,2,3,1,4 ,
     +    6,1,3,2,4 , 5,1,2,1,1 , 3,5,3,4,6 , 2,3,5,3,6 , 5,6,4,1,6 ,
     +    2,4,6,4,6 , 6,6,3,6,4 , 3,1,6,3,4 , 6,2,3,1,5 , 1,4,5,3,3 ,
     +    4,3,6,3,5 , 5,5,5,6,6 , 2,4,1,5,5 , 5,5,5,6,6 , 5,3,3,5,5 /
      DATA (ICI(I),I=1301,1361) /
     +    4,5,5,5,4 , 2,4,4,4,5 , 5,5,4,2,5 , 5,4,5,5,5 , 3,3,5,5,3 ,
     +    5,3,5,5,3 , 6,6,3,6,6 , 3,6,3,6,6 , 3,6,3,3,3 , 6,3,3,6,3 ,
     +    7,6,3,3,6 , 3,6,6,3,3 , 2 /
C
C Pull out the appropriate color index, taking precautions to prevent
C an out-of-array reference.
C
      IF (.NOT.(IAI.GE.1.AND.IAI.LE.1361)) GO TO 10000
      MAPACI=ICI(IAI)
      GO TO 10001
10000 CONTINUE
      MAPACI=1
10001 CONTINUE
C
C Done.
C
      RETURN
C
      END
