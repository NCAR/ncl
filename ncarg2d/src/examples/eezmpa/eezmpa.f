
        PROGRAM EEZMPA
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
C Define the array that holds the area map.  It is put in a labeled
C common block only because, on some machines, having a local array
C that large causes problems.
C
        COMMON /CLRCMN/ IAM(400000)
C
C Dimension the arrays needed by ARSCAM for edges.
C
        DIMENSION XCS(10000),YCS(10000)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for area and group
C ids.
C
        DIMENSION IAI(10),IAG(10)
C
C Define an array for RGB triples.
C
        DIMENSION RGB(3,14)
C
C Define an array for indices determining the ordering of the colors.
C
        DIMENSION IOC(14)
C
C Define an array for aspect source flags.
C
        DIMENSION IF(13)
C
C Declare the routine which will color the areas and the one which
C will draw the lines of latitude and longitude over water.
C
        EXTERNAL COLRAM,COLRLN
C
C Define the required RGB triples and indices.
C
        DATA RGB / 0.70,0.70,0.70 , 0.75,0.50,1.00 , 0.50,0.00,1.00 ,
     +             0.00,0.00,1.00 , 0.00,0.50,1.00 , 0.00,1.00,1.00 ,
     +             0.00,1.00,0.60 , 0.00,1.00,0.00 , 0.70,1.00,0.00 ,
     +             1.00,1.00,0.00 , 1.00,0.75,0.00 , 1.00,0.38,0.38 ,
     +             1.00,0.00,0.38 , 1.00,0.00,0.00 /
C
        DATA IOC / 6,2,5,12,10,11,1,3,4,8,9,7,13,14 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Re-set certain aspect source flags to "individual".
C
        CALL GQASF (IE,IF)
        IF(11)=1
        IF(12)=1
        CALL GSASF (IF)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define 15 different color indices.  The first 14 are spaced through
C the color spectrum and the final one is black.
C
        DO 101 J=1,14
          I=IOC(J)
          CALL GSCR(IWKID,J,RGB(1,I),RGB(2,I),RGB(3,I))
  101   CONTINUE
C
        CALL GSCR(IWKID,15,0.,0.,0.)
C
C Set up EZMAP, but don't draw anything.
C
        CALL MAPSTC ('OU','PO')
        CALL MAPROJ ('ME',0.,0.,0.)
        CALL MAPSET ('MA',0.,0.,0.,0.)
C
C Set the number of vertical strips and the group identifiers to
C be used by MAPBLA.
C
        CALL MAPSTI ('VS',150)
        CALL MAPSTI ('G1',1)
        CALL MAPSTI ('G2',2)
C
C Initialize EZMAP.
C
        CALL MAPINT
C
C Initialize the area map.
C
        CALL ARINAM (IAM,400000)
C
C Add edges to the area map.
C
        CALL MAPBLA (IAM)
C
C Pre-process the area map.
C
        CALL ARPRAM (IAM,0,0,0)
C
C Compute and print the amount of space used in the area map.
C
        ISU=400000-(IAM(6)-IAM(5)-1)
        PRINT * , 'SPACE USED IN AREA MAP IS ',ISU
C
C Set the background color.
C
        CALL GSCR (IWKID,0,1.,1.,1.)
C
C Color the map.
C
        CALL ARSCAM (IAM,XCS,YCS,10000,IAI,IAG,10,COLRAM)
C
C In black, draw a perimeter and outline all the countries.  We turn
C off the labels (since they seem to detract from the appearance of
C the plot) and we reduce the minimum vector length so as to include
C all of the points in the boundaries.
C
C Flush PLOTIT's buffers and set polyline color index to black.
C
        CALL PLOTIT(0,0,0)
        CALL GSPLCI(15)
C
        CALL MAPSTI ('LA',0)
        CALL MAPSTI ('MV',1)
        CALL MAPLBL
        CALL MAPLOT
C
C Draw lines of latitude and longitude over water.  They will be in
C black because of the GSPLCI call above.
C
        CALL MAPGRM (IAM,XCS,YCS,10000,IAI,IAG,10,COLRLN)
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
C
      SUBROUTINE COLRAM (XCS,YCS,NCS,IAI,IAG,NAI)
      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C For each area, one gets a set of points (using normalized device
C coordinates), two group identifiers and their associated area
C identifiers.  If both of the area identifiers are zero or negative,
C the area need not be color-filled; otherwise, it is filled with
C a color obtained from MAPACI.  If the area is defined by more than
C 150 points, we'd like to know about it.  (I'm assuming that the
C device being used won't handle polygons defined by more points than
C that.)
C
        IF (IAI(1).GE.0.AND.IAI(2).GE.0) THEN
          ITM=MAX(IAI(1),IAI(2))
          IF (ITM.GT.0) THEN
            IF (NCS.GT.150) PRINT * , 'COLRAM - NCS TOO BIG - ',NCS
C
C Set area fill color index.
C
            CALL GSFACI(MAPACI(ITM))
C
            CALL GFA (NCS-1,XCS,YCS)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
C
      SUBROUTINE COLRLN (XCS,YCS,NCS,IAI,IAG,NAI)
      DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
C
C For each line segment, one gets a set of points (using normalized
C device coordinates), two group identifiers and their associated
C area identifiers.  If both of the area identifiers are zero or
C negative, the segment is not drawn; otherwise, we use MAPACI to
C see if the segment is over water and, if so, we draw the segment.
C If the segment is defined by more than 150 points, we'd like to
C know about it.
C
        IF (IAI(1).GE.0.AND.IAI(2).GE.0) THEN
          ITM=MAX(IAI(1),IAI(2))
          IF (MAPACI(ITM).EQ.1) THEN
            IF (NCS.GT.150) PRINT * , 'COLRLN - NCS TOO BIG - ',NCS
            CALL GPL (NCS,XCS,YCS)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
