
      PROGRAM TEZMPB
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
C Open GKS, open workstation of type 1, activate workstation.
C
      CALL GOPKS (IERRF,ISZDM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C Invoke demo driver.
C
      CALL EZMPB (IERR,IWKID)
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
      END

      SUBROUTINE EZMPB (IERR,IWKID)
C
C PURPOSE                To provide a simple demonstration of the use
C                        of EZMAPB.
C
C USAGE                  CALL EZMPB (IERR,IWKID)
C
C ARGUMENTS
C
C ON OUTPUT              IERR
C
C                          an error parameter
C                          = 0, if the test is successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C                          EZMAPB TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is written on unit 6.
C
C PRECISION              Single.
C
C REQUIRED LIBRARY       EZMAP, EZMAPA, EZMAPB, AREAS, SPPS
C FILES
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written in May, 1998.
C
C ALGORITHM              TEZMPB draws a solid-color map of a portion
C                        of Europe.
C
C PORTABILITY            FORTRAN 77
C
C
C Define an array in which to construct the area map.
C
        DIMENSION IAMA(100000)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for x/y coordinates.
C
        DIMENSION XCRA(2000),YCRA(2000)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for area and group
C identifiers.
C
        DIMENSION IAAI(2),IAGI(2)
C
C Define the RGB color triples needed below.
C
        DIMENSION RGB(3,14)
C
C Define a set of indices to order the colors.
C
        DIMENSION IOC(14)
C
C Declare the routine which will color the areas.
C
        EXTERNAL COLRAM
C
C Declare the routine which will draw lines of latitude and longitude
C over water.
C
        EXTERNAL COLRLN
C
        DATA IOC / 6,2,5,12,10,11,1,3,4,8,9,7,13,14 /
C
        DATA RGB / 0.70 , 0.70 , 0.70 ,
     +             0.75 , 0.50 , 1.00 ,
     +             0.50 , 0.00 , 1.00 ,
     +             0.00 , 0.00 , 1.00 ,
     +             0.00 , 0.50 , 1.00 ,
     +             0.00 , 1.00 , 1.00 ,
     +             0.00 , 1.00 , 0.60 ,
     +             0.00 , 1.00 , 0.00 ,
     +             0.70 , 1.00 , 0.00 ,
     +             1.00 , 1.00 , 0.00 ,
     +             1.00 , 0.75 , 0.00 ,
     +             1.00 , 0.38 , 0.38 ,
     +             1.00 , 0.00 , 0.38 ,
     +             1.00 , 0.00 , 0.00 /
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
          CALL GSCR (IWKID,J,RGB(1,I),RGB(2,I),RGB(3,I))
  101   CONTINUE
C
C Set color index 15 to black.
C
        CALL GSCR (IWKID,15,0.,0.,0.)
C
C Set up EZMAP.
C
        CALL MAPSTI ('MV',1)
        CALL MAPSTC ('OU','PO')
        CALL MAPROJ ('ME',0.,0.,0.)
        CALL MAPSET ('CO',30.,-15.,60.,30.)
C
C Make MPLNAM use 1 and 2 as the group identifiers.
C
        CALL MAPSTI ('G1',1)
        CALL MAPSTI ('G2',2)
C
C Use 5 vertical strips to reduce the number of points defining the
C sub-areas.
C
        CALL MAPSTI ('VS',5)
C
C Initialize EZMAP.
C
        CALL MAPINT
C
C Initialize the area map.
C
        CALL ARINAM (IAMA,100000)
C
C Add edges to the area map.
C
        CALL MPLNAM ('Earth..1',3,IAMA)
C
C Pre-process the area map.
C
        CALL ARPRAM (IAMA,0,0,0)
C
C Compute and print the amount of space used in the area map.
C
        ISU=IAMA(1)-(IAMA(6)-IAMA(5)-1)
        PRINT * , 'SPACE USED IN AREA MAP IS ',ISU
C
C Color the map.
C
        CALL ARSCAM (IAMA,XCRA,YCRA,2000,IAAI,IAGI,2,COLRAM)
C
C Flush PLOTIT's buffers and set polyline color index to black.
C
        CALL PLOTIT (0,0,0)
        CALL GSPLCI (15)
C
C In black, draw a perimeter and outline all the countries.
C
        CALL MAPSTI ('LA',0)
        CALL MAPSTI ('MV',1)
        CALL MAPLBL
        CALL MPLNDR ('Earth..1',3)
C
C Draw lines of latitude and longitude over water.
C
        CALL MAPGRM (IAMA,XCRA,YCRA,2000,IAAI,IAGI,2,COLRLN)
C
C Advance the frame.
C
        CALL FRAME
C
C Done.
C
        IERR=0
        WRITE (6,1001)
C
        RETURN
C
 1001   FORMAT ('  EZMAPB TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END



      SUBROUTINE COLRAM (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C This is the routine that color-fills the areas defined by the area map.
C First, ITMP is set non-zero if and only if no area identifiers for the
C area are negative.
C
        ITMP=1
C
        DO 101 I=1,NGPS
          IF (IAAI(I).LT.0) ITMP=0
  101   CONTINUE
C
C Then, if ITMP is non-zero (which says that we really do want to color-
C fill the area) ...
C
        IF (ITMP.NE.0) THEN
C
C set ITMP to equal to the value of the area identifier for the area
C relative to group 1 and ...
C
          ITMP=0
C
          DO 102 I=1,NGPS
            IF (IAGI(I).EQ.1) ITMP=IAAI(I)
  102     CONTINUE
C
C if that area identifier is greater than zero, ...
C
          IF (ITMP.GT.0) THEN
C
C find the index of the suggested color for the area, set the fill area
C color index, and fill the area.
C
            CALL GSFACI (MPISCI(ITMP))
C
            CALL GFA (NCRA-1,XCRA,YCRA)
C
          END IF
C
        END IF
C
        RETURN
C
      END



      SUBROUTINE COLRLN (XCRA,YCRA,NCRA,IAAI,IAGI,NGPS)
C
        DIMENSION XCRA(*),YCRA(*),IAAI(*),IAGI(*)
C
C This is the routine that draws lines of latitude and longitude over
C water only.  First, ITMP is set non-zero if and only if no area
C identifiers for the area are negative.
C
        ITMP=1
C
        DO 101 I=1,NGPS
          IF (IAAI(I).LT.0) ITMP=0
  101   CONTINUE
C
C Then, if ITMP is non-zero (which says that we really do want to draw
C things in the area) ...
C
        IF (ITMP.NE.0) THEN
C
C set ITMP to equal to the value of the area identifier for the area
C relative to group 1 (EZMAP lines) ...
C
          ITMP=0
C
          DO 102 I=1,NGPS
            IF (IAGI(I).EQ.1) ITMP=IAAI(I)
  102     CONTINUE
C
C if the suggested color for the area implies that it is over water
C (the color index 1 is used only for water) ...
C
          IF (MPISCI(ITMP).EQ.1) THEN
C
C flush PLOTIT's buffers, set the polyline color index to black, and
C draw the line.
C
            CALL PLOTIT (0,0,0)
            CALL GSPLCI (15)
C
            CALL GPL (NCRA,XCRA,YCRA)
C
          END IF
C
        END IF
C
        RETURN
C
      END
