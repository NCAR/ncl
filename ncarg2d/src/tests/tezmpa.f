C
C	$Id: tezmpa.f,v 1.1.1.1 1992-04-17 22:33:29 ncargd Exp $
C
      SUBROUTINE TEZMPA (IERROR)
C
C PURPOSE                To provide a simple demonstration of the use
C                        of EZMAPA.
C
C USAGE                  CALL TEZMPA (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C
C                          an error parameter
C                          = 0, if the test is successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C                          EZMAPA TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is written on unit 6.
C
C PRECISION              Single.
C
C REQUIRED LIBRARY       EZMAP, AREAS, SPPS
C FILES
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written in JUNE, 1987.
C
C ALGORITHM              TEZMAP draws a solid-color map of a portion
C                        of Europe.
C
C PORTABILITY            FORTRAN 77
C
C
C Define an array in which to construct the area map.
C
        DIMENSION IAM(25000)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for x/y coordinates.
C
        DIMENSION XCS(200),YCS(200)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for area and group
C identifiers.
C
        DIMENSION IAI(2),IAG(2)
C
C Define the RGB color triples needed below.
C
        DIMENSION RGB(3,14)
C
C Define a set of indices to order the colors.
C
        DIMENSION IOC(14)
C
C Define an array for GKS aspect source flags.
C
        DIMENSION IF(13)
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
C Set the aspect source flags for FILL AREA INTERIOR STYLE and for
C FILL AREA STYLE INDEX to "individual".
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
          CALL GSCR(1,J,RGB(1,I),RGB(2,I),RGB(3,I))
  101   CONTINUE
C
C Set color index 15 to black.
C
        CALL GSCR(1,15,0.,0.,0.)
C
C Set up EZMAP.
C
        CALL MAPSTI ('MV',1)
        CALL MAPSTC ('OU','PO')
        CALL MAPROJ ('ME',0.,0.,0.)
        CALL MAPSET ('CO',30.,-15.,60.,30.)
C
C Make MAPBLA use 1 and 2 as the group identifiers.
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
        CALL ARINAM (IAM,25000)
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
        ISU=25000-(IAM(6)-IAM(5)-1)
        PRINT * , 'SPACE USED IN AREA MAP IS ',ISU
C
C Color the map.
C
        CALL ARSCAM (IAM,XCS,YCS,200,IAI,IAG,2,COLRAM)
C
C Flush PLOTIT's buffers and set polyline color index to black.
C
        CALL PLOTIT(0,0,0)
        CALL GSPLCI(15)
C
C In black, draw a perimeter and outline all the countries.
C
        CALL MAPSTI ('LA',0)
        CALL MAPSTI ('MV',1)
        CALL MAPLBL
        CALL MAPLOT
C
C Draw lines of latitude and longitude over water.
C
        CALL MAPGRM (IAM,XCS,YCS,200,IAI,IAG,2,COLRLN)
C
C Advance the frame.
C
        CALL FRAME
C
C Done.
C
        IERROR=0
        WRITE (6,1001)
C
        RETURN
C
 1001   FORMAT ('  EZMAPA TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
      SUBROUTINE COLRAM (XCS,YCS,NCS,IAI,IAG,NAI)
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
        ITM=1
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) ITM=0
  101   CONTINUE
        IF (ITM.NE.0) THEN
          ITM=0
          DO 102 I=1,NAI
            IF (IAG(I).EQ.1) ITM=IAI(I)
  102     CONTINUE
          IF (ITM.GT.0) THEN
C
C  Set area fill color index.
C
            CALL GSFACI(MAPACI(ITM))
C
            CALL GFA (NCS-1,XCS,YCS)
          END IF
        END IF
        RETURN
      END
      SUBROUTINE COLRLN (XCS,YCS,NCS,IAI,IAG,NAI)
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
        ITM=1
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) ITM=0
  101   CONTINUE
        IF (ITM.NE.0) THEN
          ITM=0
          DO 102 I=1,NAI
            IF (IAG(I).EQ.1) ITM=IAI(I)
  102     CONTINUE
          IF (MAPACI(ITM).EQ.1) THEN
C
C Flush PLOTIT's buffers and set polyline color index to black.
C
            CALL PLOTIT(0,0,0)
            CALL GSPLCI(15)
C
            CALL GPL (NCS,XCS,YCS)
          END IF
        END IF
        RETURN
      END
