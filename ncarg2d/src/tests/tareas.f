
      PROGRAM TAREAS
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
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL AREAS(IERR,IWKID)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE AREAS (IERROR, IWKID)
C
C PURPOSE                To provide a simple demonstration of the use
C                        of AREAS.
C
C USAGE                  CALL AREAS (IERROR)
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
C                          AREAS TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is written on unit 6.
C
C PRECISION              Single.
C
C REQUIRED LIBRARY       AREAS, SPPS
C FILES
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written in JUNE, 1987.
C
C ALGORITHM              TAREAS constructs and colors a very simple
C                        picture illustrating the use of all of the
C                        routines in the package.
C
C PORTABILITY            FORTRAN 77
C
C Define an array in which to construct the area map.
C
        DIMENSION IAM(5000)
C
C Define the arrays needed for edge-coordinate data.
C
        DIMENSION XCA(73),YCA(73)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for X/Y coordinates.
C
        DIMENSION XCS(150),YCS(150)
C
C Dimension the arrays needed by ARSCAM and ARDRLN for area and group
C identifiers.
C
        DIMENSION IAI(2),IAG(2)
C
C Define the RGB color triples needed below.
C
        DIMENSION RGB(3,15)
C
C Define a set of indices to order the colors.
C
        DIMENSION IOC(15)
C
C Define an array for GKS aspect source flags.
C
        DIMENSION IF(13)
C
C Declare the routine which will color the areas.
C
        EXTERNAL COLRAM
C
C Declare the routine which will draw lines over the circle.
C
        EXTERNAL COLRLN
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
     +             1.00 , 0.00 , 0.00 ,
     +             1.00 , 1.00 , 1.00 /
        DATA IOC / 6,2,5,12,10,11,1,3,4,8,9,7,13,14,15 /
C
C Declare the constant for converting from degrees to radians.
C
        DATA DTR / .017453292519943 /
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
C Define 15 different color indices.
C
        CALL GSCR(IWKID,0,0.,0.,0.)
        DO 101 J=1,15
          I=IOC(J)
          CALL GSCR(IWKID,J,RGB(1,I),RGB(2,I),RGB(3,I))
  101   CONTINUE
C
C Initialize the area map.
C
        CALL ARINAM (IAM,5000)
C
C Add edges to the area map.
C
        CALL SET (.01,.99,.01,.99,-1.,1.,-1.,1.,1)
C
C First, define a circle, using group 1 edges.  After this step, the
C area inside the circle has area identifier zero and the area outside
C has area identifier -1.
C
        DO 102 ING=1,73
        ANG=DTR*REAL(5*(ING-1))
        XCA(ING)=COS(ANG)
        YCA(ING)=SIN(ANG)
  102   CONTINUE
        CALL AREDAM (IAM,XCA,YCA,73,1,0,-1)
C
C Add lines splitting the circle into wedges.  The area identifiers
C for the wedges are added to the area map with this step.
C
        XCA(1)=0.
        YCA(1)=0.
        DO 103 ING=1,15
          ANG=DTR*REAL(24*(ING-1))
          XCA(2)=COS(ANG)
          YCA(2)=SIN(ANG)
          CALL AREDAM (IAM,XCA,YCA,2,1,ING,MOD(ING+13,15)+1)
  103   CONTINUE
C
C Now, put in another, smaller, off-center circle, using a group 2
C edge.  The interior of the circle has area identifier 1 and the
C exterior of the circle has group identifier 2.
C
        DO 104 ING=1,73
        ANG=DTR*REAL(5*(ING-1))
        XCA(ING)=.25+.5*COS(ANG)
        YCA(ING)=.25+.5*SIN(ANG)
  104   CONTINUE
        CALL AREDAM (IAM,XCA,YCA,73,2,1,2)
C
C Pre-process the area map.
C
        CALL ARPRAM (IAM,0,0,0)
C
C Compute and print the amount of space used in the area map.
C
        ISU=5000-(IAM(6)-IAM(5)-1)
        PRINT * , 'SPACE USED IN AREA MAP IS ',ISU
C
C Color the areas defined.
C
        CALL ARSCAM (IAM,XCS,YCS,150,IAI,IAG,2,COLRAM)
C
C In contrasting colors, draw three stars on the plot.
C
        DO 105 I=1,3
          IF (I.EQ.1) THEN
            XCN=-.5
            YCN=+.5
          ELSE IF (I.EQ.2) THEN
            XCN=-.5
            YCN=-.5
          ELSE IF (I.EQ.3) THEN
            XCN=+.5
            YCN=-.5
          END IF
          XCA(1)=XCN+.25*COS( 162.*DTR)
          YCA(1)=YCN+.25*SIN( 162.*DTR)
          XCA(2)=XCN+.25*COS(  18.*DTR)
          YCA(2)=YCN+.25*SIN(  18.*DTR)
          XCA(3)=XCN+.25*COS(-126.*DTR)
          YCA(3)=YCN+.25*SIN(-126.*DTR)
          XCA(4)=XCN+.25*COS(  90.*DTR)
          YCA(4)=YCN+.25*SIN(  90.*DTR)
          XCA(5)=XCN+.25*COS( -54.*DTR)
          YCA(5)=YCN+.25*SIN( -54.*DTR)
          XCA(6)=XCN+.25*COS( 162.*DTR)
          YCA(6)=YCN+.25*SIN( 162.*DTR)
          CALL ARDRLN (IAM,XCA,YCA,6,XCS,YCS,150,IAI,IAG,2,COLRLN)
  105   CONTINUE
C
C Draw a spiral of points in the blanked-out circle, using the colors
C from edge group 1.
C
        ICF=1
        DO 108 ING=1,1500
          RAD=REAL(ING)/1000.
          ANG=DTR*REAL(ING-1)
          XCD=.25+.5*RAD*COS(ANG)
          YCD=.25+.5*RAD*SIN(ANG)
          CALL ARGTAI (IAM,XCD,YCD,IAI,IAG,2,NAI,ICF)
          ITM=1
          DO 106 I=1,NAI
            IF (IAI(I).LT.0) ITM=0
  106     CONTINUE
          IF (ITM.NE.0) THEN
            IT1=0
            IT2=0
            DO 107 I=1,NAI
              IF (IAG(I).EQ.1) IT1=IAI(I)
              IF (IAG(I).EQ.2) IT2=IAI(I)
  107       CONTINUE
            IF (IT1.GT.0.AND.IT2.EQ.1) THEN
C
C Flush PLOTIT's buffers and set polyline color index.
C
              CALL PLOTIT(0,0,0)
              CALL GSPLCI(IT1)
C
              CALL POINT (XCD,YCD)
            END IF
          END IF
          ICF=0
  108   CONTINUE
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
 1001   FORMAT ('  AREAS TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
      SUBROUTINE COLRAM (XCS,YCS,NCS,IAI,IAG,NAI)
        DIMENSION XCS(*),YCS(*),IAI(*),IAG(*)
        ITM=1
        DO 101 I=1,NAI
          IF (IAI(I).LT.0) ITM=0
  101   CONTINUE
        IF (ITM.NE.0) THEN
          IT1=0
          DO 102 I=1,NAI
            IF (IAG(I).EQ.1) IT1=IAI(I)
            IF (IAG(I).EQ.2) IT2=IAI(I)
  102     CONTINUE
          IF (IT1.GT.0.AND.IT2.NE.1) THEN
C
C Set fill area color index.
C
            CALL GSFACI(IT1)
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
          IT1=0
          IT2=0
          DO 102 I=1,NAI
            IF (IAG(I).EQ.1) IT1=IAI(I)
            IF (IAG(I).EQ.2) IT2=IAI(I)
  102     CONTINUE
          IF (IT1.GT.0.AND.IT2.NE.1) THEN
C
C Flush PLOTIT's buffers and set polyline color index.
C
            CALL PLOTIT(0,0,0)
            CALL GSPLCI(MOD(IT1+3,15)+1)
C
            CALL GPL (NCS,XCS,YCS)
          END IF
        END IF
        RETURN
      END

