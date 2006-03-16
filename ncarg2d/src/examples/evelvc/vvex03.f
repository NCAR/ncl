
      PROGRAM VVEX03
C
C
C This example illustrates use of the user-modifiable routine
C VVUMXY to create user-defined mappings of vector data.
C The first frame demonstrates a method of plotting data contained
C  in an irregularly spaced rectangular grid. 
C The second frame plots scattered (non-gridded) data.
C The third frame maps scattered data through an EZMAP projection.
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
C Open GKS, open workstation of type 1, activate workstation
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Draw an irregularly gridded vector plot.
C
      CALL IRREX
C
C Draw a scattered vector plot.
C
      CALL SCTREX
C
C Draw a plot of scattered vectors mapped through an Ezmap projection.
C
      CALL SCEZEX
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
C =============================================================
C
      SUBROUTINE IRREX
C
C     
      PARAMETER (M=10,N=10)
      DIMENSION U(M,N), V(M,N)
C
C User-defined common block containing irregular grid coordinates.
C
      PARAMETER (PI=3.14159)
      PARAMETER (MAXDIM = 100)
      COMMON /VVUSER/ XCOORD(MAXDIM),YCOORD(MAXDIM)
      SAVE /VVUSER/
C
C Local arrays to hold the coordinate data.
C
      DIMENSION XCLOC(M), YCLOC(N)
C
      DATA XGRID / 100.0 /
      DATA YGRID / 100.0 /
      DATA XCLOC / 0.0,7.5,20.0,40.0,45.0,65.0,70.0,77.0,90.0,95.0 /
      DATA YCLOC / 12.5,22.5,30.0,35.0,52.0,58.0,70.0,75.0,85.0,90.0 /
C
C
C Specify the NDC coordinates for a plot title.
C
        DATA FX / 0.5 /, FY / 0.975 /
C
C Set up a functionally defined vector field
C
      GXSIZE = PI/XGRID
      GYSIZE = PI/YGRID
      DO  20 I=1,M
         DO  10 J=1,N
            U(I,J) = COS(GXSIZE*(XCLOC(I)))
            V(I,J) = SIN(GYSIZE*(YCLOC(J)))
   10    CONTINUE
   20 CONTINUE
C
C Copy coordinate values into the VVUSER common block arrays, in
C order to make the data accessible to VVUMXY.
C
      DO 30 I=1,M
         XCOORD(I) = XCLOC(I)
 30   CONTINUE
C
      DO 40 I=1,N
         YCOORD(I) = YCLOC(I)
 40   CONTINUE
C
C Set the user space (arguments 5,6,7, and 8 of the SET call) to contain 
C the extremes of the values assigned to the coordinate arrays; then
C tell Vectors not to do a SET call.
C 
         CALL SET(0.05,0.95,0.05,0.95,0.0,100.0,0.0,100.0,1)
         CALL VVSETI('SET - Do-SET-Call Flag', 0)
C
C Set the data coordinate boundaries equal to the grid coordinate
C boundaries (i.e. the range of the array indexes). This is actually
C the default condition for Vectors, but is included for emphasis here.
C
         CALL VVSETR('XC1 -- Lower X Bound', 1.0)
         CALL VVSETR('XCM -- Upper X Bound', REAL(M))
         CALL VVSETR('YC1 -- Lower Y Bound', 1.0)
         CALL VVSETR('YCN -- Upper Y Bound', REAL(N))
C
C Set the MAP parameter to a value indicating a user-defined mapping
C (anything other than 0, 1, or 2). The routine VVUMXY must be modified
C to recognize this value for MAP.
C
         CALL VVSETI('MAP - user-defined mapping', 3) 
         IDM=0
         RDM=0.0
         CALL VVINIT(U,M,V,M,RDM,IDM,M,N,RDM,IDM)
C
C For an irregular grid the default vector sizes may not be appropriate.
C The following (admittedly cumbersome) code adjusts the size relative 
C to the default size chosen by Vectors. It must follow VVINIT.
C
         VSZADJ = 0.75
         CALL VVGETR('DMX - NDC Maximum Vector Size', DMX)
         CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
         VRL = VSZADJ * DMX / (VR - VL)
         CALL VVSETR('VRL - Vector Realized Length', VRL)
C
         CALL VVECTR(U,V,RDM,IDM,IDM,RDM)
         CALL PERIM(1,0,1,0)
C
C Save the current normalization transformation then set it to 0.
C
         CALL GQCNTN(IERR,ICN)
         CALL GSELNT(0)
         X = CFUX(FX)
         Y = CFUY(FY)
C
C Call PLCHLQ to write the plot title.
C
         CALL PLCHLQ (X,Y,
     1           'Irregularly gridded vector data',
     2           16.,0.,0.)
C
C Restore the normalization transformation.
C
         CALL GSELNT(ICN)
         CALL FRAME
C
 100  CONTINUE
C
      RETURN
C
      END
C
C ==================================================================
C
      SUBROUTINE SCTREX
C
C In this routine the U and V arrays are treated as single-dimensioned
C entities. The location of the vector with components U(I),V(I) is
C specified by XCLOC(I),YCLOC(I). The coordinate locations are copied
C to the common block VVUSER in order to make them accessible to the
C the user-defined mapping routine, VVUMXY.
C
C Since Vectors expects 2-D arrays for U and V, the input parameters
C M and N multiplied together should equal the total number of elements 
C in each of the 1-D data arrays.
C  
      PARAMETER (M=10,N=1)
      DIMENSION U(M*N), V(M*N)
C
C User-defined common block containing X and Y locations of each 
C vector.
C
      PARAMETER (MAXDIM = 100)
      COMMON /VVUSER/ XCOORD(MAXDIM),YCOORD(MAXDIM)
      SAVE /VVUSER/
C
C All data lies within a 100 x 100 square (origin 0.0 at lower left).
C
      DIMENSION XCLOC(M*N), YCLOC(M*N)
C
      DATA XCLOC / 40.0,7.5,20.0,35.0,45.0,65.0,75.0,80.0,90.0,95.0 /
      DATA YCLOC / 50.0,80.0,20.0,60.0,15.0,80.0,20.0,45.0,60.0,50.0 /
      DATA U / .3,.6,.8,.2,.9,.8,-.4,-.3,.1,.2 /
      DATA V / -.2,.1,.7,.6,.3,-.4,-.6,-.8,-.9,-.5 /
C
C
C Specify the NDC coordinates for a plot title.
C
        DATA FX / 0.5 /, FY / 0.975 /
C
C Copy coordinate values into the VVUSER common block arrays.
C
      DO 30 I=1,M*N
         XCOORD(I) = XCLOC(I)
 30   CONTINUE
C
      DO 40 I=1,M*N
         YCOORD(I) = YCLOC(I)
 40   CONTINUE
C
C Set the user space (arguments 5,6,7, and 8 of the SET call) to contain 
C the extremes of the values assigned to the coordinate arrays; then
C tell Vectors not to do a SET call.
C 
         CALL SET(0.075,0.925,0.1,0.95,0.0,100.0,0.0,100.0,1)
         CALL VVSETI('SET - Do-SET-Call Flag', 0)
C
C Set the data coordinate boundaries equal to the grid coordinate
C boundaries (i.e. the range of the array indexes). This is actually
C the default condition for Vectors, but is included for emphasis here.
C
         CALL VVSETR('XC1 -- Lower X Bound', 1.0)
         CALL VVSETR('XCM -- Upper X Bound', REAL(M))
         CALL VVSETR('YC1 -- Lower Y Bound', 1.0)
         CALL VVSETR('YCN -- Upper Y Bound', REAL(N))
C
C Set the MAP parameter to a value indicating a user-defined mapping
C (anything other than 0, 1, or 2). The routine VVUMXY must be modified
C to recognize this value for MAP.
C
         CALL VVSETI('MAP - user-defined mapping', 4) 
         IDM=0
         RDM=0.0
         CALL VVINIT(U,M,V,M,RDM,IDM,M,N,RDM,IDM)
C
C For an scattered grid the default vector sizes may not be appropriate.
C The following (admittedly cumbersome) code adjusts the size relative 
C to the default size chosen by Vectors. It must follow VVINIT.
C
         VSZADJ = 0.1
         CALL VVGETR('DMX - NDC Maximum Vector Size', DMX)
         CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
         VRL = VSZADJ * DMX / (VR - VL)
         CALL VVSETR('VRL - Vector Realized Length', VRL)
C
         CALL VVECTR(U,V,RDM,IDM,IDM,RDM)
         CALL PERIM(1,0,1,0)
C
C
C Save the current normalization transformation then set it to 0.
C
         CALL GQCNTN(IERR,ICN)
         CALL GSELNT(0)
         X = CFUX(FX)
         Y = CFUY(FY)
C
C Call PLCHLQ to write the plot title.
C
         CALL PLCHLQ (X,Y,
     1           'Scattered vector data',
     2           16.,0.,0.)
C
C Restore the normalization transformation.
C
         CALL GSELNT(ICN)
         CALL FRAME
C
 100  CONTINUE
C
      RETURN
C
      END
C
C =================================================================
C
      SUBROUTINE SCEZEX
C
C This routine maps geographically scattered vector data
C through an Ezmap projection. In order to make the effect of the 
C projection more obvious the vectors are given uniform direction 
C and magnitude.
C  
      PARAMETER (M=20,N=1)
      DIMENSION U(M*N), V(M*N)
C
C User-defined common block containing X and Y locations of each 
C vector.
C
      PARAMETER (MAXDIM = 100)
      COMMON /VVUSER/ XCOORD(MAXDIM),YCOORD(MAXDIM)
      SAVE /VVUSER/
C
C All data lies within the region latitude 0.0,90.0 and 
C and longitude 0.0,100.0.
C
      DIMENSION XCLOC(M*N), YCLOC(M*N)
C
      DATA XCLOC / 40.0,7.5,20.0,35.0,40.0,65.0,75.0,80.0,90.0,95.0, 
     +     50.0,80.0,20.0,60.0,15.0,80.0,20.0,45.0,60.0,50.0 /
      DATA YCLOC / 50.0,80.0,20.0,60.0,15.0,80.0,20.0,45.0,60.0,50.0, 
     +     40.0,7.5,20.0,35.0,45.0,65.0,75.0,13.0,24.0,32.0 /
      DATA U / 20 * 1.0 /
      DATA V / 20 * 1.0 /
C
C
C Specify the NDC coordinates for a plot title.
C
        DATA FX / 0.5 /, FY / 0.975 /
C
C Copy coordinate values into the VVUSER common block arrays.
C
      DO 30 I=1,M*N
         XCOORD(I) = XCLOC(I)
 30   CONTINUE
C
      DO 40 I=1,M*N
         YCOORD(I) = YCLOC(I)
 40   CONTINUE
C
C Set up a Satellite view EZMAP projection. Draw the map grid only.
C
      CALL MAPSET('MA',0.0,0.0,0.0,0.0)
      CALL MAPROJ('SV',45.0,50.0,0.0)
      CALL MAPINT
      CALL MAPGRD
C
C Don't let Vectors do a SET call
C
      CALL VVSETI('SET - Do-SET-Call Flag', 0)
C
C Set the data coordinate boundaries equal to the grid coordinate
C boundaries (i.e. the range of the array indexes). This is actually
C the default condition for Vectors, but is included for emphasis here.
C
      CALL VVSETR('XC1 -- Lower X Bound', 1.0)
      CALL VVSETR('XCM -- Upper X Bound', REAL(M))
      CALL VVSETR('YC1 -- Lower Y Bound', 1.0)
      CALL VVSETR('YCN -- Upper Y Bound', REAL(N))
C
C Set the MAP parameter to a value indicating a user-defined mapping
C (anything other than 0, 1, or 2). The routine VVUMXY must be modified
C to recognize this value for MAP.
C
      CALL VVSETI('MAP - user-defined mapping', 5) 
      CALL VVSETR('LWD - vector linewidth', 2.0) 
      IDM=0
      RDM=0.0
      CALL VVINIT(U,M,V,M,RDM,IDM,M,N,RDM,IDM)
C
C Adjust vector sizes.
C
      VSZADJ = 0.1
      CALL VVGETR('DMX - NDC Maximum Vector Size', DMX)
      CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
      VRL = VSZADJ * DMX / (VR - VL)
      CALL VVSETR('VRL - Vector Realized Length', VRL)
C
      CALL VVECTR(U,V,RDM,IDM,IDM,RDM)
      CALL PERIM(1,0,1,0)
C
C
C Save the current normalization transformation then set it to 0.
C
      CALL GQCNTN(IERR,ICN)
      CALL GSELNT(0)
      X = CFUX(FX)
      Y = CFUY(FY)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ (X,Y,
     1     'Scattered vector data mapped through an Ezmap projection',
     2     16.,0.,0.)
C
C Restore the normalization transformation.
C
      CALL GSELNT(ICN)
      CALL FRAME
C
 100  CONTINUE
C
      RETURN
C
      END
C
C =================================================================
C
C Modified version of VVUMXY that implements mapping for the 
C following values of the Vectors 'MAP' internal parameter:
C 
C 3 - irregular rectangular grid mapping
C 4 - scattered data mapping
C 5 - scattered data mapped through an Ezmap projection.
C
      SUBROUTINE VVUMXY (X,Y,U,V,UVM,XB,YB,XE,YE,IST)
C
C This is a user modifiable routine that allows custom projections of
C the vector space. X and Y give the vector position within the domain
C of the data space. By default, this space is coincident with the
C grid space (i.e. 1 through dimension lengths of the U and V arrays).
C The vector endpoints are output in fractional coordinates (NDC space).
C Note that this is different from the old MXF and MYF routines, which
C output in 'plotter coordinate' space. It also differs from the 
C Conpack routine CPMPXY, which returns values in user space. 
C 
C VVUMXY (Velocity Vector -- User Map X,Y) is called whenever 
C the internal parameter MAP is set to a value other than 0, 1, or 2.
C
C Based on the magnitude and direction of the vector the start and 
C ending points of the vector are returned in NDC space.
C
C Input parameters:
C
C X,Y   -- vector position in the user coordinate system
C U,V   -- vector components from the U,V arrays for this position
C UVM   -- magnitude of the U,V components (supplied for convenience
C          and efficiency - but note that many mappings do not need 
C          this value)
C
C Output parameters:
C
C XB,YB -- starting point of the vector in fractional coordinates
C          (NDC space)
C XE,YE -- ending point of the vector in fractional coordinates
C          (NDC space)
C IST   -- status results of the mapping: 0 indicates success -- any
C          non-zero value causes VVECTR to discard the vector at this
C          location
C
C The mapping common block: made available to user mapping routines.
C NOTE: all these variables should be considered read-only by VVUMXY.
C
      COMMON /VVMAP/
     +                IMAP       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                SXDC       ,SYDC       ,NXCT       ,NYCT       ,
     +                RLEN       ,LNLG       ,INVX       ,INVY       ,
     +                ITRT       ,IWCT       ,FW2W       ,FH2H       ,
     +                DVMN       ,DVMX       ,RBIG       ,IBIG
C
      SAVE /VVMAP/
C
C Description of VVMAP contents:
C
C IMAP                - value of the internal parameter 'MAP'
C XVPL,XVPR,YVPB,YVPT - the currently set viewport values. (GETSET
C                       arguments 1, 2, 3, and 4)
C WXMN,WXMX,WYMN,WYMX - the min and max boundaries of user coordinate
C                       space, (usually but not always equivalent to
C                       window coordinates). WXMN and WYMN are true
C                       minimum values even one or both axes is 
C                       inverted. (i.e. they are equivalent to GETSET
C                       arguments 5,6,7, and 8 sorted numerically)
C XLOV,XHIV,YLOV,YHIV - min and max boundaries of the data space, by
C                       default equivalent to the array grid space.
C                       XLOV and YLOV are not necessarily less than 
C                       XHIV and YHIV.
C SXDC,SYDC           - Scaling factors for converting vector component
C                       values into lengths in NDC space.
C NXCT,NYCT           - Length of each dimension of the U and V 
C                       component arrays.
C RLEN                - Length of the maximum vector in user 
C                       coordinates.
C LNLG                - The linear/log mode (GETSET argument 9)
C INVX,INVY           - User coordinates inversion flags: 
C                       0 - not inverted, 1 - inverted
C ITRT                - value of the internal parameter TRT
C IWCT                - not currently used
C FW2W,FH2H           - scale factors for converting from fraction of
C                       viewport width/height to NDC width/height 
C DVMN,DVMX           - min/max vector lengths in NDC
C RBIG,IBIG           - machine dependent maximum REAL/INTEGER values
C
C Math constants:
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C --------------------------------------------------------------------
C User-defined common block:
C
      PARAMETER (MAXDIM = 100)
C
      COMMON /VVUSER/ XCOORD(MAXDIM), YCOORD(MAXDIM)
      SAVE /VVUSER/
C --------------------------------------------------------------------
C Local parameters (used for the Ezmap projection only):
C
C PRCFAC - Precision factor used to resolve float equality within
C            the precision of a 4 byte REAL
C PVFRAC - Initial fraction of the vector magnitude used to
C            determine the differential increment
C PFOVFL - Floating point overflow value
C IPMXCT - Number of times to allow the differential to increase
C PDUVML - Multiplier when the differential is increased
C PCSTST - Test value for closeness to 90 degree latitude
C
      PARAMETER (PRCFAC=1E5,
     +           PVFRAC=0.001,
     +           PFOVFL=1E12,
     +           IPMXCT=40,
     +           PDUVML=2.0,
     +           IPCTST=PRCFAC*90)
C
C ---------------------------------------------------------------------
C
      IF (IMAP .EQ. 3) THEN
C
C Mapping for irregular rectangular gridded vector data.
C 
C Since the array grid and input data space are coincident in this
C case, X and Y converted to integers serve as the index into the 
C coordinate arrays that define the vector location in the user
C coordinate system. 
C This code includes more tests that may be necessary in
C production environments: it does so partly to illustrate how to use
C some of the contents of the VVMAP common block.
C
         I = NINT(X)
         J = NINT(Y)
C
C NXCT and NYCT contain the number of elements along each coordinate
C axis. Therefore the following test ensures that I and J are within
C the domain of the array dimensions.
C
         IF (I.LT.1 .OR. I.GT.NXCT .OR. J.LT.1 .OR. J.GT.NYCT) THEN
            IST = -1
            RETURN
         END IF
         XC = XCOORD(I)
         YC = YCOORD(J)
C
C WXMN, WXMX, WYMN, and WYMX contain the minimum and maximum values of
C the user coordinate space. The following test ensures that the 
C coordinate values in the array are within the current boundaries
C of the user space.
C
         IF (XC.LT.WXMN .OR. XC.GT.WXMX .OR. 
     +        YC.LT.WYMN .OR. YC.GT.WYMX) THEN
            IST = -1
            RETURN
         END IF
         XB=CUFX(XC)
         YB=CUFY(YC)
         XE=XB+U*SXDC
         YE=YB+V*SYDC
C
C ---------------------------------------------------------------------
C
      ELSE IF (IMAP .EQ. 4) THEN
C
C Mapping for scattered vector data.
C 
         I = NINT(X)
         J = NINT(Y)
C
         IF (I.LT.1 .OR. I.GT.NXCT .OR. J.LT.1 .OR. J.GT.NYCT) THEN
            IST = -1
            RETURN
         END IF
C
C Since XCOORD and YCOORD are actually single dimensional arrays,
C convert the 2-d indexes supplied to VVUMXY into their 1-d equivalent 
C to index into the coordinate arrays.
C
         XC = XCOORD(NXCT*(J-1)+I)
         YC = YCOORD(NXCT*(J-1)+I)
C
         IF (XC.LT.WXMN .OR. XC.GT.WXMX .OR. 
     +        YC.LT.WYMN .OR. YC.GT.WYMX) THEN
            IST = -1
            RETURN
         END IF
         XB=CUFX(XC)
         YB=CUFY(YC)
         XE=XB+U*SXDC
         YE=YB+V*SYDC
C
C ---------------------------------------------------------------------
C
      ELSE IF (IMAP .EQ. 5) THEN
C
C Mapping for scattered vector data projected through Ezmap. XCOORD and
C YCOORD contain the Longitude and Latitude respectively of each vector
C datum. 
C  
C 
         I = NINT(X)
         J = NINT(Y)
C
         IF (I.LT.1 .OR. I.GT.NXCT .OR. J.LT.1 .OR. J.GT.NYCT) THEN
            IST = -1
            RETURN
         END IF
C
C Since XCOORD and YCOORD are actually single dimensional arrays,
C convert the 2-d indexes supplied to VVUMXY into their 1-d equivalent 
C to index into the coordinate arrays.
C
         XC = XCOORD(NXCT*(J-1)+I)
         YC = YCOORD(NXCT*(J-1)+I)
C
C The following code is adapted from the Ezmap projection code in 
C VVMPXY. An iterative technique is used that handles most vectors 
C arbitrarily close to the projection limb.
C XC is longitude, YC is latitude.
C
C Test for 90 degree latitude.
C
         IF (INT(ABS(YC)*PRCFAC+0.5).EQ.IPCTST) THEN
            IST=-1
            RETURN
         END IF
C
C Project the starting value: bail out if outside the window
C
         CALL MAPTRA (YC,XC,XB,YB)
         IF (XB .LT. WXMN .OR. XB .GT. WXMX .OR.
     +       YB .LT. WYMN .OR. YB .GT. WYMX) THEN
            IST=-5
            RETURN
         END IF
C
C Check the vector magnitude
C
         IF (INT(UVM*PRCFAC+0.5) .EQ. 0) THEN
            IST=-2
            RETURN
         END IF
C
C The incremental distance is proportional to a small fraction
C of the vector magnitude
C
         DUV=PVFRAC/UVM
         CLT=COS(YC*PDTOR)
C
C Project the incremental distance. If the positive difference doesn't
C work, try the negative difference. If the difference results in a
C zero length vector, try a number of progressively larger increments.
C
         ICT=0
         SGN=1.0
 20      CONTINUE
C
         CALL MAPTRA(YC+SGN*V*DUV,XC+SGN*U*DUV/CLT,XT,YT)
C
         DV1=SQRT((XT-XB)*(XT-XB)+(YT-YB)*(YT-YB))
         IF (DV1 .GT. RLEN) THEN
            IF (SGN .EQ. -1.0) THEN
               IST=-4
               RETURN
            ELSE
               SGN=-1.0
               GO TO 20
            END IF
         END IF
C
         IF (INT(DV1*PRCFAC) .EQ. 0) THEN
            IF (ICT .LT. IPMXCT) THEN
               ICT = ICT + 1
               DUV=DUV*PDUVML
               GO TO 20
            ELSE
               IST=-3
               RETURN
            END IF
         END IF
C
         IF (ABS(XT) .GE. PFOVFL .OR. ABS(YT) .GE. PFOVFL) THEN
            IST=-6
            RETURN
         END IF
C
         T=SGN*((XT-XB)/DV1)*UVM
         XB=CUFX(XB)
         XE=XB+T*SXDC
         T=SGN*((YT-YB)/DV1)*UVM
         YB=CUFY(YB)
         YE=YB+T*SYDC
C
C ---------------------------------------------------------------------
C
      ELSE
C
C Default mapping:
C
C WXMN, WXMX, WYMN, and WYMX contain the minimum and maximum values of
C the user coordinate space. Somewhat inaccurately, the mmenomic 'W'
C implies window coordinate space, which is usually (but not always)
C the same as user coordinate space. But note that even when 
C the coordinates are reversed, you are guaranteed that WXMN .LT. WXMX
C and WYMN .LT. WYMX. This eliminates the need to invoke MIN and MAX.
C
         IF (X.LT.WXMN .OR. X.GT.WXMX .OR. 
     +        Y.LT.WYMN .OR. Y.GT.WYMX) THEN
            IST = -1
            RETURN
         END IF
         XB=CUFX(X)
         YB=CUFY(Y)
         XE=XB+U*SXDC
         YE=YB+V*SYDC

      END IF
C
C Done.
C
      RETURN
C
      END


