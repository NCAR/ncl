C
C       $Id: vvex03.f,v 1.1 1994-01-15 00:45:53 dbrown Exp $
C
      PROGRAM VVEX03
C
C
C This example illustrates use of the user-modifiable routine
C VVUMXY to create user-defined mappings of vector data.
C The first frame shows how to plot data contained in an 
C irregularly spaced grid. 
C The second frame plots scattered (non-gridded) data.
C
C Open GKS, open workstation of type 1, activate workstation
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C Draw the irregularly gridded vectors
C
      CALL IRREX(IERR)
C
C Draw the scattered vectors
C
      CALL SCTREX(IERR)
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
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
C User-defined common block containing irregular grid coordinates
C
      PARAMETER (MAXDIM = 100)
      COMMON /VVUSER/ XCOORD(MAXDIM),YCOORD(MAXDIM)
      SAVE /VVUSER/
C
C Local arrays to hold the coordinate data
C
      DIMENSION XCLOC(M), YCLOC(N)
C
      DATA XCLOC / 0.0,7.5,20.0,35.0,45.0,65.0,75.0,80.0,90.0,95.0 /
      DATA YCLOC / 12.5,22.5,30.0,35.0,50.0,60.0,70.0,75.0,85.0,90.0 /
C
C
C Specify the NDC coordinates for a plot title.
C
        DATA FX / 0.15 /, FY / 0.975 /
C
C Set up a test uniform velocity field
C
      DO  20 I=1,M
         DO  10 J=1,N
            U(I,J) = 1.0
            V(I,J) = 1.0
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
         CALL VVSETR('XCM -- Upper X Bound', FLOAT(M))
         CALL VVSETR('YC1 -- Lower Y Bound', 1.0)
         CALL VVSETR('YCN -- Upper Y Bound', FLOAT(N))
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
C Save the current normalization transformation then set it to 0
C
         CALL GQCNTN(IERR,ICN)
         CALL GSELNT(0)
         X = CFUX(FX)
         Y = CFUY(FY)
C
C Call PLCHLQ to write the plot title.
C
         CALL PLCHLQ (X,Y,
     1           'Plot of irregularly gridded vector data',
     2           16.,0.,-1.)
C
C Restore the normalization transformation
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
C All data lies within a 100 x 100 square (origin 0.0 at lower left)
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
        DATA FX / 0.15 /, FY / 0.975 /
C
C Copy coordinate values into the VVUSER common block arrays
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
         CALL VVSETR('XCM -- Upper X Bound', FLOAT(M))
         CALL VVSETR('YC1 -- Lower Y Bound', 1.0)
         CALL VVSETR('YCN -- Upper Y Bound', FLOAT(N))
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
C Save the current normalization transformation then set it to 0
C
         CALL GQCNTN(IERR,ICN)
         CALL GSELNT(0)
         X = CFUX(FX)
         Y = CFUY(FY)
C
C Call PLCHLQ to write the plot title.
C
         CALL PLCHLQ (X,Y,
     1           'Plot of scattered vector data',
     2           16.,0.,-1.)
C
C Restore the normalization transformation
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
C Modified version of VVUMXY that implements irregular and scattered
C data mappings
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
C NOTE: all these variables should be considered read-only by VVUMXY
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
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C --------------------------------------------------------------------
C User-defined common block
C
      PARAMETER (MAXDIM = 100)
C
      COMMON /VVUSER/ XCOORD(MAXDIM), YCOORD(MAXDIM)
      SAVE /VVUSER/
C --------------------------------------------------------------------
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
         XT = XCOORD(I)
         YT = YCOORD(J)
C
C WXMN, WXMX, WYMN, and WYMX contain the minimum and maximum values of
C the user coordinate space. The following test ensures that the 
C coordinate values in the array are within the current boundaries
C of the user space.
C
         IF (XT.LT.WXMN .OR. XT.GT.WXMX .OR. 
     +        YT.LT.WYMN .OR. YT.GT.WYMX) THEN
            IST = -1
            RETURN
         END IF
         XB=CUFX(XT)
         YB=CUFY(YT)
         XE=XB+U*SXDC
         YE=YB+V*SYDC
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
         XT = XCOORD(NXCT*(J-1)+I)
         YT = YCOORD(NXCT*(J-1)+I)
C
         IF (XT.LT.WXMN .OR. XT.GT.WXMX .OR. 
     +        YT.LT.WYMN .OR. YT.GT.WYMX) THEN
            IST = -1
            RETURN
         END IF
         XB=CUFX(XT)
         YB=CUFY(YT)
         XE=XB+U*SXDC
         YE=YB+V*SYDC
C

      ELSE
C
C Default mapping:
C
C Note: this code corrects the default version of VVUMXY included with
C NCAR Graphics 3.2 incorrectly fails to convert the input X and Y
C location into fractional coordinates (NDC space).
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


