
      PROGRAM VVEX02
C
C This program plots a randomly generated data set of velocity vector
C components on a 10 different EZMAP projections. The vector 
C directions are transformed into the map coordinate space.
C The last five frame make additional adjustments to the vector
C rendering parameters.
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
      PARAMETER ( M=70 , N=150 , NPR=155)
      DIMENSION A(M,NPR),B(M,N),ZDAT(M,N)
      EQUIVALENCE (A(1,5),B(1,1))
C
C     Open GKS, open workstation, activate workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C     Generate the input arrays and set up the color table
C
      CALL GENARA(B,A,M,N)
      CALL GENDAT (ZDAT,60,60,60,25,25,-1E+2, +1E+2)
      CALL DFCLRS(IWKID)
C
C Set up the Ezmap mapping, and the data coordinate space, 
C
      CALL VVSETI('SET -- Set Call Flag', 0)
      CALL VVSETI('MAP -- Mapping Flag', 1)
      CALL VVSETR('XC1 -- Lower X Bound', -180.0)
      CALL VVSETR('XCM -- Upper X Bound', 180.0)
      CALL VVSETR('YC1 -- Lower X Bound', -90.0)
      CALL VVSETR('YCN -- Upper Y Bound', 90.0)
C
C Set up Vectors' color index array
C
      CALL VVSETI('NLV -- Number of Levels', 14)
      DO 800 I=1,14,1
         CALL VVSETI('PAI -- Parameter Array Index', I)
         CALL VVSETI('CLR -- GKS Color Index', I+1)
 800  CONTINUE
C
C Miscellaneous rendering parameters
C 
      CALL VVSETR('LWD -- Vector Linewidth', 2.25)
      CALL VVSETI('VPO -- Vector Position Method', 0)
      CALL VVSETR('VFR -- Vector Fractional Minimum', 0.33)
C
      DO 1000 I=1,10,1 
C
C Alternately color vectors based on magnitude or scalar array value
C
         IF (MOD(I,2) .EQ. 0) THEN
            CALL VVSETI('CTV -- Color Thresholds Value', 1)
         ELSE
            CALL VVSETI('CTV -- Color Thresholds Value', 2)
         END IF
C
C Do 10 different easy map projections
C
         IF (I .EQ. 3) THEN
            CALL SUPMAP (3,0.,80.,70.,90.,80.,0.,0.,2,20,4,0,IERS)
         ELSE
            CALL SUPMAP (I,0.,0.,0.,0.,0.,0.,0.,1,20,2,0,IERS)
         END IF
C
         MA = 25
         NA = 25
         IDM = 0
C
         CALL VVINIT (A(1,10),M,A(1,25),M,ZDAT,60,MA,NA,IDM,IDM)
C
C For the last 5 frames adjust the Vector rendering parameters
C based on the projection and the dataset contents
C
         IF (I .GE. 5) THEN
            CALL VVGETR('VMN -- Minimum Vector', VMN)
            CALL  VVGETR('VMX -- Maximum Vector', VMX)
            CALL VVSETR('VLC -- Vector Low Cutoff',
     +           VMN+(VMX-VMN)/5.0)
            CALL VVGETR('VFR -- Vector Fractional Minimum',VFR)
            CALL VVGETR('DMX -- Distance of Max Vector',DMX)
            CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
            VRL = 1.5 * DMX / (VR - VL)
            CALL VVSETR('VRL - Vector Realized Length', VRL)
         END IF
C
         CALL VVECTR (A(1,10),A(1,25),ZDAT,IDM,IDM,IDM)
         CALL FRAME
C
 1000 CONTINUE
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
