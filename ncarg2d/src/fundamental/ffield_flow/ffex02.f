
      PROGRAM FFEX02
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
C This program requires the input data file 'ffex02.dat'
C It reads the data from standard input, e.g.: ffex02 < ffex02.dat
C
      PARAMETER (MSIZE=36, NSIZE=33, NCLRS=14)
C
      DIMENSION U(MSIZE,NSIZE), V(MSIZE,NSIZE), P(MSIZE,NSIZE)
C
C Define a set of RGB color triples
C
      DIMENSION RGBV(3,NCLRS)
C
      DATA RGBV / 
     +     0.0,1.0,1.0,
     +     0.0745098,0.92549,0.92549,
     +     0.152941,0.847059,0.847059,
     +     0.231373,0.768627,0.768627,
     +     0.305882,0.694118,0.694118,
     +     0.384314,0.615686,0.615686,
     +     0.462745,0.537255,0.537255,
     +     0.537255,0.462745,0.462745,
     +     0.615686,0.384314,0.384314,
     +     0.694118,0.305882,0.305882,
     +     0.768627,0.231373,0.23137,
     +     0.847059,0.152941,0.152941,
     +     0.92549,0.0745098,0.074509,
     +     1.0,0.0,0.0 /
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Read the input array data
C
      CALL RDDATA(U,V,P,MSIZE,NSIZE)
C
C Set up the EZMAP projection
C
      CALL MAPSET('CO',60.0,-120.0,23.0,-60.0)
      CALL MAPROJ('LC',0.0,-75.0,45.0)
      CALL MAPINT
C
C Tell Vectors to use the mapping established by EZMAP
C
      CALL VVSETI('MAP -- Mapping Flag', 1)
      CALL VVSETI('SET -- Set Call Flag', 0)
C
C Set up data coordinate boundaries and special value processing 
C appropriately for the dataset 
C
      CALL VVSETR('XC1 -- Lower X Bound', -140.0)
      CALL VVSETR('XCM -- Upper X Bound', -52.5)
      CALL VVSETR('YC1 -- Lower X Bound', 20.0)
      CALL VVSETR('YCN -- Upper Y Bound', 60.0)
C
      CALL VVSETI('SVF -- Special Values Flag', 3)
      CALL VVSETR('USV -- U Special Value', -9999.0)
      CALL VVSETR('VSV -- V Special Value', -9999.0)
      CALL VVSETR('PSV - P Special Value', -9999.0)
      CALL VVSETI('SPC - P Special Color', 1)
C
C Turn on statistics reporting
C
      CALL VVSETI('VST -- Vector Statistics', 1)
C
      DO 1000 IFRMNO = 1,4
C
C Draw the map with a grid
C
         CALL MAPLOT
         CALL MAPGRD
C
C Set up color processing
C
         IF (IFRMNO .EQ. 4) THEN
            CALL VVSETI('CTV -- Color Thresholds Value', 2)
            CALL VVSETI('NLV -- Number Of Levels', NCLRS)
            DO 100 I=1,NCLRS,1
               ICLRIX=2+(I-1)*200/NCLRS
               CALL GSCR(IWKID,ICLRIX,RGBV(1,I),RGBV(2,I),RGBV(3,I))
               CALL VVSETI('PAI -- Parameter Array Index', I)
               CALL VVSETI('CLR -- GKS Color Index', ICLRIX)
 100        CONTINUE
         END IF
C
C Initialize Vectors
C
         IF (IFRMNO .GT. 1) THEN
            CALL VVINIT (U,MSIZE,V,MSIZE,P,MSIZE,MSIZE,NSIZE,IDM,IDM)
         END IF
C
C Adjust vector rendering options
C
         IF (IFRMNO .EQ. 3) THEN
            CALL VVSETR('AMN -- Arrow Minimum Size',0.007)
            CALL VVSETR('LWD -- Vector Line Width',1.75)
            CALL VVGETR('VMN -- Minimum Vector',VMN)
            CALL VVGETR('VMX -- Maximum Vector',VMX)
            CALL VVSETR('VLC -- Vector Low Cutoff',VMN+0.125*(VMX-VMN))
            CALL VVGETR('DMX -- Device Maximum Vector Length',DMX)
            CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
            CALL VVSETR('VRL - Vector Realized Length',1.8*DMX/(VR-VL))
            CALL VVSETR('VFR -- Vector Fractional Minimum',0.33)
         END IF
C
C Draw the vector field plot
C
         IF (IFRMNO .GT. 1) THEN
            CALL VVECTR (U,V,P,IDM,IDM,IDM)
         END IF
C
C Draw a perimeter boundary and eject the frame
C
         CALL PERIM(1,0,1,0)
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
C
      SUBROUTINE RDDATA(U,V,P,M,N)
C
C Read the data arrays from the standard input 
C
      DIMENSION U(M,N), V(M,N), P(M,N)
C     
      READ (*,*) U
      READ (*,*) V
      READ (*,*) P
C
      RETURN
      END
