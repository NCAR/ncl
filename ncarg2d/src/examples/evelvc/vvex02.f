C	$Id: vvex02.f,v 1.6 1993-02-19 22:25:25 dbrown Exp $
C
      PROGRAM VVEX02
C
C This program plots a randomly generated data set of velocity vector
C components on a number of EZMAP map projections. The vector 
C directions are transformed into the map coordinate space.
C
      PARAMETER ( M=70 , N=150 , NPR=155)
      PARAMETER (PI=3.14159 , TWOPI=2.*PI , EPS=PI/6.)
      DIMENSION A(M,NPR),B(M,N),ZDAT(M,N)
      EQUIVALENCE (A(1,5),B(1,1))
C
C     Open GKS, open workstation, activate workstation.
C
      CALL GOPKS (6,ISZ)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C     Generate the input arrays.
C
      CALL GENARA(B,A,M,N)
      CALL GENDAT (ZDAT,60,60,60,25,25,-1E+2, +1E+2)
C
      CALL DFCLRS
C
      CALL GSLWSC(1.0)
C
      DO 800 I=1,14,1
         CALL VVSETI('PAI -- Parameter Array Index', I)
         CALL VVSETI('CLR -- GKS Color Index', I+1)
 800  CONTINUE
C
      CALL VVSETR('LWD -- Vector Linewidth', 2.25)
      CALL VVSETI('VPO -- Vector Position Method', 0)
      CALL VVSETI('SET -- Set Call Flag', 0)
      CALL VVSETI('MAP -- Mapping Flag', 1)
      CALL VVSETI('NLV -- Number of Levels', 14)
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
C Treat the data as as grid covering the full globe
C
         CALL VVSETR('XC1 -- Lower X Bound', -180.0)
         CALL VVSETR('XCM -- Upper X Bound', 180.0)
         CALL VVSETR('YC1 -- Lower X Bound', -90.0)
         CALL VVSETR('YCN -- Upper Y Bound', 90.0)
         MA = 25
         NA = 25
         IDM = 0
C
         CALL VVINIT (A(1,10),M,A(1,25),M,ZDAT,60,MA,NA,IDM,IDM)
         CALL VVGETI('NLV -- Number Of Levels', NLV)
         CALL VVSETR('VFR -- Vector Fractional Minimum', 0.33)
         IF (I .GE. 5) THEN
            CALL VVGETR('VMN -- Minimum Vector', VMN)
            CALL  VVGETR('VMX -- Maximum Vector', VMX)
            CALL VVSETR('VLC -- Vector Low Cutoff',
     +           VMN+(VMX-VMN)/5.0)
            CALL VVGETR('VLC -- Vector Low Cutoff Magnitude',VLC)
            CALL VVGETR('VFR -- Vector Fractional Minimum',VFR)
            CALL VVGETR('DMX -- Distance of Max Vector',DMX)
            CALL GETSET(VL,VR,VB,VT,UL,UR,UB,UT,LL)
            VRL = 1.5 * DMX / (VR - VL)
            CALL VVSETR('VRL - Vector Realized Length', VRL)
         END IF
C
         CALL VVECTR (A(1,10),A(1,25),ZDAT,IDM,IDM,IDM)
         CALL FRAME
 1000 CONTINUE
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
