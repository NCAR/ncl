C	$Id: vvex02.f,v 1.3 1993-01-22 19:28:58 dbrown Exp $
C
      PROGRAM VVEX02
C
C This program plots a randomly generated data set of velocity vector
C components on a number of EZMAP map projections. The vector 
C directions are transformed into the map coordinate space.
C 
      PARAMETER ( M=70 , N=70)
      DIMENSION A(M,N),B(M,N),ZDAT(M,N)
C
C     Open GKS, open workstation, activate workstation.
C
      CALL GOPKS (6,ISZ)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C     Generate the input array.
C
      CALL GENARA(A,B,M,N)
C
      CALL GENDAT (ZDAT,60,60,60,25,25,-1E+2, +1E+2)
C
      CALL DFCLRS
C
C     Frame 6 -- VELVCT overlaid on EZMAP.
C
      CALL GSLWSC(1.0)
c     
      DO 800 I=1,14,1
         CALL VVSETI('PAI -- Parameter Array Index', I)
         CALL VVSETI('CLR -- GKS Color Index', I+1)
 800  CONTINUE
C
      CALL VVSETR('LWD -- Vector Linewidth', 2.25)
      CALL VVSETI('VPO -- Vector Position Method', 0)
      CALL VVSETI('SET -- Set Call Flag', 0)
      CALL VVSETI('MAP -- Mapping Flag', 1)
C
      DO 1000 I=1,10,1 
C
C Alternately color vectors based on magnitude or scalar array value
C
         ISGN=1
         IF (MOD(I,2) .EQ. 0) THEN
            ISGN=-1
         END IF
         CALL VVSETI('CTV -- Color Thresholds Value', 14*ISGN)
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
         CALL VVINIT (A(3,20),60,B,60,ZDAT,60,MA,NA,IDM,IDM)
         CALL VVGETI('NLV -- Number Of Levels', NLV)
         CALL VVSETR('VFR -- Vector Fractional Minimum', 0.33)
         IF (I .GE. 5) THEN
            CALL VVGETR('VMN -- Minimum Vector', VMN)
            CALL  VVGETR('VMX -- Maximum Vector', VMX)
            CALL VVSETR('VLM -- Vector Low Magnitude',
     +           VMN+(VMX-VMN)/5.0)
            CALL VVGETR('VLM -- Vector Low Magnitude',VLM)
            CALL VVGETR('VFR -- Vector Fractioal Minimum',VFR)
            CALL VVGETR('DMX -- Distance of Max Vector',DMX)
            CALL VVSETR('VML -- Vector Max Length', DMX*2.0)
         END IF
C
         CALL VVECTR (A(3,20),B,ZDAT,IDM,IDM,IDM)
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




