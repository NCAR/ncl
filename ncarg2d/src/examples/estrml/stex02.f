C
C	$Id: stex02.f,v 1.3 1993-02-20 00:46:25 dbrown Exp $
C
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL STEX02(ierr)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
      SUBROUTINE STEX02 (IERROR)
C
C
      PARAMETER (M=25,N=25)
      DIMENSION       U(M,N)   ,V(M,N)
      DIMENSION WRK(M*N*2)
      COMMON /TRANS/ MA,NA,ITRANS,A1,A2,D1,D2,ALNMN,ALNMX,ALTMN,ALTMX
C
C Specify coordinates for a plot title.
C
        DATA IX/94/,IY/1000/
C
C Specify VELVCT arguments.
C
      DATA FLO/0./,HI/0./,NSET/0/,LENGTH/0/,ISPV/0/,SPV/0./
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Specify velocity field functions U and V.
C
      T = 1.0E-12
      DO  20 I=1,M
         X = FLOAT(I-11)
         DO  10 J=1,N
            Y = FLOAT(J-13)
            U(I,J)= 1.0
            V(I,J)= 1.0
   10    CONTINUE
   20 CONTINUE
C
C
        CALL GQCNTN(IERR,ICN)
        CALL GSELNT(ICN)
        CALL STSETR('VNL - Normalized Vector Magnitude', 0.33)
        MA=M
        NA=N
        ITRANS=0
        ALNMN = 1.0
        ALNMX = 100.0
        ALTMN = 1.0
        ALTMX = 100.0
C        
        CALL VVSETR('VFR -- Vector Frac Minimum', 0.25)
        CALL VVSETI('TRT - Transformation Type', 1)
        DO 100 I = 1,5
           
           ICPM=-2
C           IF (I .EQ. 1) ICPM = 2
           IF (I .EQ. 5) THEN
              XMX=50.0
              LL=1
           ELSE
              XMX=100.0
              LL=I
           ENDIF
C
           CALL SET(0.05,0.95,0.05,0.95,1.0,XMX,1.0,100.0,LL)
	   CALL STSETI('CPM -- Compatibility Mode', ICPM) 
	   CALL VVSETI('CPM -- Compatibility Mode', -2) 
           CALL VVSETR('XC1 -- Lower X Bound', 1.0)
           CALL VVSETR('XCM -- Upper X Bound', XMX)
           CALL VVSETR('YC1 -- Lower X Bound', 1.0)
           CALL VVSETR('YCN -- Upper Y Bound', 100.0)
           CALL STSETR('XC1 -- Lower X Bound', 1.0)
           CALL STSETR('XCM -- Upper X Bound', XMX)
           CALL STSETR('YC1 -- Lower X Bound', 1.0)
           CALL STSETR('YCN -- Upper Y Bound', 100.0)
           CALL STSETR('SSP -- Stream Spacing', 0.015)
C           
           NSET=1
C           
           CALL GSPLCI(3)
           CALL VELVCT (U,M,V,M,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
           CALL GSPLCI(7)
           CALL STRMLN (U,V,WRK,M,M,N,1,IER)
           CALL GQCNTN(IERR,ICN)
C
C Select normalization transformation 0.
C
           CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
           X = CPUX(IX)
           Y = CPUY(IY)
           CALL PLCHLQ (X,Y,
     1           'Streamlines Plotted Over a Uniform Vector Field',
     2           16.,0.,-1.)
           CALL GSELNT(ICN)
           CALL FRAME
 100    CONTINUE
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' STREAMLINE TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END



