      PROGRAM FCELL
C
C  Produce an NX x NY  CELL ARRAY based on the Mandelbrot set--color
C  the cells depending upon the speed of convergence or divergence.
C
      PARAMETER (IWTYPE=1)
      PARAMETER (NX=50, NY=50, NITER=201)
      INTEGER COLIA(NX,NY)
      COMPLEX Z
C
C  Region of interest.
C
C     DATA XL,XR,YB,YT/-0.1,.5,-0.,1.00/
      DATA XL,XR,YB,YT/.25,.375,-0.,0.50/
C
C  Open GKS, open and activate the metafile workstation.
C
      CALL GOPKS (6,IDUM)
      CALL GOPWK (1, 2, IWTYPE)
      CALL GACWK (1)
C
      CALL GSWN(1,XL,XR,YB,YT)
      CALL GSVP(1,0.,1.,0.,1.)
      CALL GSELNT(1)
C
C  Define color indices in a continuous spectrum.
C
      DO 11 K = 1,NITER
        H = REAL(K)/REAL(NITER)*360.
C       CALL HLSRGB(H,50.,100.,RV,GV,BV)
        CALL HLSRGB(H,50.,100.,RV,GV,BV)
        CALL GSCR(1,K,RV,GV,BV)
   11 CONTINUE
      CALL GSCR(1,NITER+1,1.,0.,0.)
C
C  Set up the cell array and call GFA.
C
      DX = (XR-XL)/NX
      DY = (YT-YB)/NY
      DO 10 J=1,NY
        Y = YB+DY*REAL(J-1)
        DO 20 I=1,NX
          X = XL+DX*REAL(I-1)
          Z = CMPLX(X,Y)
          CALL CONVG(Z,NITER,.001,10000.,ITER)
          COLIA(I,J) = ITER
   20   CONTINUE
   10 CONTINUE
      CALL GCA(XL,YB,XR,YT,NX,NY,1,1,NX,NY,COLIA)
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C	
      STOP
      END
      SUBROUTINE CONVG(Z,NUM,TOLSM,TOLLG,ITER)
C
C  Iterate Z(N+1) = Z(N)**2+Z0 for N=1,...,NUM .  If the complex absolute
C  values get smaller than TOLSM, then set ITER equal to the number of
C  iterations and return.  If the complex absolute value gets larger than
C  TOLLG, set ITER to NUM and return.
C
      COMPLEX Z,ZS,ZN,ZO
      ZS = Z
      ZO = Z
      DO 10 I=1,NUM
        ZN = ZO*ZO+ZS
        IF (CABS(ZN-ZO) .LT. TOLSM) THEN
          ITER = I
          RETURN
        ELSE IF (CABS(ZN-ZO) .GT. TOLLG) THEN
          ITER = I
          RETURN
        ENDIF
        ZO = ZN
   10 CONTINUE
      ITER=NUM
      RETURN
      END 
