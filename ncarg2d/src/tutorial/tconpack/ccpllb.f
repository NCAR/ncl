        PARAMETER (MREG=50,NREG=50)
	REAL XREG(MREG),YREG(NREG),ZREG(MREG,NREG)

	EXTERNAL COLOR

C Get data array
	CALL GETDAT(XREG,YREG,ZREG,MREG,NREG)

C Open GKS
	CALL OPNGKS


C Call Conpack color fill routine

	CALL CCPLLB(ZREG,MREG,NREG,COLOR)


C Close frame and close GKS
	CALL FRAME
	CALL CLSGKS

	STOP
	END

	SUBROUTINE CCPLLB(ZREG,MREG,NREG,COLOR)

        PARAMETER (LRWK=5000,LIWK=5000,LMAP=50000,NWRK=5000,NOGRPS=5)
	REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
	INTEGER MREG,NREG,IWRK(LIWK)
	INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)

	EXTERNAL FILL
	EXTERNAL CPDRPL

C Set fill style to solid and turn off clipping
	CALL GSFAIS(1)
	CALL GSCLIP(0)

C Set up label box options
	CALL CPSETI('LLP',2)
	CALL CPSETI('LLB',2)
	CALL CPSETI('HLB',3)
	CALL CPSETI('ILB',1)
	CALL CPSETI('LBC',2)

C Initialize Conpack
	CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)

C Set up color table
	CALL CPPKCL (ZREG, RWRK, IWRK)
	CALL CPGETI('NCL',NCL)
	CALL COLOR(NCL+1)

C Draw Perimeter
	CALL CPBACK(ZREG, RWRK, IWRK)

C Initialize Areas
	CALL ARINAM(MAP, LMAP)

C Add label boxes to area map
	CALL CPLBAM(ZREG, RWRK, IWRK, MAP)

C Draw Labels
	CALL CPLBDR(ZREG, RWRK, IWRK)

C Add contours to area map
	CALL CPCLAM(ZREG, RWRK, IWRK, MAP)

C Fill contours
	CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, FILL)

	RETURN
	END

	SUBROUTINE FILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C
	DIMENSION XWRK(*),YWRK(*),IAREA(*),IGRP(*)

	DO 10, I=1,NGRPS
	  IF (IGRP(I).EQ.3) IAREA3=IAREA(I)
 10	CONTINUE

	IF (IAREA3 .GT. 0) THEN
C If the area is defined by 3 or more points, fill it
	   CALL GSFACI(IAREA3+2)
	   CALL GFA(NWRK,XWRK,YWRK)
	ENDIF
	
C Otherwise, do nothing

	RETURN
	END

	SUBROUTINE GETDAT(XREG,YREG,ZREG,MREG,NREG)

        PARAMETER (NRAN=30,LRWK=5000,LIWK=5000)
	REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
	REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
	INTEGER IWRK(LIWK)

	DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     1		   19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     2		   18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
	DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     1		    1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     2		   29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
	DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     1		   1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     2		   1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/

C
C  Set the min and max data values.
C
      XMIN = 0.0
      XMAX = 65.0
      YMIN =  0.0
      YMAX = 68.0
C
C Choose the X and Y coordinates for interpolation points on the 
C regular grid.
C
      DO 101 I=1,MREG
        XREG(I)=XMIN + (XMAX - XMIN)* REAL(I-1)/MREG
  101 CONTINUE
C
      DO 102 I=1,NREG
        YREG(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/NREG
  102 CONTINUE

C Interpolate data onto a regular grid
	CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +		MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)

	RETURN
	END
      SUBROUTINE COLOR(N)
C
C BACKGROUND COLOR
C BLACK
        CALL GSCR(1,0,0.,0.,0.)
C First foreground color is white
        CALL GSCR(1,1,1.,1.,1.)
C Second foreground color is gray
        CALL GSCR(1,2,.65,.65,.65)
C Choose other foreground colors spaced equally around the spectrum
        ICNT=0
        HUES=360./N
        DO 10, I=1,N
          XHUE=I*HUES
          CALL HLSRGB(XHUE,60.,75.,RED,GREEN,BLUE)
          IF (XHUE.LE.36.0) THEN
            CALL GSCR(1,N+3-I,RED,GREEN,BLUE)
            ICNT=ICNT+1
          ELSE
            CALL GSCR(1,I-ICNT+2,RED,GREEN,BLUE)
          ENDIF
 10     CONTINUE

        RETURN
        END

