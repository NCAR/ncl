	PROGRAM CCPLBL

        PARAMETER (M=40,N=40,LRWK=3500,LIWK=4000)
	REAL Z(M,N), RWRK(LRWK), SIZE, Y
	INTEGER IWRK(LIWK)

	CALL GETDAT (Z, M, M, N)
C Open GKS
	CALL OPNGKS
	CALL GSCLIP (0)
C Set up color table
	CALL COLOR
C Set up plot annotation color and text options
	CALL CPSETI('HLC - HIGH/LOW LABEL COLOR INDEX',9)
	CALL CPSETC('HLT - HIGH/LOW LABEL TEXT','High''Low')
	CALL CPSETI('ILC - INFORMATION LABEL COLOR INDEX',2)
	CALL CPSETC('ILT - INFORMATION LABEL TEXT',
     +		'Informational Label')
C Initialize Conpack
	CALL CPRECT(Z, M, M, N, RWRK, LRWK, IWRK, LIWK)
C Turn on contour labeling for every line
	CALL CPPKCL(Z, RWRK, IWRK)
	CALL CPGETI('NCL - NUMBER OF CONTOUR LINES',NCONS)
	CALL CPSETI('LLP - LINE LABEL POSITIONING',3)
	CALL CPSETI('LLO - LINE LABEL ORIENTATION',1)
	DO 11, I=1,NCONS
	   CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
	   CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',2)
	   CALL CPSETI('LLC - LINE LABE COLOR INDEX',4)
 11	CONTINUE
C Draw Perimeter
	CALL CPBACK(Z, RWRK, IWRK)
C Draw Contours
	CALL CPLBDR(Z,RWRK,IWRK)
	CALL CPCLDR(Z,RWRK,IWRK)

C Draw a Title
	CALL GSPLCI(6)
	CALL GETSET(VPL,VPR,VPB,VPT,WL,WR,WB,WT,LL)
	SIZE = .66 * (1.0 - VPT)
	Y = 1.0 - .5 * (1.0 - VPT)
	CALL SET (0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)
	CALL PLCHHQ (.5, Y, 'This is the Title',SIZE, 0., 0.)

C Close frame and close GKS
	CALL FRAME
	CALL CLSGKS

	STOP
	END

	SUBROUTINE GETDAT (Z, K, M, N)

	REAL Z(K,N)
	INTEGER I,J,K,M,N

	OPEN (10,FILE='ccpex.dat',STATUS='OLD')
	M=K
	DO 10, I=1,M
	  READ (10,*) (Z(I,J),J=1,N)
  10	CONTINUE

	RETURN
	END
      SUBROUTINE COLOR
C
C     BACKGROUND COLOR
C     BLACK
      CALL GSCR(1,0,0.,0.,0.)
C
C     FORGROUND COLORS
C White
      CALL GSCR(1,  1, 1.0, 1.0, 1.0)
C Aqua
      CALL GSCR(1,  2, 0.0, 0.9, 1.0)
C Red
      CALL GSCR(1,  3, 0.9, 0.25, 0.0)
C OrangeRed
      CALL GSCR(1,  4, 1.0, 0.0, 0.2)
C Orange
      CALL GSCR(1,  5, 1.0, 0.65, 0.0)
C Yellow
      CALL GSCR(1,  6, 1.0, 1.0, 0.0)
C GreenYellow
      CALL GSCR(1,  7, 0.7, 1.0, 0.2)
C Chartreuse
      CALL GSCR(1,  8, 0.5, 1.0, 0.0)
C Celeste
      CALL GSCR(1,  9, 0.2, 1.0, 0.5)
C Green
      CALL GSCR(1, 10, 0.2, 0.8, 0.2)
C DeepSkyBlue
      CALL GSCR(1, 11, 0.0, 0.75, 1.0)
C RoyalBlue
      CALL GSCR(1, 12, 0.25, 0.45, 0.95)
C SlateBlue
      CALL GSCR(1, 13, 0.4, 0.35, 0.8)
C DarkViolet
      CALL GSCR(1, 14, 0.6, 0.0, 0.8)
C Magenta
      CALL GSCR(1, 15, 1.0, 0.0, 1.0)
C Lavender
      CALL GSCR(1, 16, 0.8, 0.8, 1.0)
C Gray
      CALL GSCR(1, 17, 0.7, 0.7, 0.7)
C Done.
C
        RETURN
C
      END
