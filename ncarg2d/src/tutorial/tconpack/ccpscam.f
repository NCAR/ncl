      PARAMETER (MREG=50,NREG=50)
      REAL XREG(MREG),YREG(NREG),ZREG(MREG,NREG)

C Get data array
      CALL GETDAT(XREG,YREG,ZREG,MREG,NREG)

C Open GKS and turn off clipping
      CALL OPNGKS
      CALL GSCLIP(0)

C Call contour B&W fill routine

      CALL CCPSCM(ZREG,MREG,NREG)


C Close frame and close GKS
      CALL FRAME
      CALL CLSGKS

      STOP
      END

      SUBROUTINE CCPSCM(ZREG,MREG,NREG)

      PARAMETER (LRWK=1000,LIWK=1000,LMAP=60000,NWRK=5000,NOGRPS=5)
      REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
      INTEGER MREG,NREG,IWRK(LIWK)
      INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)

      EXTERNAL SFILL
      EXTERNAL CPDRPL

C Use regular or penalty labeling scheme so that contour labels can be
c boxed, and draw boxes.
      CALL CPSETI('LLP - LINE LABEL POSITIONING FLAG',2)
      CALL CPSETI('LLB - LINE LABEL BOX FLAG',1)
      CALL CPSETI('HLB - HIGH/LOW LABEL BOX FLAG',1)

C Set number of contour levels and initialize Conpack
      CALL CPRECT (ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
      CALL CPPKCL (ZREG, RWRK, IWRK)

C Turn on line labeling and turn off area identifiers for all levels
      CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCL)
      DO 111 I=1,NCL
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
         CALL CPSETI('AIA - AREA IDENTIFIER ABOVE',0)
         CALL CPSETI('AIB - AREA IDENTIFIER BELOW',0)
 111  CONTINUE

C Add contour levels at 1.25 and 1.5, and set area ids so that 
C you can fill between them
      CALL CPSETI('NCL - NUMBER OF CONTOUR LEVELS',NCL+2)
      CALL CPSETI('PAI - PARAMETER ARRAY INDEX',NCL+1)
      CALL CPSETR('CLV - CONTOUR LEVEL VALUE',1.25)
      CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
      CALL CPSETI('AIA - AREA IDENTIFIER ABOVE',1)
      CALL CPSETI('AIB - AREA IDENTIFIER BELOW',2)
      CALL CPSETI('PAI - PARAMETER ARRAY INDEX',NCL+2)
      CALL CPSETR('CLV - CONTOUR LEVEL VALUE',1.5)
      CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
      CALL CPSETI('AIA - AREA IDENTIFIER ABOVE',3)
      CALL CPSETI('AIB - AREA IDENTIFIER BELOW',1)

C Draw Perimeter
      CALL CPBACK(ZREG, RWRK, IWRK)

C Initialize Areas
      CALL ARINAM(MAP, LMAP)

C Add contours to area map
      CALL CPCLAM(ZREG, RWRK, IWRK, MAP)

C Add label boxes to area map
      CALL CPLBAM(ZREG, RWRK, IWRK, MAP)

C Fill contours
      CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, SFILL)

C Draw contours, masking label boxes
      CALL CPCLDM(ZREG, RWRK, IWRK, MAP, CPDRPL)

C Draw Labels
      CALL CPLBDR(ZREG, RWRK, IWRK)

C Write out the amount of space used in the area map
      CALL CPGETI('RWU - REAL WORKSPACE USED',IRWU)
      CALL CPGETI('IWU - INTEGER WORKSPACE USED',IWU)
      WRITE (6,*) 'Area map used ',MAP(1)-MAP(6)+MAP(5),' words.'
      WRITE (6,*) 'Real workspace used ',IRWU,' words.'
      WRITE (6,*) 'Integer workspace used ',IWU,' words.'

      RETURN
      END

      SUBROUTINE SFILL (XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)
C
      REAL XWRK(*),YWRK(*),ISCR(5000)
      INTEGER IAREA(*),IGRP(*),RSCR(5000)

      DO 10, I=1,NGRPS
         IF (IGRP(I).EQ.3) IAREA3=IAREA(I)
 10   CONTINUE

      IF (IAREA3 .EQ. 1) THEN
C If the area is defined by 3 or more points, fill it
         CALL SFSETR('SPACING',.006)
         CALL SFNORM(XWRK,YWRK,NWRK,RSCR,5000,ISCR,5000)
      ENDIF
	
C Otherwise, do nothing

      RETURN
      END

      SUBROUTINE GETDAT(XREG,YREG,ZREG,MREG,NREG)

      PARAMETER (NRAN=30,LRWK=5000,LIWK=5000)
      INTEGER MREG, NREG
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
 101  CONTINUE
C
      DO 102 I=1,NREG
         YREG(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/NREG
 102  CONTINUE
      
C Interpolate data onto a regular grid
      CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +		MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)

      RETURN
      END
