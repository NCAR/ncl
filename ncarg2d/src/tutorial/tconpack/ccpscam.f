
      PROGRAM CCPSCAM
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

      PARAMETER (MREG=50,NREG=50)
      REAL XREG(MREG),YREG(NREG),ZREG(MREG,NREG)
C
C Get data array
C
      CALL GETDAT(XREG,YREG,ZREG,MREG,NREG)
C
C Open GKS and turn off clipping
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP(0)
C
C Call contour B&W fill routine
C
      CALL CCPSCM(ZREG,MREG,NREG)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

      STOP
      END

      SUBROUTINE CCPSCM(ZREG,MREG,NREG)

      PARAMETER (LRWK=1000,LIWK=1000,LMAP=60000,NWRK=5000,NOGRPS=5)
      REAL ZREG(MREG,NREG),RWRK(LRWK), XWRK(NWRK), YWRK(NWRK)
      INTEGER MREG,NREG,IWRK(LIWK)
      INTEGER MAP(LMAP),IAREA(NOGRPS),IGRP(NOGRPS)

      EXTERNAL SFILL
      EXTERNAL CPDRPL
C
C Use regular or penalty labeling scheme so that contour labels can be
C boxed, and draw boxes.
C
      CALL CPSETI('LLP - LINE LABEL POSITIONING FLAG',2)
      CALL CPSETI('LLB - LINE LABEL BOX FLAG',1)
      CALL CPSETI('HLB - HIGH/LOW LABEL BOX FLAG',1)
C
C Set number of contour levels and initialize Conpack
C
      CALL CPRECT (ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
      CALL CPPKCL (ZREG, RWRK, IWRK)
C
C Turn on line labeling and turn off area identifiers for all levels
C
      CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCL)
      DO 111 I=1,NCL
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
         CALL CPSETI('AIA - AREA IDENTIFIER ABOVE',0)
         CALL CPSETI('AIB - AREA IDENTIFIER BELOW',0)
 111  CONTINUE
C
C Add contour levels at 1.25 and 1.5, and set area ids so that 
C you can fill between them
C
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
C
C Draw Perimeter
C
      CALL CPBACK(ZREG, RWRK, IWRK)
C
C Initialize Areas
C
      CALL ARINAM(MAP, LMAP)
C
C Add contours to area map
C
      CALL CPCLAM(ZREG, RWRK, IWRK, MAP)
C
C Add label boxes to area map
C
      CALL CPLBAM(ZREG, RWRK, IWRK, MAP)
C
C Fill contours
C
      CALL ARSCAM(MAP, XWRK, YWRK, NWRK, IAREA, IGRP, NOGRPS, SFILL)
C
C Draw contours, masking label boxes
C
      CALL CPCLDM(ZREG, RWRK, IWRK, MAP, CPDRPL)
C
C Draw Labels
C
      CALL CPLBDR(ZREG, RWRK, IWRK)
C
C Write out the amount of space used in the area map
C
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
C
C If the area is defined by 3 or more points, fill it
C
         CALL SFSETR('SPACING',.006)
         CALL SFNORM(XWRK,YWRK,NWRK,RSCR,5000,ISCR,5000)
      ENDIF
C   
C Otherwise, do nothing
C
      RETURN
      END

      SUBROUTINE GETDAT(XREG,YREG,ZREG,MREG,NREG)

      PARAMETER (NRAN=30,LRWK=5000,LIWK=5000)
      INTEGER MREG, NREG
      REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
      REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
      REAL XDELTA(50)
      INTEGER IWRK(LIWK)

      DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     1         19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     2         18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
      DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     1          1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     2         29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
      DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     1         1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     2         1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/
      DATA XDELTA/.00,.02,.04,.06,.08,.10,.12,.14,.16,.18,.20,
     +            .22,.24,.26,.28,.30,.32,.34,.36,.38,.40,.42,
     +            .44,.46,.48,.50,.52,.54,.56,.58,.60,.62,.64,
     +            .66,.68,.70,.72,.74,.76,.78,.80,.82,.84,.86,
     +            .88,.90,.92,.94,.96,.98/
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
         XREG(I)=XMIN + (XMAX - XMIN)*XDELTA(I)
 101  CONTINUE
C
      DO 102 I=1,NREG
         YREG(I)=YMIN + (YMAX - YMIN)*XDELTA(I)
 102  CONTINUE
C      
C Interpolate data onto a regular grid
C
      CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +      MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)

      RETURN
      END
