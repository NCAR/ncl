      PROGRAM CCPLLT
C 
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C 
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
      
      PARAMETER (LRWK=3500,LIWK=4000,LMAP=75000)
      PARAMETER (MREG=50,NREG=50)
      REAL X(MREG),Y(NREG),ZREG(MREG,NREG), RWRK(LRWK)
      INTEGER IWRK(LIWK), MAP(LMAP)
      CHARACTER*21 STRING
      
      EXTERNAL CPDRPL
      
      CALL GETDAT (X, Y, ZREG, MREG, NREG, RWRK, IWRK, LRWK, LIWK)
C
C Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Initialize Areas
C
      CALL ARINAM(MAP,LMAP)
C
C Initialize Conpack
C
      CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Pick contour levels
C
      CALL CPPKCL(ZREG, RWRK, IWRK)
C
C Get the number of contour levels chosen
C
      CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCONS)
C
C Choose the 'regular scheme' for labeling
C
      CALL CPSETI('LLP - LINE LABEL POSITIONING FLAG',2)
C
C Set the label size to be large
C
      CALL CPSETR('LLS - LINE LABEL SIZE',.01)
C
C Turn off high and low labels
C
      CALL CPSETC('HLT - HIGH/LOW LABEL TEXT',' '' ')
C
C Draw labels in the local direction of their contour line
C
      CALL CPSETI('LLO - LINE LABEL ORIENTATION',1)
C
C Modify each contour line label
C
      DO 11, I=1,NCONS
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETI('CLU - CONTOUR LINE USE FLAG',3)
         CALL CPGETR('CLV - CONTOUR LEVEL VALUES',VALUE)
         WRITE (STRING,5) VALUE
         CALL CPSETC('LLT - LINE LABEL TEXT',STRING)
 11   CONTINUE
C
C Draw Perimeter
C
      CALL CPBACK(ZREG, RWRK, IWRK)
C
C Add contours to area map
C
      CALL CPCLAM(ZREG,RWRK,IWRK,MAP)
C
C Add labels to area map
C
      CALL CPLBAM(ZREG,RWRK,IWRK,MAP)
C
C Draw contours and labels
C
      CALL CPCLDM(ZREG,RWRK,IWRK,MAP,CPDRPL)
      CALL CPLBDR(ZREG,RWRK,IWRK)
C
C Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
 5    FORMAT (F6.2,':SRIL:O:N:C',F6.2)
      STOP
      END
      
      SUBROUTINE GETDAT (X,Y,Z,M,N,RWRK,IWRK,LRWK,LIWK)
      
      PARAMETER (NRAN=30)
      
      REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
      REAL X(M), Y(N), Z(M,N), RWRK(LRWK)
      INTEGER IWRK(LIWK)
      
      DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     +      19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     +      18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
      DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     +      1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     +      29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
      DATA ZRAN /1.0, 15., 30., 14., 50., 10., 5.0, 12., 30., 24.,
     +      18., 23., 45., 15., 12., 11., 33., 7.0, 19., 16.,
     +      19., 10., 1.0, 13., 26., 18., 21., 22., 11., 1.0/
C 
C Set the min and max data values.
C 
      XMIN = 0.0
      XMAX = 65.0
      YMIN =  0.0
      YMAX = 68.0
C 
C Choose the X and Y coordinates for interpolation points on the 
C regular grid.
C 
      DO 101 I=1,M
         X(I)=XMIN + (XMAX - XMIN)* REAL(I-1)/M
 101  CONTINUE
C 
      DO 102 I=1,N
         Y(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/N
 102  CONTINUE
C
C Interpolate data onto a regular grid
C
      CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,M,N,M,X,Y,Z,IWRK,RWRK)
      
      RETURN
      END
