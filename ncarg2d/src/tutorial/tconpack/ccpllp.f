        PROGRAM CCPLLP
C
C Define error file, Fortran unit number, and workstation type,
C and workstation ID.
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1, IWKID=1)
        
        PARAMETER (NRAN=30,LRWK=3500,LIWK=4000,LMAP=50000)
        PARAMETER (MREG=50,NREG=50)
        REAL XRAN(NRAN), YRAN(NRAN), ZRAN(NRAN)
        REAL XREG(MREG), YREG(NREG), ZREG(MREG,NREG), RWRK(LRWK)
        INTEGER IWRK(LIWK), MAP(LMAP)
        
        EXTERNAL CPDRPL
        
        DATA XRAN /12., 60., 14., 33.,  8., 12., 43., 57., 22., 15.,
     +      19., 12., 64., 19., 15., 55., 31., 32., 33., 29.,
     +      18.,  1., 18., 42., 56.,  9.,  6., 12., 44., 19./
        DATA YRAN / 1.,  2.,  3., 53.,  7., 11., 13., 17., 19., 49.,
     +      1., 31., 37.,  5.,  7., 47., 61., 17.,  5., 23.,
     +      29.,  3.,  5., 41., 43.,  9., 13., 59.,  1., 67./
        DATA ZRAN /1.0, 1.5, 1.7, 1.4, 1.9, 1.0, 1.5, 1.2, 1.8, 1.4,
     +      1.8, 1.7, 1.9, 1.5, 1.2, 1.1, 1.3, 1.7, 1.2, 1.6,
     +      1.9, 1.0, 1.6, 1.3, 1.4, 1.8, 1.7, 1.5, 1.1, 1.0/
C
C Open GKS
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
        CALL GSCLIP(0)
C 
C Find the min and max data values.
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
 101    CONTINUE
C 
        DO 102 I=1,NREG
           YREG(I)=YMIN + (YMAX - YMIN)* REAL(I-1)/NREG
 102    CONTINUE
C
C Interpolate data onto a regular grid
C
        CALL IDSFFT (1,NRAN,XRAN,YRAN,ZRAN,
     +       MREG,NREG,MREG,XREG,YREG,ZREG,IWRK,RWRK)
C
C ------------Default Labels-----------------
C Set up viewport
C
        CALL CPSETR('VPT - VIEWPORT TOP',0.95)
        CALL CPSETR('VPB - VIEWPORT BOTTOM',.50)
        CALL CPSETR('VPL - VIEWPORT LEFT',0.0)
        CALL CPSETR('VPR - VIEWPORT RIGHT',.48)
        CALL CPSETC('ILT - INFORMATION LABEL TEXT',' ')
C
C Initialize Conpack
C
        CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Force Conpack to chose contour levels
C
        CALL CPPKCL(ZREG, RWRK, IWRK)
C
C Modify Conpack chosen parameters
C
        CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCONS)
C
C choose which labelling scheme will be used.
C
        CALL CPSETI('LLP - LINE LABEL POSTIONING FLAG',1)
C
C Turn off high and low labels
C
        CALL CPSETC('HLT - HIGH/LOW LABEL TEXT',' '' ')
        DO 11, I=1,NCONS
           CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
C
C Force every line to be labeled.
C
           CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
 11     CONTINUE
C
C Draw Perimeter
C
        CALL CPBACK(ZREG, RWRK, IWRK)
C
C Draw Contours
C
        CALL CPCLDR(ZREG,RWRK,IWRK)
        CALL CPLBDR(ZREG,RWRK,IWRK)
C
C Title plot
C
        CALL GSELNT(0)
        CALL PLCHHQ(.25, .975, 'Default Labels', .017, 0., 0.)
C
C ------------Medium Labels-----------------
C Set up Areas for drawing boxes
C
        CALL ARINAM(MAP, LMAP)
C
C Set up viewport
C
        CALL CPSETR('VPT - VIEWPORT TOP',0.95)
        CALL CPSETR('VPB - VIEWPORT BOTTOM',.50)
        CALL CPSETR('VPR - VIEWPORT RIGHT',1.0)
        CALL CPSETR('VPL - VIEWPORT LEFT',.52)
        CALL CPSETC('ILT - INFORMATION LABEL TEXT',' ')
C
C Initialize Conpack
C
        CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Force Conpack to chose contour levels
C
        CALL CPPKCL(ZREG, RWRK, IWRK)
C
C Modify Conpack chosen parameters
C
        CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCONS)
C
C choose which labelling scheme will be used.
C
        CALL CPSETI('LLP - LINE LABEL POSTIONING FLAG',2)
C
C Turn off high and low labels
C
        CALL CPSETC('HLT - HIGH/LOW LABEL TEXT',' '' ')
        DO 12, I=1,NCONS
           CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
C
C Force every line to be labeled.
C
           CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
 12     CONTINUE
C
C Draw Perimeter
C
        CALL CPBACK(ZREG, RWRK, IWRK)
C
C Add contours to area map
C
        CALL CPCLAM(ZREG, RWRK, IWRK, MAP)
C
C Add label boxes to area map
C
        CALL CPLBAM(ZREG, RWRK, IWRK, MAP)
C
C Draw contours
C
        CALL CPCLDM(ZREG, RWRK, IWRK, MAP, CPDRPL)
C
C Draw labels
C
        CALL CPLBDR(ZREG, RWRK, IWRK)
C
C Title plot
C
        CALL GSELNT(0)
        CALL PLCHHQ(.75, .975, 'Regular Scheme', .017, 0., 0.)
C
C ------------High Quality Labels-----------------
C Set up area map
C
        CALL ARINAM(MAP, LMAP)
C
C Set up viewport
C
        CALL CPSETR('VPT - VIEWPORT TOP',.45)
        CALL CPSETR('VPB - VIEWPORT BOTTOM',0.0)
        CALL CPSETR('VPL - VIEWPORT LEFT',0.0)
        CALL CPSETR('VPR - VIEWPORT RIGHT',1.0)
        CALL CPSETC('ILT - INFORMATION LABEL TEXT',' ')
C
C Initialize Conpack
C
        CALL CPRECT(ZREG, MREG, MREG, NREG, RWRK, LRWK, IWRK, LIWK)
C
C Force Conpack to chose contour levels
C
        CALL CPPKCL(ZREG, RWRK, IWRK)
C
C Modify Conpack chosen parameters
C
        CALL CPGETI('NCL - NUMBER OF CONTOUR LEVELS',NCONS)
C
C choose which labelling scheme will be used.
C
        CALL CPSETI('LLP - LINE LABEL POSTIONING FLAG',3)
C
C Turn off high and low labels
C
        CALL CPSETC('HLT - HIGH/LOW LABEL TEXT',' '' ')
        DO 13, I=1,NCONS
           CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
C
C Force every line to be labeled.
C
           CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',3)
 13     CONTINUE
C
C Draw Perimeter
C
        CALL CPBACK(ZREG, RWRK, IWRK)
C
C Add contours to area map
C
        CALL CPCLAM(ZREG, RWRK, IWRK, MAP)
C
C Add label boxes to area map
C
        CALL CPLBAM(ZREG, RWRK, IWRK, MAP)
C
C Draw contours
C
        CALL CPCLDM(ZREG, RWRK, IWRK, MAP, CPDRPL)
C
C Draw labels
C
        CALL CPLBDR(ZREG, RWRK, IWRK)
C
C Title plot
C
        CALL GSELNT(0)
        CALL PLCHHQ(.5, .475, 'Penalty Scheme', .017, 0., 0.)
C
C Close frame and close GKS
C
        CALL FRAME
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
        
        WRITE (6,*) 'AREA MAP SIZE =',MAP(1) - MAP(6) + MAP(5)
        STOP
        END
      
      
