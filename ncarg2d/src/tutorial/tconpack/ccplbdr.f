
      PROGRAM CCPLBDL
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

      PARAMETER (M=40,N=40,LRWK=3500,LIWK=4000)
      REAL Z(M,N), RWRK(LRWK), SIZE, Y
      INTEGER IWRK(LIWK)
      
      CALL GETDAT (Z, M, M, N)
C
C  Open GKS
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
C
C  Set up color table
C
      CALL COLOR(IWKID)
C
C  Set up plot annotation color and text options
C
      CALL CPSETI('HLC - HIGH/LOW LABEL COLOR INDEX',9)
      CALL CPSETC('HLT - HIGH/LOW LABEL TEXT','High''Low')
      CALL CPSETI('ILC - INFORMATION LABEL COLOR INDEX',2)
      CALL CPSETC('ILT - INFORMATION LABEL TEXT',
     +     'Informational Label')
C
C  Initialize Conpack
C
      CALL CPRECT(Z, M, M, N, RWRK, LRWK, IWRK, LIWK)
C
C  Turn on contour labeling for every line
C
      CALL CPPKCL(Z, RWRK, IWRK)
      CALL CPGETI('NCL - NUMBER OF CONTOUR LINES',NCONS)
      CALL CPSETI('LLP - LINE LABEL POSITIONING',3)
      CALL CPSETI('LLO - LINE LABEL ORIENTATION',1)
      DO 11, I=1,NCONS
         CALL CPSETI('PAI - PARAMETER ARRAY INDEX',I)
         CALL CPSETI('CLU - CONTOUR LEVEL USE FLAG',2)
         CALL CPSETI('LLC - LINE LABE COLOR INDEX',4)
 11   CONTINUE
C
C  Draw Perimeter
C
      CALL CPBACK(Z, RWRK, IWRK)
C
C  Draw Contours
C
      CALL CPLBDR(Z,RWRK,IWRK)
      CALL CPCLDR(Z,RWRK,IWRK)
C
C  Draw a Title
C
      CALL GSPLCI(6)
      CALL GETSET(VPL,VPR,VPB,VPT,WL,WR,WB,WT,LL)
      SIZE = .66 * (1.0 - VPT)
      Y = 1.0 - .5 * (1.0 - VPT)
      CALL SET (0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)
      CALL PLCHHQ (.5, Y, 'This is the Title',SIZE, 0., 0.)
C
C  Close frame and close GKS
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE GETDAT (Z, K, M, N)
      INTEGER I,J,K,M,N
      REAL Z(K,N)
      
      L=K
      DO 10, I=1,L
         READ (*,*) (Z(I,J),J=1,N)
 10   CONTINUE
      
      RETURN
      END
      SUBROUTINE COLOR(IWKID)
C  
C  Background color
C  Black
C
      CALL GSCR(IWKID,0,0.,0.,0.)
C  
C  Foreground colors
C  White
C
      CALL GSCR(IWKID,  1, 1.0, 1.0, 1.0)
C
C  Aqua
C
      CALL GSCR(IWKID,  2, 0.0, 0.9, 1.0)
C
C  Red
C
      CALL GSCR(IWKID,  3, 0.9, 0.25, 0.0)
C
C  OrangeRed
C
      CALL GSCR(IWKID,  4, 1.0, 0.0, 0.2)
C
C  Orange
C
      CALL GSCR(IWKID,  5, 1.0, 0.65, 0.0)
C
C  Yellow
C
      CALL GSCR(IWKID,  6, 1.0, 1.0, 0.0)
C
C  GreenYellow
C
      CALL GSCR(IWKID,  7, 0.7, 1.0, 0.2)
C
C  Chartreuse
C
      CALL GSCR(IWKID,  8, 0.5, 1.0, 0.0)
C
C  Celeste
C
      CALL GSCR(IWKID,  9, 0.2, 1.0, 0.5)
C
C  Green
C
      CALL GSCR(IWKID, 10, 0.2, 0.8, 0.2)
C
C  DeepSkyBlue
C
      CALL GSCR(IWKID, 11, 0.0, 0.75, 1.0)
C
C  RoyalBlue
C
      CALL GSCR(IWKID, 12, 0.25, 0.45, 0.95)
C
C  SlateBlue
C
      CALL GSCR(IWKID, 13, 0.4, 0.35, 0.8)
C
C  DarkViolet
C
      CALL GSCR(IWKID, 14, 0.6, 0.0, 0.8)
C
C  Magenta
C
      CALL GSCR(IWKID, 15, 1.0, 0.0, 1.0)
C
C  Lavender
C
      CALL GSCR(IWKID, 16, 0.8, 0.8, 1.0)
C
C  Gray
C
      CALL GSCR(IWKID, 17, 0.7, 0.7, 0.7)
C
C  Done.
C  
      RETURN
C  
      END
      
