
        PROGRAM SFEX02
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C Declare required dimensioned arrays.
C
        DIMENSION XCS(101),YCS(101),XRA(100),YRA(100),DST(102),IND(104)
C
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IAS(13)
C
C Define a character variable for use in labelling the plot.
C
        CHARACTER*2 LBL
C
C Initialize the values in the aspect-source-flag array.
C
        DATA IAS / 13*1 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn off the clipping indicator.
C
        CALL GSCLIP (0)
C
C Set all the GKS aspect source flags to "individual".
C
        CALL GSASF (IAS)
C
C Force solid fill.
C
        CALL GSFAIS (1)
C
C Define color indices.
C
        CALL DFCLRS(IWKID)
C
C Define the window and viewport to make it easy to do seven rows
C containing sixteen ovals apiece.
C
        CALL SET (0.,1.,0.,1.,-1.,16.5,-.5,7.5,1)
C
C For each of the possible values of the internal parameter 'TY', fill
C a set of circles, one for each of sixteen values of ICI (0-15).
C
        DO 103 ITY=-4,2
C
          YCN=REAL(3-ITY)
C
          WRITE (LBL,'(I2)') ITY
          CALL PLCHHQ (CFUX(CUFX(.5)-.006),YCN,LBL,.012,0.,1.)
C
          CALL SFSETI ('TYPE OF FILL',ITY)
C
          DO 102 ICI=0,15
C
            XCN=REAL(ICI+1)
C
            DO 101 L=1,101
              XCS(L)=XCN+.48*SIN(.062831853071796*REAL(L))
              YCS(L)=YCN+.48*COS(.062831853071796*REAL(L))
              IF (L.LT.101) THEN
                XRA(L)=XCS(L)
                YRA(L)=YCS(L)
              END IF
  101       CONTINUE
C
            CALL SFSGFA (XRA,YRA,100,DST,102,IND,104,ICI)
C
            CALL CURVE (XCS,YCS,101)
C
  102     CONTINUE
C
  103   CONTINUE
C
C Finish the labelling.
C
        CALL PLCHHQ (CFUX(CUFX(.5)-.060),4.,'"TYPE OF FILL"',
     +                                                      .012,90.,0.)
C
        DO 104 ICI=0,15
          XCN=REAL(ICI+1)
          IF (ICI.LT.10) THEN
            WRITE (LBL,'(I1)') ICI
            CALL PLCHHQ (XCN,CFUY(CUFY(.5)-.024),LBL(1:1),.012,0.,0.)
          ELSE
            WRITE (LBL,'(I2)') ICI
            CALL PLCHHQ (XCN,CFUY(CUFY(.5)-.024),LBL(1:2),.012,0.,0.)
          END IF
  104   CONTINUE
C
        CALL PLCHHQ (8.5,CFUY(CUFY(.5)-.060),'"COLOR INDEX"',.012,0.,0.)
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END
      SUBROUTINE DFCLRS(IWKID)
C
C Define a set of RGB color triples for colors 1 through 15.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 1.00 , 1.00 , 1.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (IWKID,0,0.,0.,0.)
C
        DO 101 I=1,15
          CALL GSCR (IWKID,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
