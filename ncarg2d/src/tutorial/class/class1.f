
      PROGRAM CLASS1
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

      INTEGER IND(100)
      REAL XBALLN(15),YBALLN(15),XSTR1(2),YSTR1(2)
      REAL XSTR2(8),YSTR2(8), XMOUTH(5),YMOUTH(5),EYE(2)
      REAL XHEAD(28),YHEAD(28),XNECK(2),YNECK(2)
      REAL XBLNKT(11),YBLNKT(11), XTAIL(16),YTAIL(16)
      REAL XRHLEG(8),YRHLEG(8), XBELLY(3),YBELLY(3)
      REAL XFFLEG(14),YFFLEG(14),DST(100)
      
C
C Read in the data
C
      READ (*,*) (XBALLN(I),I=1,15)
      READ (*,*) (YBALLN(I),I=1,15)
      READ (*,*) (XSTR1(I),I=1,2)
      READ (*,*) (YSTR1(I),I=1,2)
      READ (*,*) (XSTR2(I),I=1,8)
      READ (*,*) (YSTR2(I),I=1,8)
      READ (*,*) (XMOUTH(I),I=1,5)
      READ (*,*) (YMOUTH(I),I=1,5)
      READ (*,*) (EYE(I),I=1,2)
      READ (*,*) (XHEAD(I),I=1,28)
      READ (*,*) (YHEAD(I),I=1,28)
      READ (*,*) (XNECK(I),I=1,2)
      READ (*,*) (YNECK(I),I=1,2)
      READ (*,*) (XBLNKT(I),I=1,11)
      READ (*,*) (YBLNKT(I),I=1,11)
      READ (*,*) (XTAIL(I),I=1,16)
      READ (*,*) (YTAIL(I),I=1,16)
      READ (*,*) (XRHLEG(I),I=1,8)
      READ (*,*) (YRHLEG(I),I=1,8)
      READ (*,*) (XBELLY(I),I=1,3)
      READ (*,*) (YBELLY(I),I=1,3)
      READ (*,*) (XFFLEG(I),I=1,14)
      READ (*,*) (YFFLEG(I),I=1,14)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set up color table
C
      CALL GSCR(IWKID,0,0.0,0.0,0.0)
      CALL GSCR(IWKID,1,1.0,1.0,1.0)
      CALL GSCR(IWKID,2,1.0,0.7,0.7)
      CALL GSCR(IWKID,3,0.0,1.0,1.0)
      CALL GSCR(IWKID,4,1.0,1.0,0.3)
C
C Set title color and draw title before changing 
C from default normalization transformation
C
      CALL GSPLCI(4)
      CALL PLCHHQ(0.5,.92,'Every dog has his day',.017,0.0,0.0)
C
C Set normalization transformation and scaling
C
      CALL SET(0.1,0.9,0.1,0.9,-4.,14.,0.0,18.0,1)
C
C Set eye color and draw eye
C
      CALL GSPMCI(2)
      CALL POINTS(EYE(1),EYE(2),1,-4,0)
C
C Fill balloon using GKS
C
      CALL GSFAIS(1)
      CALL GSFACI(3)
      CALL GFA(15,XBALLN,YBALLN)
C
C Stipple in patch on dogs back
C
      CALL GSPMCI(1)
      CALL SFSETR('SP - Spacing',.005)
      CALL SFSETI('DO - Dot fill',1)
      CALL SFWRLD(XBLNKT,YBLNKT,11,DST,100,IND,100)
C
C Set dash pattern, and draw outlines
C
      CALL DASHDC('$$$$$$$$$$$$$$$$',1,1)
      CALL CURVED(XBALLN,YBALLN,15)
      CALL CURVED(XSTR1,YSTR1,2)
      CALL CURVED(XSTR2,YSTR2,8)
      CALL CURVED(XMOUTH,YMOUTH,5)
      CALL CURVED(XHEAD,YHEAD,28)
      CALL CURVED(XNECK,YNECK,2)
      CALL CURVED(XTAIL,YTAIL,16)
      CALL CURVED(XRHLEG,YRHLEG,8)
      CALL CURVED(XBELLY,YBELLY,3)
      CALL CURVED(XFFLEG,YFFLEG,14)
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
