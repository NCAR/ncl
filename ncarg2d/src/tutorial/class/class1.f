C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
      INTEGER IND(100)
      REAL XBALLN(15),YBALLN(15),XSTR1(2),YSTR1(2)
      REAL XSTR2(8),YSTR2(8), XMOUTH(5),YMOUTH(5),EYE(2)
      REAL XHEAD(28),YHEAD(28),XNECK(2),YNECK(2)
      REAL XBLNKT(11),YBLNKT(11), XTAIL(16),YTAIL(16)
      REAL XRHLEG(8),YRHLEG(8), XBELLY(3),YBELLY(3)
      REAL XFFLEG(14),YFFLEG(14),DST(100)
      
      OPEN(FILE='class1.dat',UNIT=3,STATUS='old')
C
C Read in the data
C
      READ (3,*) (XBALLN(I),I=1,15)
      READ (3,*) (YBALLN(I),I=1,15)
      READ (3,*) (XSTR1(I),I=1,2)
      READ (3,*) (YSTR1(I),I=1,2)
      READ (3,*) (XSTR2(I),I=1,8)
      READ (3,*) (YSTR2(I),I=1,8)
      READ (3,*) (XMOUTH(I),I=1,5)
      READ (3,*) (YMOUTH(I),I=1,5)
      READ (3,*) (EYE(I),I=1,2)
      READ (3,*) (XHEAD(I),I=1,28)
      READ (3,*) (YHEAD(I),I=1,28)
      READ (3,*) (XNECK(I),I=1,2)
      READ (3,*) (YNECK(I),I=1,2)
      READ (3,*) (XBLNKT(I),I=1,11)
      READ (3,*) (YBLNKT(I),I=1,11)
      READ (3,*) (XTAIL(I),I=1,16)
      READ (3,*) (YTAIL(I),I=1,16)
      READ (3,*) (XRHLEG(I),I=1,8)
      READ (3,*) (YRHLEG(I),I=1,8)
      READ (3,*) (XBELLY(I),I=1,3)
      READ (3,*) (YBELLY(I),I=1,3)
      READ (3,*) (XFFLEG(I),I=1,14)
      READ (3,*) (YFFLEG(I),I=1,14)
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
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
