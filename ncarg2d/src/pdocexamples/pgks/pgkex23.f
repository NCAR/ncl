
      PROGRAM PGKEX23
C
C  Illustrate changing spacing between hatch pattern lines.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1, IDIM=12)
C
      CHARACTER*15 LAB
      DIMENSION XL(4),XR(4),YB(4),YT(4),IHATCH(4),RSPACE(4)
      DATA XL/ 0.05, 0.55, 0.05, 0.55/
      DATA XR/ 0.45, 0.95, 0.45, 0.95/
      DATA YT/ 0.90, 0.90, 0.40, 0.40/
      DATA YB/ 0.50, 0.50, 0.00, 0.00/
      DATA IHATCH/ 1, 1, 3, 6/
      DATA RSPACE/ 0.01, 0.02, 0.03, 0.05/
C
C  Open GKS.
C
      CALL GOPKS (IERRF, IDUM)
C
C  Open and activate a color PostScript workstation.
C
      CALL GOPWK (IWKID, 2, NGPSWK('PS','PORT','COLOR'))
      CALL GACWK (IWKID)
C
C  Set up color table.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,0.,0.,1.)
C
      CALL GSFAIS(3)
      CALL NGSETI('Workstation ID',IWKID)
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',1)
      DO 10 I=1,4
        CALL GSFAIS(3)
        CALL GSFASI(IHATCH(I))
        CALL NGSETR('Hatch spacing',RSPACE(I))
        CALL ZIGZAG(XL(I),XR(I),YB(I),YT(I))
        CALL GSFAIS(1)
        WRITE(LAB,500) IHATCH(I)
  500   FORMAT('Style index = ',I1)
        CALL PLCHHQ(XL(I)+0.21,YT(I)-0.02,LAB,0.022,0.,0.)
        WRITE(LAB,510) RSPACE(I)
  510   FORMAT('Spacing = ',F4.2,' ')
        CALL PLCHHQ(XL(I)+0.32,YT(I)-0.09,LAB,0.022,0.,0.)
   10 CONTINUE
C
      CALL PLCHHQ(.5,.98,'Hatch line spacings',0.035,0.,0.)
C
      CALL FRAME
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE ZIGZAG(XLEFT,XRIGHT,YBOT,YTOP)
C
C  Draw a zigzag figure within the specified limits for a rectangle.
C
      PARAMETER (IDIM=10)
      DIMENSION X(IDIM),Y(IDIM)
C
      DX = (XRIGHT-XLEFT)/8.
      DY = (YTOP-YBOT)/10.
C
      DO 10 I=1,IDIM-1
        X(I) = XLEFT+(I-1)*DX
   10 CONTINUE
      X(IDIM) = X(1)
C
      Y(1) = YTOP-2.*DY
      Y(2) = YTOP
      Y(3) = YBOT
      Y(4) = YTOP-2.0*DY
      Y(5) = YBOT
      Y(6) = YTOP-4.0*DY
      Y(7) = YBOT
      Y(8) = YTOP-6.0*DY
      Y(9) = YTOP-8.5*DY
      Y(10) = Y(1)
C
      CALL GSFACI(2)
      CALL GFA(IDIM,X,Y)
      CALL GSPLCI(1)
      CALL GPL(IDIM,X,Y)
C
      RETURN
      END
