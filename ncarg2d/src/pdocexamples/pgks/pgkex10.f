
      PROGRAM PGKEX10
C
C  Illustrate GKS text path and color.
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
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Define colors.
C
      CALL GSCR(IWKID,0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID,1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID,2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID,3, 0.0, 0.0, 1.0)
      CALL GSCR(IWKID,4, 0.0, 1.0, 0.0)
      CALL GSCR(IWKID,5, 0.4, 0.0, 0.4)
      CALL GSCR(IWKID,6, 1.0, 0.0, 0.0)
C
C  Select Triplex Roman font.
C
      CALL GSTXFP(-13,2)
C
C  Text path = right, color = black
C
      X = .2
      Y = .7
      CALL GSCHH(.04)
      CALL GSTXP(0)
      CALL GSTXCI(1)
      CALL GSTXAL(1,3)
      CALL GTX(X,Y,'Text path=right')
      CALL CROSS(X,Y)
C
C  Text path = left, color = blue
C
      X = .80
      Y = .115
      CALL GSCHH(.04)
      CALL GSTXP(1)
      CALL GSTXCI(3)
      CALL GSTXAL(3,3)
      CALL GTX(X,Y,'Text path=left')
      CALL CROSS(X,Y)
C
C  Text path = down, color = red
C
      X = .22
      Y = .62
      CALL GSCHH(.025)
      CALL GSTXP(3)
      CALL GSTXCI(2)
      CALL GSTXAL(2,1)
      CALL GTX(X,Y,'Text path=down')
      CALL CROSS(X,Y)
C
C  Text path = up, color = green
C
      X = .79
      Y = .18
      CALL GSCHH(.03)
      CALL GSTXP(2)
      CALL GSTXCI(4)
      CALL GSTXAL(2,5)
      CALL GTX(X,Y,'Text path=up')
      CALL CROSS(X,Y)
C
C  Label the plot using Plotchar.
C
      CALL GSPLCI(4)
      CALL GSLWSC(2.)
      CALL PCSETI('CD',1)
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',5)
      CALL PLCHHQ(.5,.93,'Text colors and paths',.028,0.,0.)
      CALL PLCHHQ(.5,.88,'Font = triplex Roman',.028,0.,0.)
      CALL CROSS(.193,.826)
      CALL PLCHHQ(.5,.83,'- marks the GTX coordinate',.028,0.,0.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE CROSS(X,Y)
C
C  Draw a green filled cross at (X,Y).
C
      PARAMETER(ID=16)
      PARAMETER(IX=15, IMX=100,IMXH=IMX/2)
      PARAMETER(IMXM=IMX-IX, IMXP=IMX+IX, IMXHM=IMXH-IX, IMXHP=IMXH+IX)
      DIMENSION ICX(ID),ICY(ID),RCX(ID),RCY(ID)
C
      DATA ICX( 1),ICX( 2),ICX( 3),ICX( 4)/    0,    IX,  IMXH, IMXM/
      DATA ICY( 1),ICY( 2),ICY( 3),ICY( 4)/    0,     0, IMXHM,    0/
      DATA ICX( 5),ICX( 6),ICX( 7),ICX( 8)/  IMX,   IMX, IMXHP,  IMX/
      DATA ICY( 5),ICY( 6),ICY( 7),ICY( 8)/    0,    IX,  IMXH, IMXM/
      DATA ICX( 9),ICX(10),ICX(11),ICX(12)/  IMX,  IMXM,  IMXH,   IX/
      DATA ICY( 9),ICY(10),ICY(11),ICY(12)/  IMX,   IMX, IMXHP,  IMX/
      DATA ICX(13),ICX(14),ICX(15),ICX(16)/    0,     0, IMXHM,    0/
      DATA ICY(13),ICY(14),ICY(15),ICY(16)/  IMX,  IMXM,  IMXH,   IX/
C
      DO 10 I=1,ID
        RCX(I) = X-0.00025*(REAL(IMXH)-REAL(ICX(I)))
        RCY(I) = Y-0.00025*(REAL(IMXH)-REAL(ICY(I)))
   10 CONTINUE
      CALL GSFAIS(1)
      CALL GSFACI(5)
      CALL GFA(ID,RCX,RCY)
C
      RETURN
      END
