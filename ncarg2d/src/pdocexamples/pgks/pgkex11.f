
      PROGRAM PGKEX11
C
C  Illustrate text alignment attributes.
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
      CALL GSCR(IWKID,4, 0.4, 0.0, 0.4)
C
C  Specify the character height for all strings, select simplex
C  Roman font.
C
      CALL GSCHH(.025)
      CALL GSTXFP(-4,2)
      CALL GSTXCI(3)
      X = .50
      Y = .90
C
C  Alignment = (1,3) [  left, center]
C
      Y = Y-.1
      CALL GSTXAL(1,3)
      CALL GTX(X,Y,'Alignment = (1,3)')
      CALL CROSS(X,Y)
C
C  Alignment = (2,3) [center, center]
C
      Y = Y-.1
      CALL GSTXAL(2,3)
      CALL GTX(X,Y,'Alignment = (2,3)')
      CALL CROSS(X,Y)
C
C  Alignment = (3,3) [ right, center]
C
      Y = Y-.1
      CALL GSTXAL(3,3)
      CALL GTX(X,Y,'Alignment = (3,3)')
      CALL CROSS(X,Y)
C
C  Alignment = (1,1) [  left,    top]
C
      X = .25
      Y = Y-.1
      CALL GSTXAL(1,1)
      CALL GTX(X,Y,'Alignment = (1,1)')
      CALL CROSS(X,Y)
C
C  Alignment = (1,2) [  left,    cap]
C
      Y = Y-.1
      CALL GSTXAL(1,2)
      CALL GTX(X,Y,'Alignment = (1,2)')
      CALL CROSS(X,Y)
C
C  Alignment = (1,3) [  left, center]
C
      Y = Y-.1
      CALL GSTXAL(1,3)
      CALL GTX(X,Y,'Alignment = (1,3)')
      CALL CROSS(X,Y)
C
C  Alignment = (1,4) [  left,   base]
C
      Y = Y-.1
      CALL GSTXAL(1,4)
      CALL GTX(X,Y,'Alignment = (1,4)')
      CALL CROSS(X,Y)
C
C  Alignment = (1,5) [  left, bottom]
C
      Y = Y-.1
      CALL GSTXAL(1,5)
      CALL GTX(X,Y,'Alignment = (1,5)')
      CALL CROSS(X,Y)
C
C  Label the plot.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',4)
      CALL PLCHHQ(.5,.93,'Text Alignment Attributes',.025,0.,0.)
      CALL CROSS(.22,.876)
      CALL PLCHHQ(.5,.88,'- marks the GTX coordinate',.025,0.,0.)
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
C  Draw a filled cross at coordinate (X,Y) using color index 3.
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
      CALL GSFACI(2)
      CALL GFA(ID,RCX,RCY)
C
      RETURN
      END
