
      PROGRAM PGKEX08
C
C  Demo a plot describing the GKS font coordinate system.
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

      DIMENSION XB(4),YB(4)
C
C  Open GKS, open and activate a CGM workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Get the character body limits.
C
      XCENT = .5
      CAP =  .7
      BASE = .3
      HALF = .5*(CAP+BASE)
      TOP = HALF+.7*(CAP-BASE)
      BOT = HALF-.8*(CAP-BASE)
C
C  Convert the character height to width for Plotchar.
C
      CWIDTH = (6./7.)*(CAP-BASE)
C
C  Turn computation of text extent information on.
C
      CALL PCSETI('TE',1)
C
C  Specify the text font and color.
C
      CALL PCSETI('FN',21)
      CALL PCSETI('CC',1)
C
C  Compute the text extent information.
C
      CALL PLCHHQ(XCENT,HALF,'B',CWIDTH,360.,0.)
C
C  Turn text extent computation off.
C
      CALL PCSETI('TE',0)
      CALL PCGETR('XB',TLEFT)
      CALL PCGETR('XE',TRIGHT)
      EXTL = .15
      EXTR = .05
C
C  Draw a hatch pattern in the character body limits.
C
      XB(1) = TLEFT
      XB(2) = TRIGHT
      XB(3) = TRIGHT
      XB(4) = TLEFT
      YB(1) = BOT
      YB(2) = BOT
      YB(3) = TOP
      YB(4) = TOP
      CALL GSFAIS(3)
      CALL GSFASI(6)
      CALL GFA(4,XB,YB)
      CALL GSFAIS(1)
C
C  Draw the character.
C
      CALL PLCHHQ(XCENT,HALF,'B',CWIDTH,0.,0.)
C
C  Label the plot.
C
      CALL LINE(TLEFT-EXTR,TOP,TRIGHT+EXTR,TOP)
      CALL LINE(TLEFT-EXTL,CAP,TRIGHT+EXTR,CAP)
      CALL LINE(TLEFT,HALF,TRIGHT+EXTR,HALF)
      CALL LINE(TLEFT-EXTL,BASE,TRIGHT+EXTR,BASE)
      CALL LINE(TLEFT-EXTR,BOT,TRIGHT+EXTR,BOT)
C
      CALL LINE(TLEFT,TOP+EXTR,TLEFT,BOT-EXTR)
      CALL LINE(TRIGHT,TOP+EXTR,TRIGHT,BOT-EXTR)
      CALL LINE(XCENT,TOP+EXTR,XCENT,BOT-EXTR)
C
      SZ = .015
      CALL PLCHHQ(TRIGHT+EXTR+.01,TOP,'top',SZ,0.,-1.)
      CALL PLCHHQ(TRIGHT+EXTR+.01,CAP,'cap',SZ,0.,-1.)
      CALL PLCHHQ(TRIGHT+EXTR+.01,HALF,'half',SZ,0.,-1.)
      CALL PLCHHQ(TRIGHT+EXTR+.01,BASE,'base',SZ,0.,-1.)
      CALL PLCHHQ(TRIGHT+EXTR+.01,BOT,'bottom',SZ,0.,-1.)
      CALL PLCHHQ(TLEFT,BOT-EXTR-.025,'left',SZ,0.,0.)
      CALL PLCHHQ(XCENT,BOT-EXTR-.025,'center',SZ,0.,0.)
      CALL PLCHHQ(TRIGHT,BOT-EXTR-.025,'right',SZ,0.,0.)
C
      SZ = .013
      CNTL = TLEFT-.08
      CALL PLCHHQ(CNTL,HALF+.050,'CHARACTER',SZ,0.,0.)
      CALL PLCHHQ(CNTL,HALF+.020,'HEIGHT',SZ,0.,0.)
C
      CALL ARW(CNTL,CAP,0)
      CALL ARW(CNTL,BASE,1)
      CALL LINE(CNTL,HALF+.050+.02,CNTL,CAP)
      CALL LINE(CNTL,HALF,CNTL,BASE)
C
      CALL PLCHHQ(XCENT,.89,
     +            'The hatched area denotes the character body',
     +            .016,0.,0.)
      CALL PCSETI('FN',25)
      CALL PLCHHQ(XCENT,.94,'Font Coordinate System',.024,0.,0.)
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
      SUBROUTINE ARW(XP,YP,IP)
C
C  Draws an arrow tip at (X,Y) which is up if IP=0 and down if IP=1
C
      DIMENSION X(3),Y(3)
      DATA DX,DY/.01,.035/
      IYS = 1
      IF (IP .EQ. 0) THEN
        IYS = -1
      ENDIF
      X(1) = XP-DX
      X(2) = XP
      X(3) = XP+DX
      Y(1) = YP+IYS*DY
      Y(2) = YP
      Y(3) = YP+IYS*DY
      CALL GFA(3,X,Y)
      RETURN
      END
