
      PROGRAM PGKEX09
C
C  A plot illustrating the text extent rectangle.
C
      CHARACTER*13 STRING
      DATA STRING/'NCAR Graphics'/
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
C  Open GKS, open and activate a CGM workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Specify the text font and color for Plotchar.
C
      CALL PCSETI('FN',21)
      CALL PCSETI('CC',1)
C
C  Calculate the initial X start point to center the string.
C
      CALL PCSETI('TE',1)
      CALL PLCHHQ(0.,.5,STRING,.1*6./7.,360.,-1.)
      CALL PCSETI('TE',0)
      CALL PCGETR('XE',TRIGHT)
      XSTART = .5*(1.-TRIGHT)
C
C  Draw the characters and boxes around the character bodies.
C
      LSTR = LEN(STRING)
      XIN  = XSTART
      YPOS = .475
      DO 10 I=1,LSTR
        CALL DRWBOD(XIN,YPOS,STRING(I:I),.1,XOUT,TOP,BOT)
        XIN = XOUT
   10 CONTINUE
      XEND = XOUT
C
C  Label the plot.
C
      CALL PCSETI('FN',25)
      CALL PLCHHQ(.5,.80,'Text Extent Rectangle',.040,0.,0.)
      CALL PLCHHQ(.5,.72,
     + 'A concatenation of character bodies',.035,0.,0.)
      CALL PCSETI('FN',21)
      CALL PLCHHQ(.5,.64,
     + 'The hatched area shades the text extent rectangle',.025,0.,0.)
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
      SUBROUTINE DRWBOD(XIN,Y,CHR,CHGT,XOUT,TOP,BOT)
C
C  Draw the character in CHR left-centered at (XIN,Y) with character
C  height CHGT.  Draw a box around the character body.  Return the 
C  right of the character body in XOUT and the font top and bottom
C  in TOP and BOTTOM.
C
      CHARACTER*1 CHR
      DIMENSION XB(5),YB(5)
C
      CAP =  Y+.5*CHGT
      BASE = Y-.5*CHGT
      HALF = .5*(CAP+BASE)
      TOP = HALF+.7*(CAP-BASE)
      BOT = HALF-.8*(CAP-BASE)
C
C  Convert the character height to width for Plotchar.
C
      CWIDTH = (6./7.)*(CAP-BASE)
C
C  Compute the text extent information.
C
      CALL PCSETI('TE',1)
      CALL PLCHHQ(XIN,HALF,CHR,CWIDTH,360.,-1.)
      CALL PCSETI('TE',0)
      CALL PCGETR('XB',TLEFT)
      CALL PCGETR('XE',TRIGHT)
C
C  Draw a box around the character body limits and hatch the interior.
C
      XB(1) = TLEFT
      XB(2) = TRIGHT
      XB(3) = TRIGHT
      XB(4) = TLEFT
      XB(5) = XB(1)
      YB(1) = BOT
      YB(2) = BOT
      YB(3) = TOP
      YB(4) = TOP
      YB(5) = YB(1)
      CALL GSLWSC(2.)
      CALL GPL(5,XB,YB)
      CALL GSFAIS(3)
      CALL GSFASI(6)
      CALL GFA(5,XB,YB)
      CALL GSLWSC(1.)
      CALL GSFAIS(1)
C
C  Draw the character.
C
      CALL PLCHHQ(XIN,HALF,CHR,CWIDTH,0.,-1.)
C
C  Return the right limit of the character body.
C
      XOUT = TRIGHT
C
      RETURN
      END
