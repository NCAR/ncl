
      PROGRAM PGKEX03
C
C  Use inquiry functions to implement a subroutine (DRWTXT below)
C  that puts out text using Normalized Device Coordinates (NDC) 
C  for positioning.  Invoke Autograph to draw a linear/log plot
C  and then call DRWTXT two times to label the Autograph plot.
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

      DIMENSION X(100),Y(100)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDUM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a small color table.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,1.)
      CALL GSCR(IWKID,2,1.,0.,0.)
C
C  Generate a straight line with 100 points.
C
      DO 1 I=1,100
        X(I) = I
        Y(I) = 10.*I
  1   CONTINUE
C
C  Use SET to define normalization transformation 1 with linear 
C  scaling in the X direction and log scaling in the Y direction.
C
      CALL SET(.15,.85,.15,.85,1.,100.,10.,1000.,2)
C
C  Set line color to red.
C
      CALL GSPLCI(2)
C
C  Initialize the AUTOGRAPH entry EZXY so that the frame is not 
C  advanced and the Y axis is logarithmic.  Turn off axis labels.
C
      CALL DISPLA(2,0,2)
      CALL ANOTAT(' ',' ',0,0,0,0)
C
C  Output the polyline (X,Y) using EZXY.
C
      CALL EZXY(X,Y,100,' ')
C
C  Put out a couple of labels (DRWTXT uses NDC space).
C
      CALL DRWTXT(.50,.07,'The X axis is linear',-7,.025,0.)
      CALL DRWTXT(.07,.50,'The Y axis is log',-7,.025,90.)
C
C  Terminate the picture, deactivate and close the CGM workstation,
C  and close GKS.
C
      CALL FRAME
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE DRWTXT(X,Y,TXT,IFNT,CHGT,ANG)
C
C  This subroutine draws the text string in TXT at position (X,Y) using
C  font IFNT with character height CHGT (specified in NDC) and text 
C  angle ANG degrees.  The position (X,Y) is in NDC. This subroutine 
C  is isolated from any GKS attribute settings in the calling program 
C  by using inquiry functions to save all settings on entry and restore 
C  all settings on exit.  The text is aligned as (center, half) and is 
C  drawn in the foreground color.  
C
      INTEGER   ERRIND
      DIMENSION CLRECT(4)
      CHARACTER*(*) TXT
      DATA PI  /3.1415927/
C
C  Inquire and save the state of all attributes that will be used in
C  this subroutine.  These will be restored on exit.
C
C   Clipping
      CALL GQCLIP(ERRIND,ICLIPO,CLRECT)
C
C   Character up vector.
      CALL GQCHUP(ERRIND,CHUPXO,CHUPYO)
C
C   Text alignment.
      CALL GQTXAL(ERRIND,ILNHO,ILNVO)
C
C   Text font.
      CALL GQTXFP(ERRIND,ITXFO,ITXPO)
C
C   Character height.
      CALL GQCHH(ERRIND,CHHO)
C
C  Get and save the existing normalization transformation information,
C  including the log scaling parameter..
C
      CALL GETSET(XV1,XV2,YV1,YV2,XW1,XW2,YW1,YW2,LS)
C
C  Use NDC space for drawing TXT.
C
      CALL GSELNT(0)
C
C  Define the text font.
C
      CALL GSTXFP(IFNT,2)
C
C  Set the character height.
C
      CALL GSCHH(CHGT)
C
C  Set the text alignment to (center, half).
C
      CALL GSTXAL(2,3)
C
C  Select the foreground color.
C
      CALL GSTXCI(1)
C
C  Define the character up vector in accordance with ANG (recall that
C  the up vector is perpendicular to the text path).
C
      RANG = (ANG+90.)*(2.*PI/360.)
      CALL GSCHUP(COS(RANG),SIN(RANG))
C
C  Draw the text string in TXT.
C
      CALL GTX(X,Y,TXT)
C
C  Restore the original normalization transformation.
C
      CALL SET(XV1,XV2,YV1,YV2,XW1,XW2,YW1,YW2,LS)
C
C  Restore all other attributes.
C
      CALL GSCLIP(ICLIPO)
      CALL GSCHUP(CHUPXO,CHUPYO)
      CALL GSTXAL(ILNHO,ILNVO)
      CALL GSTXFP(ITXFO,ITXPO)
      CALL GSCHH (CHHO)
C
      RETURN
      END
