      PROGRAM FCCE01
C
C  This program provides a demonstration of setting up a color table 
C  and specifying colors for the GKS primitives: lines, markers, text, 
C  and filled areas.
C
C  Corrdinate data for polyline and polymarker draws.
C
       DIMENSION X1(5),X2(5),Y(5)
       DATA X1/0.15, 0.35, 0.35, 0.15, 0.15/
       DATA X2/0.65, 0.85, 0.85, 0.65, 0.65/
       DATA Y /0.45, 0.45, 0.65, 0.65, 0.45/
C
C  Open GKS and open and activate a CGM workstation.
C
      CALL GOPKS(6,0)
      IWK = 1
      CALL GOPWK(IWK,2,1)
      CALL GACWK(IWK)
C
C  Set up the color table for the CGM workstation:
C    
C    Index  Color
C    -----  -----
C      0    Black (background color)
C      1    White (foreground color)
C      2    Red
C      3    Green
C      4    Yellow
C      5    Cyan
C
      CALL GSCR(IWK, 0, 0.0, 0.0, 0.0)
      CALL GSCR(IWK, 1, 1.0, 1.0, 1.0)
      CALL GSCR(IWK, 2, 1.0, 0.0, 0.0)
      CALL GSCR(IWK, 3, 0.0, 1.0, 0.0)
      CALL GSCR(IWK, 4, 1.0, 1.0, 0.0)
      CALL GSCR(IWK, 5, 0.0, 1.0, 1.0)
C
C  Draw a green rectangle.
C
      CALL GSPLCI(3)
      CALL GPL(5,X1,Y)
C
C  Draw an asterisk scaled by 4. in the foreground color.
C
      CALL GSMKSC(4.)
      CALL GPM(1,.5,.25)
C
C  Draw a text string in yellow.
C
      CALL GSTXCI(4)
      CALL GTX(0.5,0.5,'Text')
C
C  Draw a filled rectangle in cyan.
C
      CALL GSFACI(5)
      CALL GSFAIS(1)
      CALL GFA(5,X2,Y)
C
C  Draw a red asterisk.
C
      CALL GSPMCI(2)
      CALL GPM(1,.5,.75)
C
C  Terminate the picture, deactivate and close the CGM workstation, 
C  close GKS.
C
      CALL GDAWK(1)
      CALL GCLWK(1)
      CALL GCLKS
C
      END
