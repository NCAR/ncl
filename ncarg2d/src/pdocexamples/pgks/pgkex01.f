      PROGRAM TYPGKS
C
C  Define a couple of triangles
C
      DIMENSION XT1(3),YT1(3),XT2(3),YT2(3)
      DATA XT1 /10., 90., 50./
      DATA YT1 /0.2, 0.2, 0.3/
      DATA XT2 /10., 90., 50./
      DATA YT2 /0.5, 0.5, 0.4/
      DATA IER, MUNIT, IDWK /6, 2, 1/
C
C  Open GKS, open and activate a GCM workstation.
C
      CALL GOPKS (IER, ISZ)
      CALL GOPWK (IDWK, MUNIT, 1)
      CALL GACWK (IDWK)
C
C  Define a normalization transformation and select it.
C
      CALL GSVP (1, .05, .95, .05, .95)
      CALL GSWN (1, 1., 100., .1, .5)
      CALL GSELNT (1)
C
C  Set up a color table for the CGM workstation.
C
      CALL GSCR (1, 1, 0., 1., 0.)
      CALL GSCR (1, 2, 1., 1., 0.)
      CALL GSCR (1, 3, 1., 0., 0.)
C
C  Set fill area interior style to solid.
C
      CALL GSFAIS (1)
C
C  Fill triangle 1 with green, triangle 2 with yellow.
C
      CALL GSFACI (1)
      CALL GFA (3, XT1, YT1)
      CALL GSFACI (2)
      CALL GFA (3, XT2, YT2)
C
C  Select normalization transformation 0 for drawing the text
C  to avoid effects of the non-square aspect ratio of the 
C  normalizartion transformation on the plotted characters.
C
      CALL GSELNT(0)
C
C  Set text color to red; align the text as (center, half); 
C  specify the text size; and draw it.
C
      CALL GSTXCI (3)
      CALL GSTXAL(2,3)
      CALL GSCHH(.023)
      CALL GTX (.5,.125,'A Typical GKS Program')
C
C  Advance the frame to ensure all output is plotted.
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (IDWK)
      CALL GCLWK (IDWK)
      CALL GCLKS
C
      STOP
      END
