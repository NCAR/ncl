      PROGRAM CHHTUP
C
C  Illustrate character height and up vector.
C
      DATA DTR / .017453292519943 /
C
C
C  Open GKS, open and activate the metafile workstation.
C
      CALL GOPKS (6,IDUM)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C  Define necessary color indices.
C
      CALL GSCR(1,0,0.,0.,.6)
      CALL GSCR(1,1,1.,1.,1.)
      CALL GSCR(1,2,1.,1.,0.)
      CALL GSCR(1,3,0.,1.,1.)
      CALL GSCR(1,4,0.,1.,0.)
C
      CALL GSTXFP(-12,2)
C
C  Alignment = [center, center]
C
      CALL GSTXAL(2,3)
C
C  Loop through angles from 1 degree to 135 degrees on a circular arc
C  in increments proportional to character heights.  Position text
C  strings centered on a circular arc with up vectors being tangents
C  to the arc.
C
      IANG = 1
  110 CONTINUE
      ANG=DTR*REAL(IANG)
      XOFF = .3
      YOFF = .2
C
C  Calculate an (X,Y) coordinate on the arc.
C
      XCD=XOFF+.4*COS(ANG)
      YCD=YOFF+.4*SIN(ANG)
C
C  The up vector is tangent to the circular arc.
C
      CHUX = -(YCD-YOFF)
      CHUY =   XCD-XOFF
      CALL GSCHUP(CHUX,CHUY)
C
C  Scale the character heights depending on the angle and plot the text.
C
      CHH = .0004*(136-IANG)
      CALL GSCHH(CHH)
      CALL GTX(XCD,YCD,'NCAR')
C
C  Increment the angle by an amount proportional to the character heights.
C
      IANG = REAL(IANG)+MAX(210.*CHH,1.)
      IF (IANG .LE. 135) GO TO 110
C
C  Plot a character string with the up vector being down.
C
      CALL GSCHUP(0.,-1.)
      CALL GSCHH(.03)
      CALL GTX(.25,.34,'NCAR')
      CALL GSCHUP(0.,1.)
      CALL GSTXCI(3)
      CALL GSCHH(.025)
      CALL GTX(.25,.40,'Vect.=(0.,-1.)')
C
C  Plot a character string with up vector at an angle to the right.
C
      CALL GSCHUP(1.6,2.)
      CALL GSCHH(.03)
      CALL GSTXCI(1)
      CALL GTX(.65,.65,'NCAR')
      CALL GSCHUP(0.,1.)
      CALL GSTXCI(3)
      CALL GSCHH(.025)
      CALL GTX(.8,.7,'Vect.=(1.6,2.)')
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.1,.89,'Character heights &',.035,0.,-1.)
      CALL PLCHHQ(.1,.82,'Character up vectors',.035,0.,-1.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
      STOP
      END
