
      PROGRAM PGKEX14
C
C  Illustrate character height and up vector.
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

      DATA DTR / .017453292519943 /
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF,IDUM)
      CALL GOPWK (IWKID,LUNIT,IWTYPE)
      CALL GACWK (IWKID)
C
C  Define colors.
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,1.)
      CALL GSCR(IWKID,2,.4,.0,.4)
C
      CALL GSTXFP(-4,2)
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
      CALL GSTXCI(2)
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
      CALL GSTXCI(2)
      CALL GSCHH(.025)
      CALL GTX(.8,.7,'Vect.=(1.6,2.)')
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',25)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(.1,.89,'Character heights &',.035,0.,-1.)
      CALL PLCHHQ(.1,.82,'Character up vectors',.035,0.,-1.)
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
