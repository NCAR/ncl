      PROGRAM TEST
C
C  Illustrate workstation control functions by intermixing plotting to
C  a metafile with plotting to X window workstations.
C
C
C  Open GKS
C
      CALL GOPKS(6,0)
C
C  Open a CGM workstation and define color index 6 as red on that
C  workstation.
C
      CALL GOPWK(1,2,1)
      CALL GSCR(1,6,1.,0.,0.)
C
C  Set character height and set the text alignment to (center, half).
C
      CALL GSCHH(.04)
      CALL GSTXAL(2,3)
C
C  Activate the CGM workstation.
C
      CALL GACWK(1)
C
C  Create picture 1 in the metafile and call FRAME to terminate 
C  the first picture.
C
      CALL GSTXCI(6)
      CALL GTX(.5,.5,'Picture 1')
      CALL FRAME
C
C  Put another picture in the metafile.
C
      CALL GTX(.5,.5,'Picture 2')
      CALL FRAME
C
C  Open and activate an X11 workstation and define color index 6
C  to be cyan on that workstation (this workstation will be referred 
C  to as "The first window".
C
      CALL GOPWK(5,0,8)
      CALL GACWK(5)
      CALL GSCR(5,6,0.,1.,1.)
C
C  Create picture 3.  This will be plotted to the CGM workstation as
C  well as to the X11 window since they are both active.
C
      CALL GTX(.5,.5,'Picture 3')
C
C  Terminate the metafile picture.
C
      CALL NGPICT(1,1)
C
C  Pause in the X window with a "<READY>" prompt and wait for a 
C  mouse click.  The window will be cleared after the mouse click.
C
      CALL NGPICT(5,4)
C
C  Open and activate another X workstation (to be reffered to as "The
C  second window") and define color index 6 to be green on that 
C  workstation.
C
      CALL GOPWK(7,0,8)
      CALL GACWK(7)
      CALL GSCR(7,6,0.,1.,0.)
C
C  Plot picture 4.  This will be sent to the CGM workstation and the
C  two X window workstations since they are all active.
C
      CALL GTX(.5,.5,'Picture 4')
C
C  Terminate picture 4 in the metafile.
C
      CALL NGPICT(1,1)
C
C  Make the second window current.
C
      CALL NGPICT(7,0)
C
C  Pause in the first window with a "<READY>" prompt and wait for a
C  mouse click.  The window will be cleared after the click.
C
      CALL NGPICT(5,4)
C
C  Clear the second window.
C
      CALL NGPICT(7,1)
C
C  Deactivate the metafile (but do not close it) and draw picture 5.
C  This will go to the two active X11 workstations, but not the CGM.
C
      CALL GDAWK(1)
      CALL GTX(.5,.5,'Picture 5')
C
C  Make the second window current
C
      CALL NGPICT(7,0)
C
C  Pause in the first window waiting or a mouse click
C
      CALL NGPICT(5,4)
C
C  Re-activate the metafile and deactivate the second window.
C
      CALL GACWK(1)
      CALL GDAWK(7)
C
C  Plot picture 6.  This will go to the first window and to the
C  metafile.
C
      CALL GTX(.5,.5,'Picture 6')
C
C  Terminate the picture in the CGM.
C
      CALL NGPICT(1,1)
C
C  Pause in the first window waiting for a mouse click.
C
      CALL NGPICT(5,4)
C
C  Reactivate the second window and clear it.
C
      CALL GACWK(7)
      CALL NGPICT(7,1)
C
C  Put out picture 7.  This will go to the all active workstations.
C
      CALL GTX(.5,.5,'Picture 7')
C
C  Terminate the picture in the CGM.
C
      CALL NGPICT(1,1)
C
C  Make the first window current.
C
      CALL NGPICT(5,0)
C
C  Pause in the second window waiting for a mouse click.
C
      CALL NGPICT(7,4)
C
C  Deactivate and close the first window; deactivate and close the CGM.
C 
      CALL GDAWK(5)
      CALL GDAWK(1)
      CALL GCLWK(5)
      CALL GCLWK(1)
C
C  Put out picture 8.  This will go to the second window, the only 
C  active workstation.
C
      CALL GTX(.5,.5,'Picture 8')
C
C  Pause in the second window with a "<READY>" prompt.
C
      CALL NGPICT(7,4)
C
C  Deactivate and close the second window.
C
      CALL GDAWK(7)
      CALL GCLWK(7)
C
C  Close GKS.
C
      CALL GCLKS
C
      STOP
      END
