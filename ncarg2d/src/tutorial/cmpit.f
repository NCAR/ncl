C
C	$Id: cmpit.f,v 1.2 1992-12-17 23:39:32 haley Exp $
C
      PARAMETER(IGRD=2)
      PARAMETER(M=180/IGRD,N=360/IGRD)
C
C Open GKS.
C
      CALL OPNGKS
C
C Draw a map
C
      CALL SUPMAP(8,0.,0.,0.,1,0.,0.,0.,0.,0.,0,5,0,IERR)
C
C Draw longitude lines at 2 degree intervals over the map.
C
	DO 1, I=-90,90,IGRD
	  DO 2 J=-180, 180-IGRD,IGRD
	    LEFT  = (J+181)*1000 + (I+91)
	    CALL MAPIT(REAL(I),REAL(J)     ,0)
	    CALL MAPIT(REAL(I),REAL(J+IGRD),1)
 2	  CONTINUE
	    CALL MAPIQ
 1	CONTINUE

C
C Draw latitude lines at 2 degree intervals over the map.
C
	DO 3, I=-180, 180,IGRD
	  DO 4 J=-90,90-IGRD,IGRD
	    CALL MAPIT(REAL(J),     REAL(I),0)
	    CALL MAPIT(REAL(J+IGRD),REAL(I),1)
 4	  CONTINUE
	    CALL MAPIQ
 3	CONTINUE
C
C Put the label at the top of the plot.
C
      CALL SET (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLCHHQ (.5,.85,'Drawing Lines on a Map',.017,0.,0.)
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL CLSGKS
C
C Done.
C
      STOP
C
      END

