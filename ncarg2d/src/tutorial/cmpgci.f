C
C	$Id: cmpgci.f,v 1.1 1992-09-29 16:09:57 ncargd Exp $
C
      PARAMETER(IGRD=2)
      PARAMETER(M=180/IGRD,N=360/IGRD)
      REAL RLAT(100),RLON(100)
C
C Open GKS.
C
      CALL OPNGKS
C
C Draw a map
C
      CALL SUPMAP(8,0.,-50.,0.,0.,-80.,90.,10.,2,0.,0,0,IERR)

C
C Get data values defining a great circle between Washinton DC and
C London
C
	CALL MAPGCI(38.,-77.,51.,0.,100,RLAT,RLON)
C
C Draw the great circle
C
	CALL MAPIT(38.,-77.,0)
	DO 10, I=1,100
	  CALL MAPIT(RLAT(I),RLON(I),1)
 10	CONTINUE
	CALL MAPIT(51.,0.,1)
	CALL MAPIQ

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

