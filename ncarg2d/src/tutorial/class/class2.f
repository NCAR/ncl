	PROGRAM CLASS2

	PARAMETER (NPTS=200)
	PARAMETER (NCURVE=4)
	REAL YDRA(NPTS,NCURVE),XDRA(NPTS)
	CHARACTER*27 STRING

C Generate some data
	DO 10 I=1,NPTS
	  XDRA(I )=I*0.1
	  DO 10 J=1,NCURVE
	    YDRA(I,J)=SIN(XDRA(I)+0.2*J)*EXP(-0.01*XDRA(I)*J**2)
 10	CONTINUE

C Open GKS
	CALL OPNGKS

C Set up a color table
	CALL DEFCLR

C Label the axes
	CALL PLCHHQ(.5,.05,'Time (seconds)',.012,0.,0.)
	CALL PLCHHQ(.025,.5,'Position (meters)',.012,90.,0.)

C Set the window for the range of the data and set the viewport to
C protect the axis labels
	CALL SET(.1,.9,.1,.9,0.0,20.0,-1.4,1.4,1)

C Set up tick mark labels
	CALL LABMOD('(I2)','(F4.1)',0,0,8,8,0,0,0)

C Draw axes and their labels
	CALL GRIDAL(10,2,15,2,1,1,5,0.0,0.0)

C Draw each curve with a different label
	DO 20, I=1,NCURVE
	  WRITE (STRING,'(A,I1)') 26H'$$$$$$$$$$$$$$$$''Curve'',I
	  CALL DASHDC(STRING,1,1)
	  CALL GSPLCI(I+1)
	  CALL GSTXCI(I+1)
	  CALL CURVED(XDRA,YDRA(1,I),NPTS)
 20	CONTINUE

C Close the frame
	CALL FRAME

C Close GKS
	CALL CLSGKS

	STOP
	END

	SUBROUTINE DEFCLR
C Define a color table
C
C Bacground color is black
	CALL GSCR(1, 0, 0.0, 0.0, 0.0)
C Default foreground color is white
	CALL GSCR(1, 1, 1.0, 1.0, 1.0)
C Red
	CALL GSCR(1, 2, 1.0, 0.0, 0.0)
C Green
	CALL GSCR(1, 3, 0.0, 1.0, 0.0)
C Blue 
	CALL GSCR(1, 4, 0.4, 0.7, 0.9)
C Magenta
	CALL GSCR(1, 5, 0.7, 0.4, 0.7)
C Orange
	CALL GSCR(1, 6, 0.9, 0.7, 0.4)
C Teal
	CALL GSCR(1, 7, 0.4, 0.9, 0.7)
	RETURN
	END
