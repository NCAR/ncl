C
	REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

	DATA PLIM1 /0.,0./
	DATA PLIM2 /0.,0./
	DATA PLIM3 /0.,0./
	DATA PLIM4 /0.,0./
C
C Open GKS
C
      CALL OPNGKS 
C
C INVOKE DEMO DRIVER
C
      CALL GTXEXP('CE',-20.,-50.,0.,'PO','CO',21.3,-90.,-55.,-20.)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL CLSGKS

	STOP
	END

      SUBROUTINE GTXEXP(PROJ, PLAT, PLON, ROTA, OUTLN,
     +	JLIM, PLIM1, PLIM2, PLIM3, PLIM4)

	CHARACTER*2 PROJ, OUTLN, JLIM
	REAL PLIM1(2), PLIM2(2), PLIM3(2), PLIM4(2)

C
C PURPOSE                To provide a simple demonstration of the
C                        GKS GTX character drawing techniques.
C

C
C Set up a color table
C
       CALL DFCLRS
C
C Draw Continental, political outlines 
C
        CALL MAPSTC ('OU - OUTLINE DATASET SELECTOR',OUTLN)
C
C Set up projection
C
	CALL MAPROJ (PROJ,PLAT,PLON,ROTA)
C
C If it's a satellite projection, choose a satellite distance
C
	IF (PROJ.EQ.'SV') CALL MAPSTR ('SA - SATELLITE DISTANCE',5.)
C
C Set limits of map
C
        CALL MAPSET (JLIM,PLIM1,PLIM2,PLIM3,PLIM4)
C
C Turn off Grid lines
C
	CALL MAPSTR ('GR',0.)
C
C Turn off perimeter of map
C
        CALL MAPSTI ('PE', 0)
C
C Turn off map labels
C
        CALL MAPSTI ('LA', 0)
C
C Draw map
C
	CALL MAPDRW

C
C Label Brazil
C
C          Set text color
	   CALL GSTXCI(4)
C          Set text size
           CALL GSCHH(3.)
C	   Set the font
	   CALL GSTXFP(-16,2)
C          Transform the coordinates
   	   CALL MAPTRA(-10.,-60.,X,Y)
C          Draw the text
	   CALL GTX(X,Y,'Brazil')
           
C
C Label Argentina
C
C          Set text color
	   CALL GSTXCI(5)
C          Set text size
           CALL GSCHH(2.)
C	   Set the font
	   CALL GSTXFP(-14,2)
C          Transform the coordinates
   	   CALL MAPTRA(-43.,-68.,X,Y)
C          Set the angle
	   CALL GSCHUP (-1.,.3)
C          Draw the text
	   CALL GTX(X,Y,'Argentina')
           
C
C Label Uruguay
C
C          Set text color
	   CALL GSTXCI(6)
C          Set text size
           CALL GSCHH(1.0)
C	   Set the font
	   CALL GSTXFP(-4,2)
C          Transform the coordinates
   	   CALL MAPTRA(-32.5,-58.,X,Y)
C          Set the angle
	   CALL GSCHUP (.7,1.)
C          Draw the text
	   CALL GTX(X,Y,'Uruguay')

C
C Label Paraguay
C
C          Set text color
	   CALL GSTXCI(5)
C          Set text size
           CALL GSCHH(1.0)
C	   Set the font
	   CALL GSTXFP(-4,2)
C          Transform the coordinates
   	   CALL MAPTRA(-21.,-61.,X,Y)
C          Set the angle
	   CALL GSCHUP (1.,1.)
C          Draw the text
	   CALL GTX(X,Y,'Paraguay')

C
C Label Bolivia
C
C          Set text color
	   CALL GSTXCI(14)
C          Set text size
           CALL GSCHH(1.6)
C	   Set the font
	   CALL GSTXFP(-6,2)
C          Transform the coordinates
   	   CALL MAPTRA(-17.,-68.,X,Y)
C          Set the angle
	   CALL GSCHUP (0.,1.)
C          Draw the text
	   CALL GTX(X,Y,'Bolivia')

C
C Label Chile
C
C          Set text color
	   CALL GSTXCI(4)
C          Set text size
           CALL GSCHH(1.5)
C	   Set the font
	   CALL GSTXFP(-7,2)
C          Transform the coordinates
   	   CALL MAPTRA(-40.,-72.,X,Y)
C          Set the angle
	   CALL GSCHUP (-1.,0.03)
C          Expand the spacing between characters
	   CALL GSCHSP(2.)
C          Draw the text
	   CALL GTX(X,Y,'Chile')

C
C Label Peru
C
C          Set text color
	   CALL GSTXCI(14)
C          Set text size
           CALL GSCHH(3.)
C	   Set the font
	   CALL GSTXFP(-15,2)
C          Transform the coordinates
   	   CALL MAPTRA(-6.,-79.,X,Y)
C          Set the angle
	   CALL GSCHUP (1.,1.)
C          Reset the spacing between characters
	   CALL GSCHSP(0.)
C          Draw the text
	   CALL GTX(X,Y,'Peru')

C
C Label Equador
C
C          Set text color
	   CALL GSTXCI(15)
C          Set text size
           CALL GSCHH(1.5)
C	   Set the font
	   CALL GSTXFP(1,2)
C          Transform the coordinates
   	   CALL MAPTRA(0.,-86.,X,Y)
C          Set the angle
	   CALL GSCHUP (0.,1.)
C          Draw the text
	   CALL GTX(X,Y,'Equador')

C
C Label Colombia
C
C          Set text color
	   CALL GSTXCI(1)
C          Set text size
           CALL GSCHH(1.3)
C	   Set the font
	   CALL GSTXFP(-10,2)
C          Transform the coordinates
   	   CALL MAPTRA(7.,-77.,X,Y)
C          Set the angle
	   CALL GSCHUP (.8,1.)
C          Draw the text
	   CALL GTX(X,Y,'Colombia')

C
C Label Venezuela
C
C          Set text color
	   CALL GSTXCI(3)
C          Set text size
           CALL GSCHH(1.5)
C	   Set the font
	   CALL GSTXFP(-6,2)
C          Transform the coordinates
   	   CALL MAPTRA(7.,-70.,X,Y)
C          Set the angle
	   CALL GSCHUP (0.,1.)
C          Draw the text
	   CALL GTX(X,Y,'Venezuela')

C
C Label Guyana
C
C          Set text color
	   CALL GSTXCI(1)
C          Set text size
           CALL GSCHH(1.0)
C	   Set the font
	   CALL GSTXFP(-4,2)
C          Transform the coordinates
   	   CALL MAPTRA(7.,-59.5,X,Y)
C          Set the angle
	   CALL GSCHUP (1.,0.2)
C          Draw the text
	   CALL GTX(X,Y,'Guyana')

C
C Label Fr. Guyana
C
C          Set text color
	   CALL GSTXCI(1)
C          Set text size
           CALL GSCHH(1.2)
C	   Set the font
	   CALL GSTXFP(-4,2)
C          Transform the coordinates
   	   CALL MAPTRA(2.,-53.5,X,Y)
C          Set the angle
	   CALL GSCHUP (-1.,.5)
C          Draw the text
	   CALL GTX(X,Y,'Fr. Guyana')

C
C Label Suriname
C
C          Set text color
	   CALL GSTXCI(14)
C          Set text size
           CALL GSCHH(1.2)
C	   Set the font
	   CALL GSTXFP(-4,2)
C          Transform the coordinates
   	   CALL MAPTRA(2.5,-56.,X,Y)
C          Set the angle
	   CALL GSCHUP (-1.,.2)
C          Draw the text
	   CALL GTX(X,Y,'Suriname')

C
C Label the plot
C
C          Set text color
	   CALL GSTXCI(1)
C          Set text size
           CALL GSCHH(4.)
C	   Set the font
	   CALL GSTXFP(-14,2)
C          Transform the coordinates
   	   CALL MAPTRA(15.,-80.,X,Y)
C          Set the angle
	   CALL GSCHUP (0.,1.)
C          Draw the text
           CALL GTX (X,Y,'South America')

C
C Draw a border around plot
C
C     Reset the plot window
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C     Set the line color to black
      CALL GSPLCI (1)
C
C  Create a background perimeter
C
      CALL FRSTPT( 0.0, 0.0)
      CALL VECTOR( 1.0, 0.0)
      CALL VECTOR( 1.0, 1.0)
      CALL VECTOR( 0.0, 1.0)
      CALL VECTOR( 0.0, 0.0)

C
C Advance the frame.
C
        CALL FRAME
C
C Done.
C
        RETURN
	END

      SUBROUTINE DFCLRS
C
C Define a set of RGB color triples for colors 1 through 15.
C
        DIMENSION RGBV(3,15)
C
C Define the RGB color triples needed below.
C
        DATA RGBV / 0.00 , 0.00 , 0.00 ,
     +              0.70 , 0.70 , 0.70 ,
     +              0.75 , 0.50 , 1.00 ,
     +              0.50 , 0.00 , 1.00 ,
     +              0.00 , 0.00 , 1.00 ,
     +              0.00 , 0.50 , 1.00 ,
     +              0.00 , 1.00 , 1.00 ,
     +              0.00 , 1.00 , 0.60 ,
     +              0.00 , 1.00 , 0.00 ,
     +              0.70 , 1.00 , 0.00 ,
     +              1.00 , 1.00 , 0.00 ,
     +              1.00 , 0.75 , 0.00 ,
     +              1.00 , 0.38 , 0.38 ,
     +              1.00 , 0.00 , 0.38 ,
     +              1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
        CALL GSCR (1,0,1.,1.,1.)
C
        DO 101 I=1,15
          CALL GSCR (1,I,RGBV(1,I),RGBV(2,I),RGBV(3,I))
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END

