	PROGRAM CLBLBR

	CHARACTER*7 LAB1(11), LAB2(13)
	INTEGER IFILL1(11), IFILL2(14)

	DATA LAB1 /'.','here','go','can',':G:font','any','in',
     +		'number','or','word','Any'/
	DATA LAB2 /'.','boxes','between','lines','the','or','boxes',
     +		'either','match','can','labels','that','Notice'/
	DATA IFILL1 /11,10,9,8,7,6,5,4,3,2,1/
	DATA IFILL2 /3,4,5,6,7,8,9,10,2,11,12,13,14,15/

C
C Open GKS
C
	CALL OPNGKS
C
C Set up color table
C
	CALL COLOR
C
C Set color fill to be solid
C
	CALL GSFAIS(1)
C
C Draw two vertical label bars.
C
	CALL SFSETR('AN',35.)
	CALL SFSETI('TY',-4)
	CALL LBLBAR(1,.05,.45,.05,.95,11,.3,1.,IFILL1,0,LAB1,11,1)
	CALL SFSETI('TY',0)
	CALL LBLBAR(1,.55,.95,.05,.95,14,.3,1.,IFILL2,1,LAB2,13,2)
C
C Close GKS
C
	CALL CLSGKS

	STOP
	END
      SUBROUTINE COLOR
C
C     BACKGROUND COLOR
C     BLACK
      CALL GSCR(1,0,0.,0.,0.)
C
C     FORGROUND COLORS
C White
      CALL GSCR(1,  1, 1.0, 1.0, 1.0)
C Aqua
      CALL GSCR(1,  2, 0.0, 0.9, 1.0)
C Red
      CALL GSCR(1,  3, 0.9, 0.25, 0.0)
C OrangeRed
      CALL GSCR(1,  4, 1.0, 0.0, 0.2)
C Orange
      CALL GSCR(1,  5, 1.0, 0.65, 0.0)
C Yellow
      CALL GSCR(1,  6, 1.0, 1.0, 0.0)
C GreenYellow
      CALL GSCR(1,  7, 0.7, 1.0, 0.2)
C Chartreuse
      CALL GSCR(1,  8, 0.5, 1.0, 0.0)
C Celeste
      CALL GSCR(1,  9, 0.2, 1.0, 0.5)
C Green
      CALL GSCR(1, 10, 0.2, 0.8, 0.2)
C DeepSkyBlue
      CALL GSCR(1, 11, 0.0, 0.75, 1.0)
C RoyalBlue
      CALL GSCR(1, 12, 0.25, 0.45, 0.95)
C SlateBlue
      CALL GSCR(1, 13, 0.4, 0.35, 0.8)
C DarkViolet
      CALL GSCR(1, 14, 0.6, 0.0, 0.8)
C Orchid
      CALL GSCR(1, 15, 0.85, 0.45, 0.8)
C Lavender
      CALL GSCR(1, 16, 0.8, 0.8, 1.0)
C Gray
      CALL GSCR(1, 17, 0.7, 0.7, 0.7)
C Done.
C
        RETURN
C
      END
