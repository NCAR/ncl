C
C	$Id: cmpcc.f,v 1.1 1992-09-29 16:09:46 ncargd Exp $
C
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

	SUBROUTINE CFILL (XC, YC, NPTS, AREAID, GRPID, IDSIZE)
C
C Fill area map
C
	INTEGER AREAID(IDSIZE), GRPID(IDSIZE), IDSIZE, NPTS, ICOL
	REAL XC(NPTS), YC(NPTS)

	ICOL = 0
	DO 10, I=1,IDSIZE
	   IF (GRPID(I).EQ.1) IAREA=AREAID(I)
 10	CONTINUE
	   
	IF (IAREA .GE. 1) THEN
	   ICOL = MAPACI(IAREA)
	   IF (ICOL .eq. 1) THEN
C Color the ocean blue.
	      CALL GSFACI(2)
	      CALL GFA(NPTS-1, XC, YC)
	   ELSE 
C If the area is over land, fill it using the country color id.
	      CALL GSFACI(ICOL+2)
	      CALL GFA(NPTS-1, XC, YC)
	   ENDIF
	ENDIF
	
	RETURN
	END

	SUBROUTINE MASK(XC,YC,MCS,AREAID,GRPID,IDSIZE)

	INTEGER AREAID(IDSIZE),GRPID(IDSIZE),ID
	REAL XC(MCS),YC(MCS)

C Retrieve area id for geographical area
	DO 10, I=1,IDSIZE
	   IF (GRPID(I).EQ.1) ID=AREAID(I)
 10	CONTINUE
C If the line is over water, and has 2 or more points draw it.
	IF ((MAPACI(ID).EQ.1).AND.(MCS.GE.2)) THEN
	   CALL CURVED(XC,YC,MCS)
	ENDIF
	
C Otherwise, don't draw the line - mask it.

	RETURN
	END

	SUBROUTINE SHADE1 (XC, YC, NPTS, AREAID, GRPID, IDSIZE)
C
C Fill area map
C
	INTEGER AREAID(IDSIZE),GRPID(IDSIZE),IDSIZE,NPTS,IWRK(5000)
	REAL XC(NPTS), YC(NPTS), RWRK(5000)

	IAID = 0
	DO 10 I=1,IDSIZE
	   IF (GRPID(I) .EQ. 1) IAID = AREAID(I)
 10	CONTINUE
C
C Fill Areas over land using softfill
C
C Areas over water have area color indices of 1 so we use that to 
C distinguish them.
C
	IF ((IAID .GT. 0) .AND. (MAPACI(IAID) .EQ. 1)) THEN
	   CALL SFSETR('SP',.005)
	   CALL SFSETR('ANGLE', 45.)
	   CALL SFWRLD(XC, YC, NPTS-1, RWRK, 5000, IWRK, 5000)
	ENDIF

	RETURN
	END

	SUBROUTINE SHADE2 (XC, YC, NPTS, AREAID, GRPID, IDSIZE)
C
C Fill area map
C
	INTEGER AREAID(IDSIZE),GRPID(IDSIZE),IDSIZE,NPTS,IWRK(10000)
	REAL XC(NPTS), YC(NPTS), RWRK(10000)

	CALL GSFAIS (1)
	IAID = 0
	DO 10 I=1,IDSIZE
	   IF (GRPID(I) .EQ. 1) IAID = AREAID(I)
 10	CONTINUE
C
C Fill Areas over land using softfill
C
C Areas over water have area color indices of 1 so we use that to 
C distinguish them.
C
	IF (IAID .GT. 0) THEN
	   IF (MAPACI(IAID) .EQ. 1) THEN
	      CALL SFSETR('SP',.005)
	      CALL SFSETR('ANGLE', 45.)
	      CALL SFWRLD(XC, YC, NPTS-1, RWRK, 10000, IWRK, 10000)
	   ELSE 
	      CALL GSFACI (MAPACI(IAID))
	      CALL GFA (NPTS, XC, YC)
	   ENDIF
	ENDIF

	RETURN
	END
