
      PROGRAM AGEX07
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
C
C Define the data arrays and the dash-pattern array.
C
      REAL XDRA(101),YDRA(101,9)
      CHARACTER*28 DSHP(9)
C
C Declare the type of the dash-pattern-name generator.
C
      CHARACTER*16 AGDSHN
C
C Initialize GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data arrays and the dash pattern array.
C
      DO 101 I=1,101
        XDRA(I)=-90.+1.8*REAL(I-1)
  101 CONTINUE
C
      DO 103 J=1,9
        WRITE (DSHP(J),1001) J
        FJ=J
        DO 102 I=1,101
          YDRA(I,J)=3.*FJ-(FJ/2700.)*XDRA(I)**2
  102   CONTINUE
  103 CONTINUE
C
C Turn on windowing.
C
      CALL AGSETI ('WINDOWING.',1)
C
C Move the edges of the curve window (grid).
C
      CALL AGSETF ('GRID/LEFT.'  ,.10)
      CALL AGSETF ('GRID/RIGHT.' ,.90)
      CALL AGSETF ('GRID/BOTTOM.',.10)
      CALL AGSETF ('GRID/TOP.'   ,.85)
C
C Set the x and y minimum and maximum.
C
      CALL AGSETF ('X/MINIMUM.',-90.)
      CALL AGSETF ('X/MAXIMUM.',+90.)
      CALL AGSETF ('Y/MINIMUM.',  0.)
      CALL AGSETF ('Y/MAXIMUM.', 18.)
C
C Set left axis parameters.
C
      CALL AGSETI ('LEFT/MAJOR/TYPE.',1)
      CALL AGSETF ('LEFT/MAJOR/BASE.',3.)
      CALL AGSETI ('LEFT/MINOR/SPACING.',2)
C
C Set right axis parameters.
C
      CALL AGSETI ('RIGHT/FUNCTION.',1)
      CALL AGSETF ('RIGHT/NUMERIC/TYPE.',1.E36)
C
C Set bottom axis parameters.
C
      CALL AGSETI ('BOTTOM/MAJOR/TYPE.',1)
      CALL AGSETF ('BOTTOM/MAJOR/BASE.',15.)
      CALL AGSETI ('BOTTOM/MINOR/SPACING.',2)
C
C Set top axis parameters.
C
      CALL AGSETI ('TOP/FUNCTION.',2)
      CALL AGSETF ('TOP/NUMERIC/TYPE.',1.E36)
C
C Set up the dash patterns to be used.
C
      CALL AGSETI ('DASH/SELECTOR.',9)
      CALL AGSETI ('DASH/LENGTH.',28)
      DO 104 I=1,9
        CALL AGSETC (AGDSHN(I),DSHP(I))
  104 CONTINUE
C
C Set up the left label.
C
      CALL AGSETC ('LABEL/NAME.','L')
      CALL AGSETI ('LINE/NUMBER.',100)
      CALL AGSETC ('LINE/TEXT.','HEIGHT (KILOMETERS)$')
C
C Set up the right label.
C
      CALL AGSETC ('LABEL/NAME.','R')
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.',
     +             'PRESSURE (TONS/SQUARE FURLONG)$')
C
C Set up the bottom labels.
C
      CALL AGSETC ('LABEL/NAME.','B')
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','LATITUDE (DEGREES)$')
C
      CALL AGSETC ('LABEL/NAME.','SP')
      CALL AGSETF ('LABEL/BASEPOINT/X.',.000001)
      CALL AGSETF ('LABEL/BASEPOINT/Y.',0.)
      CALL AGSETF ('LABEL/OFFSET/Y.',-.015)
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','SP$')
C
      CALL AGSETC ('LABEL/NAME.','NP')
      CALL AGSETF ('LABEL/BASEPOINT/X.',.999999)
      CALL AGSETF ('LABEL/BASEPOINT/Y.',0.)
      CALL AGSETF ('LABEL/OFFSET/Y.',-.015)
      CALL AGSETI ('LINE/NUMBER.',-100)
      CALL AGSETC ('LINE/TEXT.','NP$')
C
C Set up the top label.
C
      CALL AGSETC ('LABEL/NAME.','T')
      CALL AGSETI ('LINE/NUMBER.',80)
      CALL AGSETC ('LINE/TEXT.',
     +             'DISTANCE FROM EQUATOR (MILES)$')
      CALL AGSETI ('LINE/NUMBER.',90)
      CALL AGSETC ('LINE/TEXT.',' $')
      CALL AGSETI ('LINE/NUMBER.',100)
      CALL AGSETC ('LINE/TEXT.',
     +             'LINES OF CONSTANT INCRUDESCENCE$')
      CALL AGSETI ('LINE/NUMBER.',110)
      CALL AGSETC ('LINE/TEXT.','EXAMPLE 7 (EZMXY)$')
C
C Set up centered (box 6) label.
C
      CALL AGSETC ('LABEL/NAME.','EQUATOR')
      CALL AGSETI ('LABEL/ANGLE.',90)
      CALL AGSETI ('LINE/NUMBER.',0)
      CALL AGSETC ('LINE/TEXT.','EQUATOR$')
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Draw the graph, using EZMXY.
C
      CALL EZMXY (XDRA,YDRA,101,9,101,CHAR(0))
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
C
C Format for encode above.
C
 1001 FORMAT ('$$$$$$$$$$$$$$$$$$$$$''J''=''',I1,'''')
C
      END
      SUBROUTINE AGUTOL (IAXS,FUNS,IDMA,VINP,VOTP)
C
C Mapping for the right axis.
C
      IF (FUNS.EQ.1.) THEN
        IF (IDMA.GT.0) VOTP=ALOG10(20.-VINP)
        IF (IDMA.LT.0) VOTP=20.-10.**VINP
C
C Mapping for the top axis.
C
      ELSE IF (FUNS.EQ.2.) THEN
        IF (IDMA.GT.0) VOTP=70.136*VINP
        IF (IDMA.LT.0) VOTP=VINP/70.136
C
C Default (identity) mapping.
C
      ELSE
        VOTP=VINP
      END IF
C
C Done.
C
      RETURN
C
      END
      SUBROUTINE BNDARY
C
C Routine to draw the plotter-frame edge.
C
      CALL PLOTIT (    0,    0,0)
      CALL PLOTIT (32767,    0,1)
      CALL PLOTIT (32767,32767,1)
      CALL PLOTIT (    0,32767,1)
      CALL PLOTIT (    0,    0,1)
      RETURN
      END
