C
C $Id: agezsu.f,v 1.7 2008-07-27 00:14:34 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGEZSU (ITOC,XDRA,YDRA,IDXY,MANY,NPTS,LABG,IIVX,IIEX,
     +                                                      IIVY,IIEY)
C
      REAL XDRA(1),YDRA(1)
      CHARACTER*(*) LABG
C
C The routine AGEZSU is used by the AUTOGRAPH routines EZY, EZXY, EZMY,
C EZMXY, and IDIOT to examine those parameters which are peculiar to the
C old version of AUTOGRAPH and to do the appropriate call to AGSTUP.
C The arguments are as follows:
C
C -- ITOC indicates which routine is calling AGEZSU, as follows:
C
C     -- ITOC .EQ. 1 - call by EZY
C     -- ITOC .EQ. 2 - call by EZXY
C     -- ITOC .EQ. 3 - call by EZMY
C     -- ITOC .EQ. 4 - call by EZMXY
C     -- ITOC .EQ. 5 - call by IDIOT
C
C -- XDRA is an array of x-coordinate data.
C
C -- YDRA is an array of y-coordinate data.
C
C -- IDXY is the first dimension of YDRA.
C
C -- MANY is the number of curves defined by XDRA and YDRA.
C
C -- NPTS is the number of points per curve.
C
C -- LABG is a new header label (or the single character CHAR(0), if the
C    header label is to be unchanged).
C
C -- IIVX, IIEX, IIVY, and IIEY are indexing controls for the x and y
C    data arrays, computed and returned by AGEZSU for use in setting up
C    calls to the routine AGCURV.
C
C Examine the frame-advance parameter.  Do frame advance as appropriate.
C
      CALL AGGETI ('FRAM.',IFRA)
      IFRA=MAX(1,MIN(3,IFRA))
C
      IF (IFRA.EQ.3) CALL FRAME
C
C Set up the header label.
C
      IF (ICHAR(LABG(1:1)).NE.0) THEN
        CALL AGSETC ('LABE/NAME.', 'T')
        CALL AGSETI ('LINE/NUMB.', 100)
        CALL AGSETC ('LINE/TEXT.',LABG)
      END IF
C
C Set up the AGSTUP arguments defining the coordinate-data arrays.
C
      CALL AGGETI ('ROW .',IROW)
      IROW=MAX(-2,MIN(+2,IROW))
C
      NVIY=MANY
      IIVY=IDXY
      NEVY=NPTS
      IIEY=1
C
      IF (IROW.LE.0.AND.ITOC.GE.3.AND.ITOC.LE.4) THEN
        IIVY=1
        IIEY=IDXY
      END IF
C
      NVIX=NVIY
      IIVX=IIVY
      NEVX=NEVY
      IIEX=IIEY
C
      IF (ABS(IROW).LE.1) THEN
        NVIX=1
        IIVX=0
        NEVX=NPTS
        IIEX=1
      END IF
C
      IF (ITOC.EQ.1.OR.ITOC.EQ.3) IIEX=0
C
C Do the AGSTUP call.
C
      CALL AGSTUP (XDRA,NVIX,IIVX,NEVX,IIEX,YDRA,NVIY,IIVY,NEVY,IIEY)
C
C Done.
C
      RETURN
C
      END
