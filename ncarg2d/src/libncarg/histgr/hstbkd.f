C
C $Id: hstbkd.f,v 1.7 2008-07-27 00:17:15 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HSTBKD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA HSTBKDX
C
      COMMON /HSTGC1/ HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM,
     -       HFRAME, LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL,
     -       FREQNC, HWIND(4), COLSHA, COLREC, COLAXI, COLMED, COLTEX,
     -       COLTIT, COLPER, DRAWL, SPACE, LABMAX, CHARL, HEIGHT,
     -       ORIENT, COLSH2, SETSPA, SETSP2, MVALU, SETMVA, SETEPS,
     -       NMVAL, PMVAL, PERTIT
      LOGICAL HORZNT, PERCNT, MIDVAL, SHADE, MEDIAN, PERIM, HFRAME,
     -        LISTOP, WINDOW, COLORS, HSTFOR, TITLE, LABEL, FREQNC,
     -        DRAWL, SPACE, CHARL, MVALU, NMVAL, PMVAL, PERTIT
      COMMON /HSTGC2/ STRFOR, STRTIT, STRLAB, STRFRE, STRPER, LABTEX
      CHARACTER*96  STRTIT
      CHARACTER*55  STRFOR, STRLAB, STRFRE, STRPER
      CHARACTER*15 LABTEX(30)
      INTEGER  COLSHA, COLREC, COLAXI, COLMED, COLTEX
      INTEGER  COLTIT, COLPER, COLSH2, HEIGHT, ORIENT
C
      DATA HORZNT /.FALSE./
      DATA PERCNT /.TRUE./
      DATA MIDVAL /.TRUE./
      DATA SPACE  /.TRUE./
      DATA SHADE  /.TRUE./
      DATA DRAWL  /.FALSE./
      DATA MEDIAN /.FALSE./
      DATA PERIM  /.FALSE./
      DATA HFRAME /.TRUE./
      DATA LISTOP /.FALSE./
      DATA WINDOW /.FALSE./
      DATA COLORS /.FALSE./
      DATA HSTFOR /.FALSE./
      DATA TITLE  /.FALSE./
      DATA LABEL  /.FALSE./
      DATA FREQNC /.FALSE./
      DATA PERTIT /.FALSE./
      DATA CHARL  /.FALSE./
      DATA MVALU  /.FALSE./
      DATA NMVAL  /.TRUE./
      DATA PMVAL  /.TRUE./
      DATA HWIND  /0.,1.,0.,1./
      DATA COLSHA /1/
      DATA COLSH2 /1/
      DATA COLREC /1/
      DATA COLAXI /1/
      DATA COLMED /1/
      DATA COLTEX /1/
      DATA COLTIT /1/
      DATA COLPER /1/
      DATA HEIGHT /2/
      DATA ORIENT /0/
      DATA SETSPA /2./
      DATA SETSP2 /0./
      DATA SETMVA /-9999./
      DATA SETEPS /1.E-10/
      DATA STRFOR / '(G10.3)' /
      END
