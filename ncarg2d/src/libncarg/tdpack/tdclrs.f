C
C $Id: tdclrs.f,v 1.6 2008-07-27 00:17:31 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDCLRS (IWID,IBOW,SHDE,SHDR,IOFC,IOLC,ILMT)
C
C TDCLRS is called to do the GSCR calls defining the following colors
C on a specified workstation:
C
C   Color 0 - background color (black or white, depending on IBOW)
C   Color 1 - foreground color (white or black, depending on IBOW)
C   Color 2 - pure red
C   Color 3 - pure green
C   Color 4 - pure blue
C   Color 5 - pure cyan
C   Color 6 - pure magenta
C   Color 7 - pure yellow
C   Colors IOFC through IOLC - gray shades, from white to black
C   Colors IOFC+NSHD through IOLC+NSHD - shades of gray
C   Colors IOFC+2*NSHD through IOLC+2*NSHD - shades of red
C   Colors IOFC+3*NSHD through IOLC+3*NSHD - shades of green
C   Colors IOFC+4*NSHD through IOLC+4*NSHD - shades of blue
C   Colors IOFC+5*NSHD through IOLC+5*NSHD - shades of cyan
C   Colors IOFC+6*NSHD through IOLC+6*NSHD - shades of magenta
C   Colors IOFC+7*NSHD through IOLC+7*NSHD - shades of yellow
C
C (NSHD is equal to IOLC-IOFC+1, the number of elements in each block
C of color shades.)
C
C The arguments are as follows:
C
C   IWID is the workstation identifier.
C
C   If IBOW is 0, the foreground color is white and the background color
C   is black; if IBOW is non-zero, the opposite is true.
C
C   SHDE is a real value between 0 and 1, inclusive; values near 0 call
C   for more intense shades to be used, while values near 1 call for
C   more nearly pastel shades to be used.
C
C   SHDR is a real value between 0 and 1, inclusive; values near 0 say
C   that a narrower range of shades is to be used, while values near 1
C   say that a broader range of shades is to be used.
C
C   IOFC and IOLC are the first and last integers in a block of color
C   indices to be used for NSHD shades of gray ranging from pure white
C   to pure black (where NSHD=IOLC-IOFC+1).  The next NSHD indices (in
C   numerical order) will be used for the shades of gray selected by
C   SHDR and SHDE; the next NSHD indices after that for selected shades
C   of red, the next NSHD indices after that for selected shades of
C   green, and so on.
C
C   ILMT, if set to a value between 1 and 7, inclusive, says that only
C   that many blocks of NSHD indices will be defined.  For example, if
C   ILMT has the value 4, only the black-to-white scale and the shades
C   of gray, red, and green will be generated; shades of blue, cyan,
C   magenta, and yellow will not be.  (This allows one to have more
C   shades of each color at the expense of using fewer colors.)  Using
C   a value of ILMT less than 1 or greater than 7 will result in all
C   8*NSHD color shades being defined.
C
C The colors defined by calling TDCLRS may be used for any purpose, but
C they are particularly useful when calling TDPACK routines to render
C surfaces.
C
C Define the foreground and background colors (white on black or black
C on white).
C
        IF (IBOW.EQ.0) THEN
          CALL GSCR (IWID,0,0.,0.,0.)
          CALL GSCR (IWID,1,1.,1.,1.)
        ELSE
          CALL GSCR (IWID,0,1.,1.,1.)
          CALL GSCR (IWID,1,0.,0.,0.)
        END IF
C
C Define the pure colors: 2 = red; 3 = green; 4 = blue; 5 = cyan;
C 6 = magenta; and 7 = yellow.
C
        CALL GSCR (IWID,2,1.,0.,0.)
        CALL GSCR (IWID,3,0.,1.,0.)
        CALL GSCR (IWID,4,0.,0.,1.)
        CALL GSCR (IWID,5,0.,1.,1.)
        CALL GSCR (IWID,6,1.,0.,1.)
        CALL GSCR (IWID,7,1.,1.,0.)
C
C Define the specified color shades.
C
        NSHD=IOLC-IOFC+1
C
        DO 101 IOCC=IOFC,IOLC
          P=MAX(0.,MIN(1.,1.-     REAL(IOCC-IOFC)/REAL(IOLC-IOFC)))
          Q=MAX(0.,MIN(1.,1.-SHDR*REAL(IOCC-IOFC)/REAL(IOLC-IOFC)))
C         full range:
          CALL GSCR (IWID,IOCC       ,     P,     P,     P)
          IF (ILMT.EQ.1) GO TO 101
C         grays:
          CALL GSCR (IWID,IOCC+  NSHD,     Q,     Q,     Q)
          IF (ILMT.EQ.2) GO TO 101
C         reds:
          CALL GSCR (IWID,IOCC+2*NSHD,     Q,SHDE*Q,SHDE*Q)
          IF (ILMT.EQ.3) GO TO 101
C         greens:
          CALL GSCR (IWID,IOCC+3*NSHD,SHDE*Q,     Q,SHDE*Q)
          IF (ILMT.EQ.4) GO TO 101
C         blues:
          CALL GSCR (IWID,IOCC+4*NSHD,SHDE*Q,SHDE*Q,     Q)
          IF (ILMT.EQ.5) GO TO 101
C         cyans:
          CALL GSCR (IWID,IOCC+5*NSHD,SHDE*Q,     Q,     Q)
          IF (ILMT.EQ.6) GO TO 101
C         magentas:
          CALL GSCR (IWID,IOCC+6*NSHD,     Q,SHDE*Q,     Q)
          IF (ILMT.EQ.7) GO TO 101
C         yellows:
          CALL GSCR (IWID,IOCC+7*NSHD,     Q,     Q,SHDE*Q)
  101   CONTINUE
C
C Done.
C
        RETURN
C
      END
