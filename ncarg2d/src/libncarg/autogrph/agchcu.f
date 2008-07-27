C
C $Id: agchcu.f,v 1.6 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGCHCU (IFLG,KDSH)
C
C The routine AGCHCU is called by AGCURV just before and just after each
C curve is drawn.  The default version does nothing.  A user may supply
C a version to change the appearance of the curves.  The arguments are
C as follows:
C
C - IFLG is zero if a curve is about to be drawn, non-zero if a curve
C   has just been drawn.
C
C - KDSH is the last argument of AGCURV, as follows:
C
C      AGCURV called by   Value of KDSH
C      ----------------   ----------------------------------------
C      EZY                1
C      EZXY               1
C      EZMY               "n" or "-n", where n is the curve number
C      EZMXY              "n" or "-n", where n is the curve number
C      the user program   the user value
C
C   The sign of KDSH, when AGCURV is called by EZMY or EZMXY, indicates
C   whether the "user" dash patterns or the "alphabetic" dash patterns
C   were selected for use.
C
C Done.
C
      RETURN
C
      END
