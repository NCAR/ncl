C
C $Id: agchcu.f,v 1.3 2000-07-12 16:21:55 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
