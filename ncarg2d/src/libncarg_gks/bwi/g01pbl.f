C
C	$Id: g01pbl.f,v 1.3 2000-07-12 16:50:46 haley Exp $
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
      INTEGER FUNCTION G01PBL (NCHARS, NBYTES)
C
C  Return as the value of the function the parameter list length
C  of an element that contains NCHARS characters of string data
C  and NBYTES bytes of other data.
C
C  The length will be either  NCHARS+NBYTES+1 or NCHARS+NBYTES+3,
C  depending upon whether a short form or long form string is
C  called for.
C
      INTEGER  NCHARS, NBYTES, LEN
C
C  For short form.
C
      LEN = 1 + NCHARS + NBYTES
C
C  Short form can handle at most 254 characters in a string.
C
      IF (NCHARS.GT.254)  LEN = LEN + 2
      G01PBL = LEN
C
      RETURN
      END
