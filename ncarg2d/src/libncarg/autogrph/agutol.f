C
C $Id: agutol.f,v 1.3 2000-07-12 16:22:02 haley Exp $
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
      SUBROUTINE AGUTOL (IAXS,FUNS,IDMA,VINP,VOTP)
C
C This routine is called to perform the mapping from the "user system"
C along an axis to the "label system" along that axis or vice-versa.  It
C may be replaced by the user in order to create a desired graph.  The
C arguments are as follows:
C
C -- IAXS is the index of the axis being drawn.  Its value is 1, 2, 3,
C    or 4, implying the left, right, bottom, or top axis, respectively.
C
C -- FUNS is the value of the parameter 'AXIS/s/FUNCTION.', which may be
C    used to select the desired mapping function for axis IAXS.  It is
C    recommended that the default value (zero) be used to specify the
C    identity mapping.  A non-zero value may be integral (1., 2., etc.)
C    and serve purely to select the code to be executed or it may be the
C    value of a real parameter in the equations defining the mapping.
C
C -- IDMA specifies the direction of the mapping.  A value greater than
C    zero indicates that VINP is a value in the user system and that
C    VOTP is to be a value in the label system, a value less than zero
C    the opposite.
C
C -- VINP is an input value in one coordinate system along the axis.
C
C -- VOTP is an output value in the other coordinate system along the
C    axis.
C
C The default routine simply defines the identity mapping for all axes.
C
      VOTP=VINP
C
      RETURN
C
      END
