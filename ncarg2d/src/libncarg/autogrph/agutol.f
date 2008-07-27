C
C $Id: agutol.f,v 1.6 2008-07-27 00:14:36 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
