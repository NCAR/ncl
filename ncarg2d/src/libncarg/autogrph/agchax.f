C
C	$Id: agchax.f,v 1.1.1.1 1992-04-17 22:30:56 ncargd Exp $
C
C
C ---------------------------------------------------------------------
C
      SUBROUTINE AGCHAX (IFLG,IAXS,IPRT,VILS)
C
C The routine AGCHAX is called by AGAXIS just before and just after each
C of a selected set of objects on the axes are drawn.  A user may supply
C a version to change the appearance of these objects.  The arguments
C are as follows:
C
C - IFLG is zero if a particular object is about to be drawn, non-zero
C   if it has just been drawn.
C
C - IAXS is the number of the axis in question.  The values 1, 2, 3, and
C   4 imply the right, left, bottom, and top axes, respectively.
C
C - IPRT is an integer implying which part of the axis is being drawn.
C   The value 1 implies the line itself, 2 a major tick, 3 a minor tick,
C   4 the mantissa of a label, and 5 the exponent of a label.
C
C - VILS is the value, in the label coordinate system along the axis,
C   associated with the position of the object being drawn.  IPRT=1
C   implies VILS=0.
C
C Done.
C
      RETURN
C
      END
