C
C $Id: mpchln.f,v 1.2 1998-04-24 22:26:13 kennison Exp $
C
      SUBROUTINE MPCHLN (IFLG,ILTY,IOAL,IOAR)
C
C This routine is called by MPLNDM and MPLNDR just before and just after
C processing each line defined by a dataset.  IFLG is positive if a line
C is about to be drawn, negative if a line was just drawn.  ILTY is the
C type of the line, IOAL is the identifier of the area to its left, and
C IOAR is the identifier of the area to its right.  HLUMPCHLN is meant
C to be replaced by an LLU user by a routine that sets line width and
C color as desired.
C
        RETURN
C
      END
