C
C $Id: mpchln.f,v 1.1 1998-04-16 20:45:36 kennison Exp $
C
      SUBROUTINE MPCHLN (ILTY,IOAL,IOAR)
C
C This routine is called by MPLNDM and MPLNDR just before and just after
C processing each line defined by a dataset.  ILTY is the type of the
C line (negated for calls after the line is processed), IOAL is the
C identifier of the area to its left, and IOAR is the identifier of the
C area to its right.  MPCHLN is intended to be replaced by an LLU user
C by a routine that sets line width and color as desired.
C
        RETURN
C
      END
