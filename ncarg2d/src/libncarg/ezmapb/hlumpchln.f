C
C $Id: hlumpchln.f,v 1.1 1998-04-16 20:45:35 kennison Exp $
C
      SUBROUTINE HLUMPCHLN (ILTY,IOAL,IOAR)
C
C This routine is called by MPLNDM and MPLNDR just before and just after
C processing each line defined by a dataset.  ILTY is the type of the
C line (negated for calls after the line is processed), IOAL is the
C identifier of the area to its left, and IOAR is the identifier of the
C area to its right.  HLUMPCHLN is intended to be replaced by the HLU
C developers by a routine that sets line width and color as desired.
C
C Call MPCHLN, which is replaceable by an LLU user.
C
        CALL MPCHLN (ILTY,IOAL,IOAR)
C
        RETURN
C
      END
