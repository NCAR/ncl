C
C $Id: agchil.f,v 1.2 1996-04-18 17:46:01 kennison Exp $
C
      SUBROUTINE AGCHIL (IFLG,LBNM,LNNO)
C
      CHARACTER*(*) LBNM
C
C The routine AGCHIL is called by AGLBLS just before and just after each
C informational-label line of text is drawn.  The default version does
C nothing.  A user may supply a version to change the appearance of the
C text lines.  The arguments are as follows:
C
C - IFLG is zero if a text line is about to be drawn, non-zero if one
C   has just been drawn.
C
C - LBNM is the name of the label containing the line in question.
C
C - LNNO is the number of the line.
C
C Done.
C
      RETURN
C
      END
