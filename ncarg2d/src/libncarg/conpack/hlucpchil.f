C
C $Id: hlucpchil.f,v 1.1 1995-04-26 22:45:22 kennison Exp $
C
      SUBROUTINE HLUCPCHIL (IFLG)
C
C This routine stands between CONPACK and the user call-back routine
C CPCHIL.  When HLUs are not in use, this version of the routine gets
C loaded, so that CPCHIL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CPCHIL.
C
      CALL CPCHIL (IFLG)
C
      RETURN
C
      END
