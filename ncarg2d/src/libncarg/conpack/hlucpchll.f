C
C $Id: hlucpchll.f,v 1.1 1995-04-26 22:45:22 kennison Exp $
C
      SUBROUTINE HLUCPCHLL (IFLG)
C
C This routine stands between CONPACK and the user call-back routine
C CPCHLL.  When HLUs are not in use, this version of the routine gets
C loaded, so that CPCHLL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CPCHLL.
C
      CALL CPCHLL (IFLG)
C
      RETURN
C
      END
