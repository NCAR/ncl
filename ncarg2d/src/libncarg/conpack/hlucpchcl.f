C
C $Id: hlucpchcl.f,v 1.1 1995-04-26 22:45:20 kennison Exp $
C
      SUBROUTINE HLUCPCHCL (IFLG)
C
C This routine stands between CONPACK and the user call-back routine
C CPCHCL.  When HLUs are not in use, this version of the routine gets
C loaded, so that CPCHCL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CPCHCL.
C
      CALL CPCHCL (IFLG)
C
      RETURN
C
      END
