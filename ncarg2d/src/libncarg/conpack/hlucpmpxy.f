C
C $Id: hlucpmpxy.f,v 1.1 1995-04-26 22:45:23 kennison Exp $
C
      SUBROUTINE HLUCPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
C
C This routine stands between CONPACK and the user call-back routine
C CPMPXY.  When HLUs are not in use, this version of the routine gets
C loaded, so that CPMPXY is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CPMPXY.
C
      CALL CPMPXY (IMAP,XINP,YINP,XOTP,YOTP)
C
      RETURN
C
      END
