C
C $Id: hlucpscae.f,v 1.1 1995-04-26 22:45:24 kennison Exp $
C
      SUBROUTINE HLUCPSCAE (ICRA,ICA1,ICAM,ICAN,XCPF,YCPF,XCQF,YCQF,
     +                                          IND1,IND2,ICAF,IAID)
C
      DIMENSION ICRA(ICA1,*)
C
C This routine stands between CONPACK and the user call-back routine
C CPSCAE.  When HLUs are not in use, this version of the routine gets
C loaded, so that CPSCAE is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CPSCAE.
C
      CALL CPSCAE (ICRA,ICA1,ICAM,ICAN,XCPF,YCPF,XCQF,YCQF,
     +                                 IND1,IND2,ICAF,IAID)
C
      RETURN
C
      END
