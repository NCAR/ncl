C
C $Id: slubkg.f,v 1.1 1993-01-15 23:30:40 kennison Exp $
C
      SUBROUTINE SLUBKG (IPOC)
C
C This routine may be replaced by the user with code to add graphics to
C the background over which the titles are being scrolled.  Care should
C be taken when altering the state of GKS or SPPS.
C
C IPOC says what is going on in STITLE at the time that SLUBKG is
C called, as follows:
C
C     IPOC  Position of call to SLUBKG
C     ----  -----------------------------------------------------------
C      -1   Just before drawing characters on a "fade-in" frame.
C      +1   Just before drawing characters on a "fade-in" frame.
C      -2   Just before drawing characters on a "start" frame.
C      +2   Just before drawing characters on a "start" frame.
C      -3   Just before drawing characters on a "move" frame.
C      +3   Just before drawing characters on a "move" frame.
C      -4   Just before drawing characters on an "end" frame.
C      +4   Just before drawing characters on an "end" frame.
C      -5   Just before drawing characters on a "fade-out" frame.
C      +5   Just before drawing characters on a "fade-out" frame.
C
C The default version of the routine does nothing.
C
      RETURN
C
      END
