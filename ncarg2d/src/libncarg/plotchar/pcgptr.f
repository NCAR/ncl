C
C $Id: pcgptr.f,v 1.7 1995-05-01 22:21:10 kennison Exp $
C
      SUBROUTINE PCGPTR (KCHR,LCHR,INDX,NLCH,IPNT)
C
C This routine finds the character KCHR in the array LCHR, which has
C been sorted in ascending order, and returns the value of the index,
C from the parallel array INDX, in IPNT.
C
      CHARACTER*1 LCHR(NLCH),KCHR
      DIMENSION   INDX(NLCH)
C
C A binary-halving technique is used.  It is assumed that LCHR is short
C enough so that 10 steps will suffice; if KCHR is not found in that
C many steps, the search is stopped and IPNT, which is used to count
C down the steps, ends up zeroed to indicate what happened.
C
      IBOT=1
      ITOP=NLCH
      IPNT=10
      ITRY=ITOP
      GO TO 102
  101 IF (IPNT.EQ.0) GO TO 106
      IPNT=IPNT-1
      ITRY=(IBOT+ITOP)/2
  102 IF (ICHAR(LCHR(ITRY))-ICHAR(KCHR)) 103,105,104
  103 IBOT=ITRY
      GO TO 101
  104 ITOP=ITRY
      GO TO 101
  105 IPNT=INDX(ITRY)
C
C Done.
C
  106 RETURN
C
      END
