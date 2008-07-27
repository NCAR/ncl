C
C $Id: agchnl.f,v 1.6 2008-07-27 00:14:33 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE AGCHNL (IAXS,VILS,CHRM,MCIM,NCIM,IPXM,CHRE,MCIE,NCIE)
C
      CHARACTER*(*) CHRM,CHRE
C
C The routine AGCHNL is called by AGAXIS just after it has set up the
C character strings comprising a numeric label along an axis.  The
C default version does nothing.  A user may supply his own version to
C change the numeric labels.  For each numeric label, this routine is
C called twice by AGAXIS - once to determine how much space will be
C required when the label is actually drawn and once just before it
C is actually drawn.  The arguments are as follows:
C
C - IAXS is the number of the axis being drawn.  Its value is 1, 2, 3,
C   or 4, implying the left, right, bottom, or top axes, respectively.
C   The value of IAXS must not be altered.
C
C - VILS is the value to be represented by the numeric label, in the
C   label system for the axis.  The value of VILS must not be altered.
C
C - CHRM, on entry, is a character string containing the mantissa of the
C   numeric label, as it will appear if AGCHNL makes no changes.  If the
C   numeric label includes a "times" symbol, it will be represented by
C   a blank in CHRM.  (See IPXM, below.)  CHRM may be modified.
C
C - MCIM is the length of CHRM - the maximum number of characters that
C   it will hold.  The value of MCIM must not be altered.
C
C - NCIM, on entry, is the number of meaningful characters in CHRM.  If
C   CHRM is changed, NCIM should be changed accordingly.
C
C - IPXM, on entry, is zero if there is no "times" symbol in CHRM; if it
C   is non-zero, it is the index of the appropriate character position
C   in CHRM.  If AGCHNL changes the position of the "times" symbol in
C   CHRM, removes it, or adds it, the value of IPXM must be changed.
C
C - CHRE, on entry, is a character string containing the exponent of the
C   numeric label, as it will appear if AGCHNL makes no changes.  CHRE
C   may be modified.
C
C - MCIE is the length of CHRE - the maximum number of characters that
C   it will hold.  The value of MCIE must not be altered.
C
C - NCIE, on entry, is the number of meaningful characters in CHRE.  If
C   CHRE is changed, NCIE should be changed accordingly.
C
C Done.
C
      RETURN
C
      END
