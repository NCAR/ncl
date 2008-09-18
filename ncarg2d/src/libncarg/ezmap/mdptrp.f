C
C $Id: mdptrp.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPTRP (UINS,VINS,UOUT,VOUT,UINT,VINT)
C
        DOUBLE PRECISION UINS,VINS,UOUT,VOUT,UINT,VINT
C
C This routine finds the point of intersection (UINT,VINT) of the line
C from (UINS,VINS) to (UOUT,VOUT) with the edge of a rectangular frame.
C The first point is inside the frame and the second outside the frame.
C
C Because MDPTRP can be called with the same actual arguments for UINT
C and VINT as for UOUT and VOUT, respectively, UINT and VINT must not
C be reset until all use of UOUT and VOUT is complete.
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
C Declare local variables.
C
        INTEGER          IREA
C
        DOUBLE PRECISION DU,DV,U,V
C
C Declare local arithmetic statement functions.
C
        DOUBLE PRECISION UFUN,VFUN
C
C Given one coordinate of a point on the line joining (UINS,VINS) and
C (UOUT,VOUT), the other can be obtained by using one of the following
C arithmetic statement functions:
C
        UFUN(V)=UINS+(V-VINS)*DU/DV
        VFUN(U)=VINS+(U-UINS)*DV/DU
C
C                                      I     I
C                                   5  I  4  I  6
C                                      I     I
C                                 -----------------
C First, determine in which            I     I
C of the areas shown the            2  I  1  I  3
C point (UOUT,VOUT) lies.              I     I
C                                 -----------------
C                                      I     I
C                                   8  I  7  I  9
C                                      I     I
C
        IREA=1
        IF (UOUT-UMIN) 101,104,102
  101   IREA=IREA+1
        GO TO 104
  102   IF (UOUT-UMAX) 104,104,103
  103   IREA=IREA+2
  104   IF (VOUT-VMIN) 105,108,106
  105   IREA=IREA+6
        GO TO 108
  106   IF (VOUT-VMAX) 108,108,107
  107   IREA=IREA+3
C
C Next, compute the quantities required by UFUN and VFUN and jump to the
C appropriate piece of code for the given area.
C
  108   DU=UOUT-UINS
        DV=VOUT-VINS
C
        GO TO (119,113,114,115,109,110,116,111,112) , IREA
C
  109   IF (UFUN(VMAX)-UMIN) 113,115,115
  110   IF (UFUN(VMAX)-UMAX) 115,115,114
  111   IF (UFUN(VMIN)-UMIN) 113,116,116
  112   IF (UFUN(VMIN)-UMAX) 116,116,114
C
  113   UINT=UMIN
        GO TO 117
  114   UINT=UMAX
        GO TO 117
  115   VINT=VMAX
        GO TO 118
  116   VINT=VMIN
        GO TO 118
C
  117   VINT=VFUN(UINT)
        RETURN
C
  118   UINT=UFUN(VINT)
        RETURN
C
  119   UINT=UOUT
        VINT=VOUT
C
C Done.
C
        RETURN
C
      END
