C
C $Id: idblda.f,v 1.5 2008-07-27 00:17:30 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE IDBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA IDBLDAX
C
C This "routine" provides default values for the internal parameters of
C BIVAR.
C
C The common block IDCOMN holds BIVAR variables associated with changes
C made in October, 1995, including a couple of internal parameters.
C
        COMMON /IDCOMN/ INTY,ITTY,ALSP,BLSP,CLSP,XAVG,YAVG
        SAVE   /IDCOMN/
C
C INTY is the flag that says what type of interpolation to use (0 for
C quintic interpolation, 1 for linear interpolation).
C
        DATA INTY / 0 /
C
C ITTY is the flag that says what type of triangulation to use (0 for
C the original, 1 for the new).
C
        DATA ITTY / 0 /
C
C ALSP, BLSP, CLSP, XAVG, and YAVG are not actually internal parameters
C of BIVAR.  They are computed and used when linear interpolation is
C selected.  The first three are coefficients in the equation of the
C plane that best fits the data and the final two are mean X and Y
C values computed from the data.
C
        DATA ALSP,BLSP,CLSP,XAVG,YAVG / 5*0. /
C
      END
