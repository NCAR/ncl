C
C $Id: lbblda.f,v 1.6 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LBBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA LBBLDAX
C
C This block data subroutine defines the default values of internal
C parameters for LBLBAR.
C
C Declare the common block where internal parameters are stored.
C
        COMMON /LBCOMN/ ICBL,ICFL,ICLB,WOBL,WOFL,WOLB
        SAVE   /LBCOMN/
C
C ICBL is the color index to be used for box lines.  A negative value
C implies that the color index is to be that specified by the current
C value of the GKS polyline color index.
C
        DATA ICBL / -1 /
C
C ICFL is the color index to be used for fill lines.  A negative value
C implies that the color index is to be that specified by the current
C value of the GKS polyline color index.
C
        DATA ICFL / -1 /
C
C ICLB is the color index to be used for labels.  A negative value
C implies that the color index is to be that specified by the current
C value of the GKS text color index.
C
        DATA ICLB / -1 /
C
C WOBL is the desired width of the box lines.  A value less than or
C equal to zero implies that line width is to be inherited.
C
        DATA WOBL / 0. /
C
C WOFL is the desired width of the fill lines.  A value less than or
C equal to zero implies that line width is to be inherited.
C
        DATA WOFL / 0. /
C
C WOLB is the desired width of lines to be used when plotting labels
C (which will only affect characters drawn by software).  A value less
C than or equal to zero implies that line width is to be inherited.
C
        DATA WOLB / 0. /
C
      END
