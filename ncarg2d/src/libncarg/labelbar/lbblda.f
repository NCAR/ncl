C
C	$Id: lbblda.f,v 1.1.1.1 1992-04-17 22:32:58 ncargd Exp $
C
      BLOCK DATA LBBLDA
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
