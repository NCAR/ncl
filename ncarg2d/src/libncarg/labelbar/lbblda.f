C
C $Id: lbblda.f,v 1.3 2000-07-12 16:24:38 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
