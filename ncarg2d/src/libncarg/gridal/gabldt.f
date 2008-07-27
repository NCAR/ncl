C
C $Id: gabldt.f,v 1.9 2008-07-27 00:17:13 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GABLDT
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA GABLDTX
C
C This "routine" declares default values for all of the GRIDAL
C parameters.
C
C Declare the common block containing real and integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ILTY,IORX,NCFX,NCFY,RCWX,
     +                  RCWY,RDCX,RDCY,RMJX,RMJY,RMNX,RMNY,RWAX,RWLB,
     +                  RWMJ,RWMN
        SAVE   /GAREIN/
C
C Declare the common block containing character parameters.
C
        COMMON /GACHAR/ FNLX,FNLY
        CHARACTER*10    FNLX,FNLY
        SAVE   /GACHAR/
C
C Declare default values:
C
C FNLX is the format to be used to generate numeric labels for the X
C axis.
C
        DATA FNLX / '(E10.3)' /
C
C FNLY is the format to be used to generate numeric labels for the Y
C axis.
C
        DATA FNLY / '(E10.3)' /
C
C ICAX, ICLB, ICMJ, and ICMN are color indices for axes, labels, major
C grid line/ticks, and minor grid lines/ticks, respectively.  Values
C which are less than zero imply that no color is to be forced for the
C associated item.
C
        DATA ICAX,ICLB,ICMJ,ICMN / -1,-1,-1,-1 /
C
C ILTY says what type of labels are to be written.  The value 0 says
C that WTSTR is to be called, the value 1 that PLCHHQ is to be called,
C and the value 2 that GAPLCH is to be called (which preprocesses the
C numeric string to force the use of superscript exponents and then
C calls PLCHHQ).
C
        DATA ILTY / 0 /
C
C IORX is the orientation of the labels on the X axis.  The value 0
C means "horizontal", any other value "vertical".
C
        DATA IORX / 0 /
C
C NCFX is the length (number of characters) to be assumed in positioning
C X-axis labels.  The value 0 indicates that the non-blank portion of
C each label should be located and positioned independently of the rest
C of the labels.
C
        DATA NCFX / 0 /
C
C NCFY is the length (number of characters) to be assumed in positioning
C Y-axis labels.  The value 0 indicates that the non-blank portion of
C each label should be located and positioned independently of the rest
C of the labels.
C
        DATA NCFY / 0 /
C
C RCWX is the character width for characters in labels on the X axis.
C
        DATA RCWX / 10. /
C
C RCWY is the character width for characters in labels on the Y axis.
C
        DATA RCWY / 10. /
C
C RDCX is the decrement, in the X direction, from the left edge of
C the grid (or from the vertical axis) to the nearest edge of the
C Y-axis labels.
C
        DATA RDCX / 20. /
C
C RDCY is the decrement, in the Y direction, from the bottom edge of
C the grid (or from the horizontal axis) to the nearest edge of the
C X-axis labels.
C
        DATA RDCY / 20. /
C
C RMJX is the length of major ticks on the X axis.
C
        DATA RMJX / 12. /
C
C RMJY is the length of major ticks on the Y axis.
C
        DATA RMJY / 12. /
C
C RMNX is the length of minor ticks on the X axis.
C
        DATA RMNX / 8. /
C
C RMNY is the length of minor ticks on the Y axis.
C
        DATA RMNY / 8. /
C
C RWAX, RWLB, RWMJ, and RWMN are line widths for axes, labels, major
C grid line/ticks, and minor grid lines/ticks, respectively.  Values
C which are less than or equal to zero imply that no line width is to
C be forced for the associated item.
C
        DATA RWAX,RWLB,RWMJ,RWMN / 0.,0.,0.,0. /
C
      END
