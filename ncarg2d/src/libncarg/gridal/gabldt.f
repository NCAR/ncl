C
C	$Id: gabldt.f,v 1.1.1.1 1992-04-17 22:31:20 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C C O D E   -   B L O C K   D A T A   G A B L D T
C-----------------------------------------------------------------------
C
      BLOCK DATA GABLDT
C
C This "routine" declares default values for all of the GRIDAL
C parameters.
C
C Declare the common block containing real or integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ICWX,ICWY,IDCX,IDCY,IORX,
     +                  IMJX,IMJY,IMNX,IMNY,NCFX,NCFY
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
C ICAX, ICLB, ICMJ, and ICMN are color indices for axes, labels, major
C grid line/ticks, and minor grid lines/ticks, respectively.  Values
C which are less than or equal to zero imply that no color is to be
C forced for the associated item.
C
        DATA ICAX,ICLB,ICMJ,ICMN / -1,-1,-1,-1 /
C
C ICWX is the character width for characters in labels on the X axis.
C
        DATA ICWX / 10 /
C
C ICWY is the character width for characters in labels on the Y axis.
C
        DATA ICWY / 10 /
C
C IDCX is the decrement, in the X direction, from the left edge of
C the grid (or from the vertical axis) to the nearest edge of the
C Y-axis labels.
C
        DATA IDCX / 20 /
C
C IDCY is the decrement, in the Y direction, from the bottom edge of
C the grid (or from the horizontal axis) to the nearest edge of the
C X-axis labels.
C
        DATA IDCY / 20 /
C
C IORX is the orientation of the labels on the X axis.  The value 0
C means "horizontal", any other value "vertical".
C
        DATA IORX / 0 /
C
C IMJX is the length of major ticks on the X axis.
C
        DATA IMJX / 12 /
C
C IMJY is the length of major ticks on the Y axis.
C
        DATA IMJY / 12 /
C
C IMNX is the length of minor ticks on the X axis.
C
        DATA IMNX / 8 /
C
C IMNY is the length of minor ticks on the Y axis.
C
        DATA IMNY / 8 /
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
C REVISION HISTORY -----------------------------------------------------
C
C October, 1986    All new code (DJK).
C
C November, 1988   Changed color-setting calls to use GKS routines,
C                  rather than SPPS routines.  Inserted calls to
C                  ensure dumping of the pen-move buffers in SPPS.
C
C October, 1990    Changed the test for an "I" format to look for "i"
C                  as well as "I".
C
C-----------------------------------------------------------------------
      END
