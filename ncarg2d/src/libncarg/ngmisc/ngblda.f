C
C	$Id: ngblda.f,v 1.7 1996-01-12 21:31:07 boote Exp $
C
      BLOCKDATA NGBLDA
C
C  Specify default values for escape functions.
C
      include 'ngcomn.h'
C
C  Workstation IDs initially undefined.
C
      DATA IWKID, IXPS/-1, -1/
C
C  No full background color by default.
C
      DATA IFULLB/0/
C
C  Maximum stack and path sizes.
C
      DATA ISTKMX, IPTHMX/ 200, 1300/
C
C  Line joins and caps are round.
C
      DATA ILJOIN, ILCAP/ 1, 1/ 
C
C  Page coordinates, in default PostScript coordinate system.
C
      DATA ILLX, ILLY, IURX, IURY/ 36, 126, 576, 666/      
C
C  Fill and hatch spacings.
C
      DATA FILSPC, HATSPC/0.0005, .01/
C
C  Miter limit.
C
      DATA RMITER/ 10./
C
C  Nominal linewidth scale.
C
      DATA RNLSCL/ 1./
C
C  Coordinate scale factor.
C
      DATA ICOSCL/25/
C
C  Maximum number of GKS errors allowed before abort.
C
      DATA IERRMX/10/
C
C  Flag to control whether GKS clipping is on (0 = no; 1 = yes).
C
      DATA IGKSCP/1/
C
C  Percentage error allow in matching colors in shared X color maps.
C
      DATA IPERCX/20/
C
C  Color model flags
C
      DATA ISCX,IMCX,IPCX/0,0,0/
C
C  Flag for whether an X window should have a private color map.
C  (0 = no; 1 = yes).
C
      DATA IPRIVX/0/
C
C  File name and picture name.
C
      DATA CFILNM,CPICNM/'DEFAULT',' '/
C
C  Flag to NGDOTS to indicate whether to draw dots (=0) or circles (=1).
C
      DATA ICDFLG/0/
C
      END
