C
C	$Id: ngblda.f,v 1.19 2010-02-08 05:58:44 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGBLDA
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA NGBLDAX
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
C  Supression flag for PS.
C
      DATA ISUP/0/
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
C  Page coordinates, in default PostScript coordinate system,
C  used for escape -1526.
C
      DATA NLLX, NLLY, NURX, NURY/ 36, 126, 576, 666/      
C
C  Bounding box for EPS/EPSI output.
C
      DATA MLLX, MLLY, MURX, MURY/ 36, 126, 576, 666/      
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
C  PostScript color model (1=RGB, 0=CMYK)
C
      DATA IPSCM/1/
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
C  Segment save flag. (1 = delete segments; 0 = save segments)
C
      DATA ISVSEG/1/
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
C  Logo type (1=full NCAR logo, 2=UCAR logo star, 
C             3="NCAR" in Bell Gothic Black font, 
C             4="UCAR" in Bell Gothic Black font, 
C             5=UCAR star logo, plus "UCAR" in Bell Gothic Black font) 
C
      DATA LOGOTYP/1/
C
C  Color indices for logos (primary and secondary colors).
C
      DATA LOGOCOL,LOGOSND/1,1/ 
C
C  Default X,Y NDC coordinate position for logos, and the default  size.
C
      DATA OXLOGO,OYLOGO,OLSIZE/0.93,0.05,0.07/
C
C  Defaults for the PDF bounding media box in 1/72" units.
C
      DATA IPGHGT,IPGWTH/792,612/
C
C  Defaults for the PS paper size in for the setpagedevice, in 1/72" units.
C
      DATA IPSHGT,IPSWTH/792,612/
C
      END
