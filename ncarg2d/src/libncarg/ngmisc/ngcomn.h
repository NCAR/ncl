C
C $Id: ngcomn.h,v 1.22 2010-02-08 05:58:44 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C
C  NGCOMI contains the values for all settable parameters:
C
C    CFILNM  -  Metafile name.
C    CPICNM  -  Picture name for CGM pictures.
C     IWKID  -  Currently applicable workstation ID.
C    IFULLB  -  Flag for full background in PostScript output.
C      IXPS  -  Flag for pause in the X11 driver.
C    ISTXMX  -  PostScript stack size limit.
C    IPTHMX  -  PostScript path size limit.
C    ILJOIN  -  PostScript line join type.
C     ILCAP  -  PostScript line cap type.
C      ILLX  -  Lower left X coordinate for PostScript page position.
C      ILLY  -  Lower left Y coordinate for PostScript page position.
C      IURX  -  Upper right X coordinate for PostScript page position.
C      IURY  -  Upper right Y coordinate for PostScript page position.
C      NLLX  -  Lower left X coordinate for PostScript page position.
C      NLLY  -  Lower left Y coordinate for PostScript page position.
C      NURX  -  Upper right X coordinate for PostScript page position.
C      NURY  -  Upper right Y coordinate for PostScript page position.
C      MLLX  -  Lower left X coordinate for PS/EPSI bounding box.
C      MLLY  -  Lower left Y coordinate for PS/EPSI bounding box.
C      MURX  -  Upper right X coordinate for PS/EPSI bounding box.
C      MURY  -  Upper right Y coordinate for PS/EPSI bounding box.
C    FILSPC  -  Spacing between fill lines in PostScript.
C    HATSPC  -  Spacing between hatch fill lines in PostScript
C    RNLSCL  -  Nominal linewidth scale factor for PostScript lines.
C    ICOSCL  -  Scale factor for PostScript coordinate representation.
C    IPSCM   -  PostScript color model (1=RGB, 0=CMYK).
C    RMITER  -  Miter limit for PostScript lines when line join type is
C               miter.
C    IERRMX  -  Maximum number of GKS errors allowed before abort.
C    IGKSCP  -  Flag for controlling if GKS clipping is on.
C    IPERCX  -  Percentage error allowed in matching colors in X output.
C    IPRIVX  -  Flags whether an X window should have a private color
C               map instead of sharing with other maps.
C      ISCX  -  Shared color model for X.
C      IMCX  -  Mixed color model for X.
C      IPCX  -  Private color model for X.
C    ICDFLG  -  Flags whether NGDOTS draws circles or dots (0=dots;
C               1=circles).
C      ISUP  -  Suppress bkg. and/or bounding box for PS (0=neither;
C               1=suppress both; 2= suppress bkg.; 3=suppress bb.
C   LOGOCOL  -  Color index to be used for logos.
C   LOGOSND  -  Secondary color index for logos.
C   LOGOTYP  -  Logo type (1 = NCAR, 2 = UCAR).
C   OXLOGO   -  Logo X coordinate position in NDC.
C   OYLOGO   -  Logo Y coordinate position in NDC.
C   OLSIZE   -  Logo size.
C   IPGHGT   -  Page height for PDF media box.
C   IPGWTH   -  Page width for PDF media box.
C   IPSHGT   -  Page height for PS PageSize
C   IPSWTH   -  Page width for PS PageSize
C
      COMMON /NGCOMI/  IWKID, IFULLB,   IXPS,  ISTKMX,  IPTHMX, 
     +                ILJOIN, ILCAP,    ILLX,    ILLY,    IURX,
     +                  IURY, FILSPC, HATSPC,  RNLSCL,  ICOSCL,
     +                RMITER, IERRMX, IGKSCP,  IPERCX,   IPSCM,
     +                IPRIVX, ICDFLG,   ISCX,    IMCX,    IPCX,
     +                  ISUP, NLLX,     NLLY,    NURX,    NURY,
     +               LOGOTYP, LOGOCOL, LOGOSND       ,  OXLOGO,
     +                OYLOGO, OLSIZE, ISVSEG,  IPGHGT,  IPGWTH,
     +                  MLLX,   MLLY,   MURX,    MURY,  IPSWTH,
     +                IPSHGT
      COMMON /NGCOMC/ CFILNM, CPICNM, CSEGNM
      CHARACTER CFILNM*256, CPICNM*80, CSEGNM*15
