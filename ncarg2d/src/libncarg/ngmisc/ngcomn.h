C
C $Id: ngcomn.h,v 1.15 2002-04-04 22:04:15 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
C   OXLOGO   -  Color index to be used for logos.
C   OYLOGO   -  Secondary color index for logos.
C   OLSIZE   -  Logo type (1 = NCAR, 2 = UCAR).
C
      COMMON /NGCOMI/  IWKID, IFULLB,   IXPS,  ISTKMX,  IPTHMX, 
     +                ILJOIN, ILCAP,    ILLX,    ILLY,    IURX,
     +                  IURY, FILSPC, HATSPC,  RNLSCL,  ICOSCL,
     +                RMITER, IERRMX, IGKSCP,  IPERCX,   IPSCM,
     +                IPRIVX, ICDFLG,   ISCX,    IMCX,    IPCX,
     +                  ISUP, NLLX,     NLLY,    NURX,    NURY,
     +               LOGOTYP, LOGOCOL, LOGOSND       ,  OXLOGO,
     +                OYLOGO, OLSIZE
      COMMON /NGCOMC/ CFILNM, CPICNM, CSEGNM
      CHARACTER CFILNM*256, CPICNM*80, CSEGNM*15
