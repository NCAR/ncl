C
C	$Id: ngblda.f,v 1.12 2001-02-06 21:17:47 fred Exp $
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
