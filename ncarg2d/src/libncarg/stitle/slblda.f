C
C $Id: slblda.f,v 1.3 2000-07-12 16:25:57 haley Exp $
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
      BLOCK DATA SLBLDA
C
C This "routine" provides default values for the internal parameters of
C STITLE.
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C The common block SLCLRS holds color-table parameters.
C
        COMMON /SLCLRS/ CRED(256),CGRN(256),CBLU(256),LOCI(256),NOCI
        SAVE   /SLCLRS/
C
C GPSZ is the interline spacing assumed by FTITLE, in the plotter
C coordinate system.
C
        DATA GPSZ / 40. /
C
C IBGC is the default background color index.
C
        DATA IBGC / 0 /
C
C IBGF is the background fade flag (-2 for a fade from/to black in the
C HSV system, -1 for no fade, and "n" greater than or equal to 0 for a
C fade-in from the color with color index n/1000 and a fade-out to the
C color with color index MOD[n,1000], in the RGB system).
C
        DATA IBGF / -2 /
C
C ICOP is the centering option for lines written by FTITLE.
C
        DATA ICOP / 1 /
C
C IDOT is a flag saying whether to draw alignment frames with dots in
C the corners.
C
        DATA IDOT / 0 /
C
C IFGC is the default foreground color index.
C
        DATA IFGC / 1 /
C
C IFGF is the foreground fade flag (-2 for a fade from/to black in the
C HSV system, -1 for no fade, and "n" greater than or equal to 0 for a
C fade-in from the color with color index n/1000 and a fade-out to the
C color with color index MOD[n,1000], in the RGB system).
C
        DATA IFGF / -2 /
C
C IJMP is the distance between centers of practice frames.
C
        DATA IJMP / 300 /
C
C IMAP is the PLOTCHAR mapping flag.
C
        DATA IMAP / 100 /
C
C INCU is the card-input unit for FTITLE.
C
        DATA INCU / 5 /
C
C IWLU and IWWI are the logical unit and the workstation ID for WISS.
C
        DATA IWLU,IWWI / 4 , 9 /
C
C IWRK is the ID of the first active workstation, set to an impossible
C value, so that it will be initialized when it's needed.
C
        DATA IWRK / -1 /
C
C NOCI is the number of color indices currently defined by entries in
C the "list of color indices" array LOCI.
C
        DATA NOCI / 0 /
C
C IXST and IXND are the start and finish coordinates for STITLE in the
C X direction.
C
        DATA IXST,IXND / 512 ,512 /
C
C OORV is the PLOTCHAR out-of-range value.
C
        DATA OORV / 1.E12 /
C
C PCSZ is the nominal character size used by STITLE, in the plotter
C coordinate system.  All user character-size values are given as
C multiples of this.
C
        DATA PCSZ / 21. /
C
C RNFS is the real number of frames per second.  The default, 24, is
C an appropriate value for old-fashioned film, but video tape uses 30
C frames per second.
C
        DATA RNFS / 24. /
C
C RVPL, RVPR, RVPB, and RVPT define the limits of the STITLE viewport.
C
        DATA RVPB,RVPL,RVPR,RVPT / 0. , 0. , 1. , 1. /
C
C TFIN and TFOU are the default fade-in and fade-out times.
C
        DATA TFIN,TFOU / 0. , 0. /
C
C TGP1, TGP2, and TGP3 are the blank-frame gap lengths used by FTITLE.
C
        DATA TGP1,TGP2,TGP3 / 1. , .5 , 0. /
C
      END
