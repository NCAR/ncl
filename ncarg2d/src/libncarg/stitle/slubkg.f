C
C $Id: slubkg.f,v 1.4 2000-07-12 16:26:00 haley Exp $
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
      SUBROUTINE SLUBKG (IPOC)
C
C This routine may be replaced by the user with code to add graphics to
C the background over which the titles are being scrolled.  Care should
C be taken when altering the state of GKS or SPPS.
C
C IPOC says what is going on in STITLE at the time that SLUBKG is
C called, as follows:
C
C IPOC  Position of call to SLUBKG
C ----  ---------------------------------------------------------------
C  -1   Just before drawing titles on a "fade-in" frame.
C  +1   Just after drawing titles on a "fade-in" frame.
C  -2   Just before drawing titles on a "start" frame.
C  +2   Just after drawing titles on a "start" frame.
C  -3   Just before drawing titles on a "move" frame.
C  +3   Just after drawing titles on a "move" frame.
C  -4   Just before drawing titles on an "end" frame.
C  +4   Just after drawing titles on an "end" frame.
C  -5   Just before drawing titles on a "fade-out" frame.
C  +5   Just after drawing titles on a "fade-out" frame.
C
C The default version of the routine does nothing.
C
        RETURN
C
      END
