C
C	$Id: pccfff.f,v 1.5 2002-03-28 00:18:06 fred Exp $
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
      SUBROUTINE PCCFFF (IPSS,IBNU,NFNT,NASC,CHGT,SIZE,RDGU,LDGU,NDGU)        
C
      include 'pcffme.h'
      include 'pcffdx.h'
C
      DIMENSION RDGU(LDGU)
C
C The subroutine PCCFFF retrieves the digitization of a specified
C character from one of the locally-defined NCAR Graphics fontcaps
C that are defined for filled fonts.
C
C IPSS is the number of the pass being executed by PLCHHQ.
C
C IBNU is the number of a unit to be used in reading the fontcaps.
C
C NFNT is the number of the desired font.
C
C NASC is the ASCII decimal equivalent of the desired character.
C
C CHGT is a REAL specifying the desired character height in digitization
C      units (this is the value into which  FNTCAP-FNTBAS is mapped.)
C
C SIZE is a REAL specifying the approximate desired character height
C      in NDC units.
C
C RDGU is a real array in which the digitization is to be returned.
C
C LDGU is the length of RDGU.
C
C NDGU is the number of elements returned in RDGU.
C
C The digitization is to be in the form expected by PLCHHQ, in a
C coordinate system centered at the center of the character.  RDGU(1)
C must be a negative number whose magnitude is the distance from the
C center of the character to its left edge.  RDGU(2) must be a positive
C number whose magnitude is the distance from the center of the
C character to its right edge.
C
C For each odd value of I greater than 2, RDGU(I) is an X coordinate
C and RDGU(I+1) is a Y coordinate of some point on a polyline that is
C to be drawn or filled, with the following exception:  each polyline
C is is terminated by a "point" with X coordinate -2048. or -2047.; 
C the value -2048. implies that the polyline is to be drawn and -2047. 
C that it is to be filled.  This point is not actually part of the 
C polyline, it simply marks the end of the polyline.  The Y coordinate
C paired with it says what color is to be used for the draw or fill.  
C If the Y coordinate is 0., no special color is implied; drawing or
C filling will be done using whatever color is implied by internal
C parameters of PLOTCHAR determining the colors to be used for the 
C various parts of normal characters.  If the Y coordinate is positive,
C a special color is implied (as, for example in state and federal
C highway signs).  
C
C ---------------------------------
C
C The variable NFRL, if non-zero, defines the number of the font read
C last.
C
      SAVE NFRL
C
      DATA NFRL / 0/
C
C Zero the count of digitization elements returned.
C
      NDGU=0
      NFNTL = NFNT
C
C If the desired font is not the one read last, read it.  If there is
C any problem with reading it, just return with NDGU zeroed to indicate
C that the desired character was not available.
C
      IF (NFRL .NE. NFNT) THEN
C
C  Set the OUTLIN flag for outline fonts and reassign the font number.
C
        IF (NFNT.EQ.121 .OR. NFNT.EQ.122 .OR. NFNT.EQ.125 .OR.
     +      NFNT.EQ.126 .OR. NFNT.EQ.129 .OR. NFNT.EQ.130 .OR.
     +      NFNT.EQ.133 .OR. NFNT.EQ.134 .OR. NFNT.EQ.135 .OR.
     +      NFNT.EQ.136 .OR. NFNT.EQ.137 .OR. NFNT.EQ.199) THEN
          OUTLIN = 1
          NFNTL = NFNT - 100
        ELSE
          OUTLIN = 0
        ENDIF
C
C Open the fontcap file.
C
        IBNS=IBNU
        CALL PCFOPN (IBNU,NFNTL)
C
C Read the fontcap.
C
        CALL PCFRED (IBNU,NFNTL,IBFC,IFCLEN)
C
C Close the fontcap file.
C
        CALL PCFCLS (IBNU,NFNTL)
        IBNU=IBNS
C
C Set the variable NFRL to reflect the number of the font last read.
C
        NFRL=NFNT
C
C Get the font metric information from the fontcap and store in common
C PCINDX.
C
        CALL PCFFME (CHGT)
      END IF
C
C  Calculate the scale factor to go from the font coordinate space 
C  to the height specified in the call to PCCFFF.
C
      SCALE = CHGT/REAL(FCAP-FBASE)
C
C Get the digitization.
C
      CALL PCFFGD (IPSS,NASC,CHGT,SIZE,RDGU,LDGU,NDGU)
C
C Done.
C
      RETURN
C
      END
