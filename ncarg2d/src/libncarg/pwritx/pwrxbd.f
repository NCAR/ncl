C
C	$Id: pwrxbd.f,v 1.3 2000-08-22 15:05:45 haley Exp $
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
      BLOCK DATA PWRXBD
C
C
C  SAVE DATA VALUES FOR LATER PASSES
C
      COMMON/PWRSV1/NNWD,NUMU5,INDLEN,IDDLEN,NUNIT
C
C  INITIALIZATION FOR MASK GENERATION
C
      COMMON/PINIT1/IFRST
      LOGICAL IFRST
C
C  MASKING AND OTHER MACHINE-DEPENDENT CONSTANTS WHICH MUST BE
C  CALCULATED.
C
      COMMON/PINIT/MASK(4),MASK6(64),MASK12(64),LAST6,LAST12,IBIT15,
     +        MASK14,NUM15U,NBWD
C
C INITIALIZATION FOR SUBROUTINES PWRITX AND XTCH.
      COMMON /PWRC2/ INDZER
C
C INITIALIZATION FOR SUBROUTINE PWRITX.
      LOGICAL FINIT
      COMMON /PSAV1/ FINIT,IFNT,IC,IT,ITO,RLX,RLY,RLOX,RLOY
C
C INITIALIZATION FOR SUBROUTINE PWRITX.
      COMMON /PUSER/ MODE
C
C
C  IFRST IS USED BY XTCH TO DECIDE IF MASKS HAVE BEEN GENERATED
C
      DATA IFRST/.FALSE./
C INDZER IS PUT INTO ARRAY LC BY ROUTINE XTCH TO INDICATE TO ROUTINE
C PWRITX THAT AN ALL-0-BITS UNIT WAS ENCOUNTERED.
C
      DATA INDZER /-2048/
C
C FINIT IS USED IN SUBROUTINE PWRITX. IT IS A FLAG TO INDICATE IF SOME
C REQUIRED INITIALIZATION HAS BEEN DONE.
C
      DATA FINIT /.FALSE./
C
C INITIALIZE INDICES TO ROMAN,PRINCIPAL, AND UPPER.
C
      DATA IFNT,IC,IT /0,0,0/
C
C INITIALIZE PREVIOUS CHARACTER SIZE TO PRINCIPAL.
C
      DATA ITO /0/
C
C INITIALIZE CHARACTER SIZE AND PREVIOUS CHARACTER SIZE TO PRINCIPAL.
C
      DATA RLX,RLY,RLOX,RLOY /16.0,32.0,16.0,32.0/
C
C INITIALIZE THE CHARACTER SET TO BE USED TO COMPLEX.
C
      DATA MODE /0/
C
C
      END
