C
C $Id: argetr.f,v 1.9 2000-07-12 16:21:47 haley Exp $
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
      SUBROUTINE ARGETR (IPN,RVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be retrieved.
C
C RVL is a real variable in which the desired value is to be
C returned by ARGETR.
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI,IRC(16),RLA,RWA,RDI,RSI
      SAVE   /ARCOMN/
C
C Declare the BLOCK DATA routine external, which should force it to
C load from a binary library.
C
      EXTERNAL ARBLDA
C
C Define a character temporary to hold an error message.
C
      CHARACTER*38 CTM
C
C Check for an uncleared prior error.
C
      IF (ICFELL('ARGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (.NOT.(LEN(IPN).LT.2)) GO TO 10001
        CTM(1:36)='ARGETR - PARAMETER NAME TOO SHORT - '
        CTM(37:36+LEN(IPN))=IPN
        CALL SETER (CTM(1:36+LEN(IPN)),2,1)
        RETURN
10001 CONTINUE
C
C Get the appropriate parameter value.
C
      IF (.NOT.(IPN(1:2).EQ.'AL'.OR.IPN(1:2).EQ.'al')) GO TO 10002
        RVL=RLA
      GO TO 10003
10002 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'AT'.OR.IPN(1:2).EQ.'at')) GO TO 10004
        IF (.NOT.(IAU.EQ.0)) GO TO 10005
          CALL ARINIT (IER)
          IF (.NOT.(IER.NE.0)) GO TO 10006
            CALL SETER
     +      ('ARGETR/ARINIT - VALUE OF ''LC'' IS TOO LARGE',3,1)
            RETURN
10006     CONTINUE
10005   CONTINUE
        RVL=REAL(IAU)
        IF (RVL.GE.3.) RVL=REAL(IBS)
      GO TO 10003
10004 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'AW'.OR.IPN(1:2).EQ.'aw')) GO TO 10007
        RVL=RWA
      GO TO 10003
10007 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DB'.OR.IPN(1:2).EQ.'db')) GO TO 10008
        RVL=REAL(IDB)
      GO TO 10003
10008 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DC'.OR.IPN(1:2).EQ.'dc')) GO TO 10009
        RVL=REAL(IDC)
      GO TO 10003
10009 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'DI'.OR.IPN(1:2).EQ.'di')) GO TO 10010
        RVL=REAL(IDI)
      GO TO 10003
10010 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'ID'.OR.IPN(1:2).EQ.'id')) GO TO 10011
        RVL=RDI
      GO TO 10003
10011 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'IS'.OR.IPN(1:2).EQ.'is')) GO TO 10012
        RVL=RSI
      GO TO 10003
10012 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'LC'.OR.IPN(1:2).EQ.'lc')) GO TO 10013
        RVL=REAL(ILC)
      GO TO 10003
10013 CONTINUE
      IF (.NOT.(IPN(1:2).EQ.'RC'.OR.IPN(1:2).EQ.'rc')) GO TO 10014
        CALL ARGPAI (IPN,3,IPI)
        IF (.NOT.(IPI.EQ.0)) GO TO 10015
          RVL=REAL(IRC(1))
        GO TO 10016
10015   CONTINUE
        IF (.NOT.(IPI.GE.1.AND.IPI.LE.16)) GO TO 10017
          RVL=REAL(IRC(IPI))
        GO TO 10016
10017   CONTINUE
          CALL SETER ('ARGETR - ''RC'' INDEX IS OUT OF RANGE',4,1)
          RETURN
10016   CONTINUE
      GO TO 10003
10014 CONTINUE
        CTM(1:36)='ARGETR - PARAMETER NAME NOT KNOWN - '
        CTM(37:38)=IPN(1:2)
        CALL SETER (CTM(1:38),5,1)
        RETURN
10003 CONTINUE
C
C Done.
C
      RETURN
C
      END
