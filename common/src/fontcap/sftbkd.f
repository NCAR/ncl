C
C	$Id: sftbkd.f,v 1.5 2008-07-27 12:23:42 haley Exp $
C
C                                                                      
C			     Copyright (C)  1997
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SFTBKD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA SFTBKDX
C
C  COMMON data definitions for fontcap parser for stroked fonts.
C
      include 'capfnt.h'
      include 'fnttab.h'
      include 'fnterr.h'
C
C  Type the local variables.
C
      INTEGER II
C
C  Define the locations of the individual tables in the PART1 array.
C
      INTEGER PT01,PT02,PT03,PT04,PT05,
     1        PTEND
      PARAMETER (PT01=1)
      PARAMETER (PT02=PT01+PD011*PD012)
      PARAMETER (PT03=PT02+PD021*PD022)
      PARAMETER (PT04=PT03+PD031*PD032)
      PARAMETER (PT05=PT04+PD041*PD042)
      PARAMETER (PTEND=PT05+PD051*PD052)
C
C  Define table row starts.
C
      INTEGER RS02, RS03, RS04, RS05
      PARAMETER (RS02 = PD011+1)
      PARAMETER (RS03 = RS02 + PD021)
      PARAMETER (RS04 = RS03 + PD031)
      PARAMETER (RS05 = RS04 + PD041)
C
      DATA FNTRHT, FNTTOP, FNTCAP, FNTHLF, FNTBAS, FNTBOT, FNTTYP
     1    /     0,      0,      0,      0,      0,      0,     -1/
      DATA CORXST, CORXLN, CORYST, CORYLN, CORPST, CORPLN/ 6*0/
      DATA PBEGST, PBEGLN, PENDST, PENDLN/ 4*0/
      DATA CPNLST/ 0/
C
C  Define the major classes.
C
      DATA (PART1(II),II=PT01,(PT02-1))/
     1     'C','H','A','R','A','C','T','E','R',' '
     2    ,'C','O','O','R','D',5*' '
     3    ,'P','A','I','N','T',5*' '
     4    ,'C','H','A','R',6*' '
     5    ,'F','O','N','T',6*' '
     Z    /
C
C  Define the subclasses of class CHARACTER.
C
      DATA (PART1(II),II=PT02,(PT03-1))/
     1     'S','T','A','R','T',' '
     2    ,'E','N','D',3*' '
     3    ,'W','I','D','T','H',' '
     Z    /
C
C  Define the subclasses of class COORD.
C
      DATA (PART1(II),II=PT03,(PT04-1))/
     1     'X','_','S','T','A','R','T',3*' '
     2    ,'X','_','L','E','N',5*' '
     3    ,'Y','_','S','T','A','R','T',3*' '
     4    ,'Y','_','L','E','N',5*' '
     5    ,'P','E','N','_','S','T','A','R','T',' '
     6    ,'P','E','N','_','L','E','N',3*' '
     Z    /
C
C  Define the subclasses of class PAINT.
C
      DATA (PART1(II),II=PT04,(PT05-1))/
     1     'B','E','G','I','N','_','S','T','A','R','T',' '
     2    ,'B','E','G','I','N','_','L','E','N',' ',' ',' '
     3    ,'E','N','D','_','S','T','A','R','T',' ',' ',' '
     4    ,'E','N','D','_','L','E','N',' ',' ',' ',' ',' '
     Z    /
C
C  Define the subclasses of class FONT.
C
      DATA (PART1(II),II=PT05,(PTEND-1))/
     1     'R','I','G','H','T',' '
     2    ,'T','O','P',3*' '
     3    ,'C','A','P',3*' '
     4    ,'H','A','L','F',' ',' '
     5    ,'B','A','S','E',' ',' '
     6    ,'B','O','T','T','O','M'
     7    ,'T','Y','P','E',' ',' '
     Z    /
C
C  Define parser tables 2 - 5.
C
      DATA PART2/
     1          RS02, RS03, RS04, 0, RS05, 3*0, 6*0, 4*0, 7*0
     Z         /
      DATA PART3/
     1          PD011*1, PD021*2, PD031*3, PD041*4, PD051*5
     Z         /
      DATA PART4/
     1         PT01,PT02,PT03,PT04,PT05
     2        /
      DATA PART5/
     1         PD011,PD012,PD021,PD022,PD031,PD032,PD041,PD042,
     2         PD051,PD052
     Z        /
C
C  Define the keyword separator character and the keyword terminator
C  character.
C 
      DATA KEYSEP/'_'/, KEYTER/' '/
C
C  Define the initial two-character sequence to be interpreted
C  as beginning comment lines.
C
      DATA FRMCOM /'/','*'/
C
C Initialize buffer pointer.
C
      DATA IPTR/1/
C
C  Error flags in COMMON FNTERR.
C
      DATA ALLOK, EOFFL, INTERR, MAXINT
     1     /0, 1, 2, 3/
C
C  Number of bits used to store the integer flags used to specify
C  major class and subclass.
C
      DATA PTHBTS /4/
      END
