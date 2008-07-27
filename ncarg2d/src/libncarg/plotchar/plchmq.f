C
C $Id: plchmq.f,v 1.15 2008-07-27 00:17:20 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PLCHMQ (XPOS,YPOS,CHRS,SIZE,ANGD,CNTR)
C
C This is the medium-quality character-drawing routine.
C
C
C D E C L A R A T I O N S
C
C
      CHARACTER*(*) CHRS
C
C The COMMON block PCPFLQ contains internal parameters that affect the
C behavior of routines besides PLCHHQ.
C
      COMMON /PCPFLQ/ IMAP,OORV,RHTW
      SAVE   /PCPFLQ/
C
C Define an array in which to declare the 94 legal characters.
C
      CHARACTER*1 LCHR(94)
C
C Define arrays in which to declare the pointers into the digitization
C arrays and the digitizations themselves.
C
      DIMENSION INDX(94),KXCO(857),KYCO(857)
C
C Three internal variables are subject to change and need to be saved
C from each call to the next.
C
      SAVE INIT,LCHR,INDX
C
C The following DATA statements associate each character with an index
C in the digitization arrays.  For example, the digitization for the
C character A starts at KXCO(328) and KYCO(328), while that for a B
C starts at KXCO(336) and KYCO(336).
C
      DATA (LCHR(I),I=  1, 94) /
     O                         '!','"','#','$','%','&','''','(',')','*',
     1                         '+',',','-','.','/','0','1','2','3','4',
     2                         '5','6','7','8','9',':',';','<','=','>',
     3                         '?','@','A','B','C','D','E','F','G','H',
     4                         'I','J','K','L','M','N','O','P','Q','R',
     5                         'S','T','U','V','W','X','Y','Z','[','\\',
     6                         ']','^','_','`','a','b','c','d','e','f',
     7                         'g','h','i','j','k','l','m','n','o','p',
     8                         'q','r','s','t','u','v','w','x','y','z',
     9                         '{','|','}','~'                        /
C
      DATA (INDX(I),I=  1, 94) /
     O                           1, 11, 23, 35, 51, 74, 87, 93,100,107,
     1                         116,122,129,132,139,142,155,159,168,183,
     2                         193,203,215,219,238,250,264,278,282,288,
     3                         292,308,328,336,350,359,367,377,386,399,
     4                         408,417,423,432,436,442,447,457,465,478,
     5                         489,502,508,515,519,525,531,538,543,548,
     6                         551,556,560,563,566,581,593,602,614,625,
     7                         634,649,658,669,683,692,697,712,721,731,
     8                         743,755,763,776,785,794,798,804,810,816,
     9                         821,833,839,851                        /
C
C The following DATA statements define the character digitizations.
C Each character is digitized in a box which is basically 60 units
C wide by 70 units tall.  The width includes 20 units of white space
C along the right edge of the character.  A few characters dip below
C the bottom of the box.  Vertical spacing is intended to be 120 units.
C If KXCO(I) = 77, KYCO(I) is a flag:  KYCO(I) = 0 means that the next
C delared move is pen-up (all others are pen-down), and KYCO(I)=77
C signals the end of the digitization for a given character.
C
C Note:  KYCO(151) changed from 0 to 77 to remove slash from zero.  DJK
C (3/11/88).
C
      DATA (KXCO(I),I=  1,100) /
     O                          21, 21, 77, 19, 19, 24, 24, 19, 24, 77,
     1                           9,  9, 77,  9,  9, 77, 31, 31, 77, 31,
     2                          31, 77, 14,  9, 77, 26, 31, 77, 40,  5,
     3                          77,  0, 35, 77, 19, 19, 77,  0,  9, 28,
     4                          38, 38, 28,  9,  0,  0,  9, 28, 38, 77,
     5                          12,  5,  0,  0,  5, 12, 16, 16, 12, 77,
     6                          40,  0, 77, 24, 24, 28, 35, 40, 40, 35,
     7                          28, 24, 77, 40,  5,  5,  9, 16, 21, 21,
     8                           0,  0,  7, 24, 40, 77, 19, 16, 19, 21,
     9                          19, 77, 24, 19, 16, 16, 19, 24, 77, 16/
      DATA (KYCO(I),I=  1,100) /
     O                          70, 21,  0,  3, -3, -3,  3,  3, -3, 77,
     1                          70, 60,  0, 60, 70,  0, 70, 60,  0, 60,
     2                          70, 77, 65, 26,  0, 26, 65,  0, 54, 54,
     3                           0, 36, 36, 77, 70,  0,  0, 16,  5,  5,
     4                          13, 26, 36, 36, 44, 54, 65, 65, 54, 77,
     5                          70, 70, 65, 57, 52, 52, 57, 65, 70,  0,
     6                          70,  0,  0, 13,  5,  0,  0,  5, 13, 18,
     7                          18, 13, 77,  0, 52, 62, 70, 70, 62, 54,
     8                          23, 10,  0,  0, 23, 77, 70, 60, 60, 70,
     9                          70, 77, 70, 60, 47, 23, 10,  0, 77, 70/
      DATA (KXCO(I),I=101,200) /
     O                          21, 24, 24, 21, 16, 77,  0, 38, 77, 19,
     1                          19, 77,  0, 38, 77, 19, 19, 77,  0, 38,
     2                          77, 16, 19, 19, 16, 16, 19, 77,  0, 38,
     3                          77, 19, 19, 24, 24, 19, 24, 77,  0, 40,
     4                          77, 12,  0,  0, 12, 28, 40, 40, 28, 12,
     5                          77, 40,  0, 77,  5, 21, 21, 77,  0,  9,
     6                          31, 40, 40,  0,  0, 40, 77,  0, 12, 28,
     7                          40, 40, 28, 19, 77, 28, 40, 40, 28, 12,
     8                           0, 77, 31, 31, 26,  0,  0, 31, 77, 31,
     9                          40, 77,  2, 28, 40, 40, 28, 12,  0,  0/
      DATA (KYCO(I),I=101,200) /
     O                          60, 47, 23, 10,  0, 77, 54, 16,  0, 62,
     1                           8,  0, 16, 54, 77, 52, 10,  0, 31, 31,
     2                          77,-10, -5,  3,  3,  0,  0, 77, 31, 31,
     3                          77,  3, -3, -3,  3,  3, -3, 77,  0, 70,
     4                          77, 70, 54, 16,  0,  0, 16, 54, 70, 70,
     5                          77, 70,  0, 77, 54, 70,  0, 77, 60, 70,
     6                          70, 60, 39, 10,  0,  0, 77, 60, 70, 70,
     7                          60, 47, 36, 36,  0, 36, 26, 10,  0,  0,
     8                          13, 77,  0, 70, 70, 23, 18, 18,  0, 18,
     9                          18, 77,  0,  0, 13, 31, 44, 44, 34, 70/
      DATA (KXCO(I),I=201,300) /
     O                          40, 77,  0, 12, 28, 40, 40, 28, 12,  0,
     1                           0,  2, 21, 77,  0, 40, 12, 77,  9,  0,
     2                           0,  9,  0,  0,  9, 31, 40, 40, 31,  9,
     3                          77, 31, 40, 40, 31,  9, 77, 21, 38, 40,
     4                          40, 28, 12,  0,  0, 12, 28, 40, 77, 19,
     5                          24, 24, 19, 19, 24, 77, 19, 19, 24, 24,
     6                          19, 24, 77, 19, 24, 24, 19, 19, 24, 77,
     7                          19, 21, 21, 19, 19, 21, 77, 33,  2, 33,
     8                          77, 38,  2, 77,  2, 38, 77,  2, 33,  2,
     9                          77,  5,  5, 12, 24, 33, 33, 16, 16, 77/
      DATA (KYCO(I),I=201,300) /
     O                          70, 77, 29, 41, 41, 29, 13,  0,  0, 13,
     1                          29, 49, 70, 77, 70, 70,  0, 77, 70, 60,
     2                          47, 36, 26, 10,  0,  0, 10, 26, 36, 36,
     3                           0, 36, 47, 60, 70, 70, 77,  0, 21, 41,
     4                          57, 70, 70, 57, 41, 29, 29, 41, 77, 31,
     5                          31, 26, 26, 31, 26,  0, 16, 10, 10, 16,
     6                          16, 10, 77, 31, 31, 26, 26, 31, 26,  0,
     7                           3,  8, 16, 16, 13, 13, 77, 52, 31, 10,
     8                          77, 41, 41,  0, 21, 21, 77, 52, 31, 10,
     9                          77, 54, 62, 70, 67, 60, 47, 29, 18,  0/
      DATA (KXCO(I),I=301,400) /
     O                          16, 16, 21, 21, 16, 21, 77, 31,  9,  0,
     1                           0,  9, 31, 40, 40, 35, 31, 28, 28, 24,
     2                          16, 12, 12, 16, 24, 28, 77,  0, 14, 24,
     3                          38, 77,  5, 33, 77,  0, 31, 40, 40, 31,
     4                           0, 77, 31, 40, 40, 31,  0,  0, 77, 40,
     5                          28, 12,  0,  0, 12, 28, 40, 77,  0, 28,
     6                          40, 40, 28,  0,  0, 77,  0,  0, 40, 77,
     7                           0, 26, 77,  0, 40, 77,  0,  0, 77,  0,
     8                          26, 77,  0, 40, 77, 24, 40, 40, 77, 40,
     9                          28, 12,  0,  0, 12, 28, 40, 77,  0,  0/
      DATA (KYCO(I),I=301,400) /
     O                           3, -3, -3,  3,  3, -3, 77, 10, 10, 21,
     1                          49, 60, 60, 49, 29, 23, 23, 29, 39, 47,
     2                          47, 39, 29, 23, 23, 29, 77,  0, 70, 70,
     3                           0,  0, 26, 26, 77, 70, 70, 60, 47, 36,
     4                          36,  0, 36, 26, 10,  0,  0, 70, 77, 13,
     5                           0,  0, 13, 57, 70, 70, 57, 77, 70, 70,
     6                          57, 13,  0,  0, 70, 77, 70,  0,  0,  0,
     7                          36, 36,  0, 70, 70, 77, 70,  0,  0, 36,
     8                          36,  0, 70, 70, 77, 26, 26,  0,  0, 13,
     9                           0,  0, 13, 57, 70, 70, 57, 77, 70,  0/
      DATA (KXCO(I),I=401,500) /
     O                          77,  0, 40, 77, 40, 40, 77,  7, 31, 77,
     1                          19, 19, 77,  9, 31, 77,  0, 12, 26, 38,
     2                          38, 77,  0,  0, 77, 40, 12, 77,  0, 40,
     3                          77,  0,  0, 40, 77,  0,  0, 19, 38, 38,
     4                          77,  0,  0, 40, 40, 77,  0, 12, 28, 40,
     5                          40, 28, 12,  0,  0, 77,  0,  0, 31, 40,
     6                          40, 31,  0, 77,  0,  0, 12, 28, 40, 40,
     7                          28, 12,  0, 77, 16, 40, 77,  0,  0, 31,
     8                          40, 40, 31,  0, 77, 31, 40, 77,  0,  9,
     9                          31, 40, 40, 31,  9,  0,  0,  9, 31, 40/
      DATA (KYCO(I),I=401,500) /
     O                           0, 34, 34,  0,  0, 70, 77, 70, 70,  0,
     1                          70,  0,  0,  0,  0, 77, 13,  0,  0, 13,
     2                          70, 77, 70,  0,  0,  0, 39,  0, 26, 70,
     3                          77, 70,  0,  0, 77,  0, 70, 34, 70,  0,
     4                          77,  0, 70,  0, 70, 77, 57, 70, 70, 57,
     5                          13,  0,  0, 13, 57, 77,  0, 70, 70, 60,
     6                          41, 31, 31, 77, 57, 13,  0,  0, 13, 57,
     7                          70, 70, 57,  0, 41,-10, 77,  0, 70, 70,
     8                          60, 44, 36, 36,  0, 36,  0, 77, 10,  0,
     9                           0, 10, 23, 34, 36, 47, 60, 70, 70, 60/
      DATA (KXCO(I),I=501,600) /
     O                          77, 21, 21, 77,  0, 42, 77,  0,  0, 12,
     1                          28, 40, 40, 77,  0, 19, 38, 77,  0,  9,
     2                          21, 33, 42, 77,  0, 40, 77,  0, 40, 77,
     3                           0, 19, 19, 77, 19, 38, 77,  0, 40,  0,
     4                          40, 77, 26, 14, 14, 26, 77,  0, 40, 77,
     5                          12, 24, 24, 12, 77,  7, 19, 31, 77, -2,
     6                          42, 77, 16, 21, 77,  9, 24, 31, 31, 21,
     7                           9,  0,  0,  7, 24, 31, 77, 31, 40, 77,
     8                           0,  0, 77,  0,  9, 26, 35, 35, 26,  9,
     9                           0, 77, 33, 26,  9,  0,  0,  9, 26, 33/
      DATA (KYCO(I),I=501,600) /
     O                          77,  0, 70,  0, 70, 70, 77, 70, 13,  0,
     1                           0, 13, 70, 77, 70,  0, 70, 77, 70,  0,
     2                          34,  0, 70, 77, 70,  0,  0,  0, 70, 77,
     3                          70, 39,  0,  0, 39, 70, 77, 70, 70,  0,
     4                           0, 77, 75, 75, -3, -3, 77, 70,  0, 77,
     5                          75, 75, -3, -3, 77, 62, 70, 62, 77,-13,
     6                         -13, 77, 70, 54, 77, 49, 49, 39, 10,  0,
     7                           0, 10, 21, 31, 31, 26,  0, 10,  0, 77,
     8                          70,  0,  0, 10,  0,  0, 10, 29, 39, 39,
     9                          29, 77, 34, 39, 39, 29, 10,  0,  0,  5/
      DATA (KXCO(I),I=601,700) /
     O                          77, 35, 26,  9,  0,  0,  9, 26, 35, 77,
     1                          35, 35, 77,  0, 35, 35, 26,  9,  0,  0,
     2                           9, 26, 35, 77,  5, 31, 77, 14, 14, 21,
     3                          31, 35, 77,  0,  9, 26, 35, 35, 77, 35,
     4                          26,  9,  0,  0,  9, 26, 35, 77,  0,  0,
     5                          77,  0,  9, 26, 35, 35, 77, 16, 16, 21,
     6                          21, 16, 21, 77, 16, 19, 19, 77, 16, 16,
     7                          21, 21, 16, 21, 77, 16, 19, 19, 14,  7,
     8                           2, 77,  0,  0, 77,  0, 28, 77,  9, 35,
     9                          77, 14, 19, 19, 24, 77,  0,  0, 77, 19/
      DATA (KYCO(I),I=601,700) /
     O                          77, 29, 39, 39, 29, 10,  0,  0, 10,  0,
     1                           0, 70, 77, 21, 21, 29, 39, 39, 29, 10,
     2                           0,  0,  8, 77, 41, 41,  0,  0, 57, 65,
     3                          65, 60, 77,-13,-21,-21,-10, 39,  0, 29,
     4                          39, 39, 29, 10,  0,  0, 10, 77, 70,  0,
     5                           0, 29, 39, 39, 29,  0, 77, 54, 49, 49,
     6                          54, 54, 49,  0, 34, 34,  0, 77, 54, 49,
     7                          49, 54, 54, 49,  0, 34, 34,-16,-21,-21,
     8                         -16, 77, 70,  0,  0, 31, 47,  0, 36,  0,
     9                          77, 70, 65,  5,  0, 77, 39,  0,  0,  0/
      DATA (KXCO(I),I=701,800) /
     O                          19, 77,  0,  7, 14, 19, 24, 31, 38, 38,
     1                          77,  0,  0, 77,  0,  9, 26, 35, 35, 77,
     2                           0,  0,  9, 26, 35, 35, 26,  9,  0, 77,
     3                           0,  0, 77,  0,  9, 26, 35, 35, 26,  9,
     4                           0, 77, 35, 35, 77, 35, 26,  9,  0,  0,
     5                           9, 26, 35, 77,  0,  0, 77,  0,  9, 26,
     6                          35, 77,  0,  9, 26, 35, 35, 26,  9,  0,
     7                           0,  9, 26, 33, 77,  7, 31, 77, 33, 28,
     8                          21, 16, 16, 77,  0,  0,  9, 26, 35, 77,
     9                          35, 35, 77,  2, 19, 35, 77,  0,  9, 19/
      DATA (KYCO(I),I=701,800) /
     O                          31,  0, 31, 39, 39, 31, 39, 39, 31,  0,
     1                          77, 39,  0,  0, 29, 39, 39, 29,  0, 77,
     2                          29, 10,  0,  0, 10, 29, 39, 39, 29, 77,
     3                          39,-21,  0, 29, 39, 39, 29, 10,  0,  0,
     4                          10, 77, 39,-21,  0, 10,  0,  0, 10, 29,
     5                          39, 39, 29, 77, 39,  0,  0, 29, 39, 39,
     6                          31, 77,  5,  0,  0,  5, 16, 21, 21, 26,
     7                          34, 39, 39, 34, 77, 39, 39,  0,  5,  0,
     8                           0,  5, 65, 77, 39, 10,  0,  0, 10,  0,
     9                           0, 39, 77, 39,  0, 39, 77, 39,  0, 34/
      DATA (KXCO(I),I=801,857) /
     O                          28, 38, 77,  2, 35, 77,  2, 35, 77,  0,
     1                          19, 77,  9, 38, 77,  2, 35,  2, 35, 77,
     2                          26, 21, 16, 16, 14,  9, 14, 16, 16, 21,
     3                          26, 77, 19, 19, 77, 19, 19, 77, 14, 19,
     4                          24, 24, 26, 31, 26, 24, 24, 19, 14, 77,
     5                           0,  7, 14, 26, 33, 40, 77            /
      DATA (KYCO(I),I=801,857) /
     O                           0, 39, 77, 39,  0,  0,  0, 39, 77, 39,
     1                           0,  0,-21, 39, 77, 39, 39,  0,  0, 77,
     2                          75, 75, 67, 44, 39, 36, 34, 29,  3, -3,
     3                          -3, 77, 70, 44,  0, 26,  0, 77, 75, 75,
     4                          67, 44, 39, 36, 34, 29,  3, -3, -3, 77,
     5                          29, 36, 36, 23, 23, 31, 77            /
C
C INIT is 0 if this is the first call to PLCHMQ, 1 otherwise.
C
      DATA INIT / 0 /
C
C NLCH is the length of LCHR and INDX.
C
      DATA NLCH / 94 /
C
C WIDE and HIGH are the digitized width and height of the characters;
C WIDE includes white space on the right edge, the width of which is
C WHTE.
C
      DATA WIDE,HIGH,WHTE / 60.,70.,20. /
C
C
C I N I T I A L I Z A T I O N
C
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PCBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PLCHMQ - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C On the first call to PLCHMQ, sort LCHR, maintaining its relationship
C with INDX.  (For example, after sorting, if LCHR(I)='B', INDX(I)=336.)
C The sorting makes it possible to locate characters quickly.
C
      IF (INIT.EQ.0) THEN
        INIT=1
        CALL PCSORT (LCHR,INDX,NLCH)
      END IF
C
C Find the length of the string and, if it's zero, quit.
C
      NCHR=LEN(CHRS)
      IF (NCHR.LE.0) RETURN
C
C Get the fractional coordinates of the reference point.
C
      IF (IMAP.LE.0) THEN
        XFRA=CUFX(XPOS)
        IF (ICFELL('PLCHMQ',2).NE.0) RETURN
        YFRA=CUFY(YPOS)
        IF (ICFELL('PLCHMQ',3).NE.0) RETURN
      ELSE
        XFRA=XPOS
        YFRA=YPOS
      END IF
C
C Determine the resolution of the plotter, as declared by default or
C by the user.
C
      CALL GETUSV ('XF',IRSX)
      IF (ICFELL('PLCHMQ',4).NE.0) RETURN
      RSLN=2.**IRSX-1.
C
C Determine a multiplier for the digitized size which will make the
C characters have the size requested by the user.  First, compute the
C same multiplier we would use for PLCHHQ (except for the adjustment
C factor SIZA) ...
C
      IF (IMAP.LE.0) THEN
        IF (SIZE.LE.0.) THEN
          SIZM=ABS(SIZE)/1023.
        ELSE IF (SIZE.LT.1.) THEN
          SIZM=SIZE/16.
        ELSE
          SIZM=(SIZE/RSLN)/16.
        END IF
      ELSE
        SIZM=SIZE/16.
      END IF
C
C ... and then adjust for the fact that the digitization is different.
C
      SIZM=16.*SIZM/WIDE
C
C Compute the orientation angle, in radians, and the magnitudes of the
C X and Y components of a vector at that angle with magnitude SIZM.
C
      ANGR=.017453292519943*ANGD
C
      XCOV=SIZM*COS(ANGR)
      YCOV=SIZM*SIN(ANGR)
C
C Compute a multiplier for the digitized y coordinates that will make
C the height come out right.
C
      YMLT=ABS(RHTW)/1.75
C
C Find the fractional coordinates for the beginning of the string.  We
C must take into account the fact that each character is digitized with
C (0,0) at the lower left-hand corner of the character.  We must also
C take into account the user's centering option.
C
      XLLC=XFRA+.5*HIGH*YMLT*YCOV
     +                         -.5*(CNTR+1.)*(REAL(NCHR)*WIDE-WHTE)*XCOV
      YLLC=YFRA-.5*HIGH*YMLT*XCOV
     +                         -.5*(CNTR+1.)*(REAL(NCHR)*WIDE-WHTE)*YCOV
C
C
C C H A R A C T E R   L O O P
C
C
C Loop through all the characters in the input string.
C
      DO 107 ICHR=1,NCHR
C
C If the character is not a blank, get a pointer to the beginning of
C its digitization.
C
        IPNT=0
        IF (CHRS(ICHR:ICHR).NE.' ')
     +                 CALL PCGPTR (CHRS(ICHR:ICHR),LCHR,INDX,NLCH,IPNT)
C
C Once we've got a valid pointer, stroke out the character it points to.
C The pen starts out "up".  If mapping is turned on and an out-of-range
C value is defined, initialize a visibility flag and X and Y coordinates
C of a "new" point.
C
        IF (IPNT.GT.0) THEN
C
          IPNT=IPNT-1
          IPEN=0
C
          IF (IMAP.GT.0.AND.OORV.NE.0.) THEN
            IVSN=0
            XNEW=0.
            YNEW=0.
          END IF
C
C Advance to the next digitization pair.
C
  101     IPNT=IPNT+1
C
C Check for an X/Y coordinate pair (as opposed to an op code).
C
          IF (KXCO(IPNT).NE.77) THEN
C
C Process an X/Y coordinate pair.  See if mapping is off or on.
C
            IF (IMAP.LE.0) THEN
C
C Mapping is turned off; just compute the appropriate fractional
C coordinates and use the SPPS routine PLOTIF.
C
              CALL PLOTIF (XLLC+REAL(KXCO(IPNT))*XCOV-
     +                     YMLT*REAL(KYCO(IPNT))*YCOV,
     +                     YLLC+REAL(KXCO(IPNT))*YCOV+
     +                     YMLT*REAL(KYCO(IPNT))*XCOV,IPEN)
              IF (ICFELL('PLCHMQ',5).NE.0) RETURN
C
            ELSE
C
C Mapping is turned on.  If the pen is up, just move to the current
C point.  If the pen is down and the current point is too far from the
C last one, arrange to generate some points in between.
C
              IF (IPEN.EQ.0) THEN
                XTMB=0.
                YTMB=0.
                NINT=1
              ELSE
                XTMB=REAL(KXCO(IPNT-1))
                YTMB=REAL(KYCO(IPNT-1))
                NINT=MAX(1,ABS(KXCO(IPNT)-KXCO(IPNT-1))/7,
     +                     ABS(KYCO(IPNT)-KYCO(IPNT-1))/7)
              END IF
C
              XTME=REAL(KXCO(IPNT))
              YTME=REAL(KYCO(IPNT))
C
C Begin interpolation loop.
C
              DO 106 IINT=1,NINT
C
C Interpolate to get an X/Y position.
C
                P=REAL(NINT-IINT)/REAL(NINT)
C
                XTMI=P*XTMB+(1.-P)*XTME
                YTMI=P*YTMB+(1.-P)*YTME
C
C Check whether an out-of-range value is defined or not.
C
                IF (OORV.EQ.0.) THEN
C
C No out-of-range value is defined; do the mapping, transform to the
C fractional system, and use PLOTIF.
C
                  CALL PCMPXY (IMAP,XLLC+XTMI*XCOV-YMLT*YTMI*YCOV,
     +                              YLLC+XTMI*YCOV+YMLT*YTMI*XCOV,
     +                                                  XTMP,YTMP)
                  IF (ICFELL('PLCHMQ',6).NE.0) RETURN
                  XPEN=CUFX(XTMP)
                  IF (ICFELL('PLCHMQ',7).NE.0) RETURN
                  YPEN=CUFY(YTMP)
                  IF (ICFELL('PLCHMQ',8).NE.0) RETURN
                  CALL PLOTIF (XPEN,YPEN,IPEN)
                  IF (ICFELL('PLCHMQ',9).NE.0) RETURN
C
                ELSE
C
C An out-of-range value is defined; we have to cope with the fact that
C some points may disappear under the given mapping.
C
C The new point becomes the old point.
C
                  IVSO=IVSN
                  XOLD=XNEW
                  YOLD=YNEW
C
C Compute the coordinates and the visibility flag for the new point.
C
                  XNEW=XLLC+XTMI*XCOV-YMLT*YTMI*YCOV
                  YNEW=YLLC+XTMI*YCOV+YMLT*YTMI*XCOV
C
                  CALL PCMPXY (IMAP,XNEW,YNEW,XTMP,YTMP)
                  IF (ICFELL('PLCHMQ',10).NE.0) RETURN
C
                  IF (XTMP.EQ.OORV) THEN
                    IVSN=0
                  ELSE
                    IVSN=1
                  END IF
C
C Process the various combinations of old-point/new-point visibility.
C
                  IF (IVSO.EQ.0.AND.IVSN.NE.0) THEN
C
C The old point was invisible and the new one is visible.  If the line
C segment between them is supposed to be drawn, use a binary-halving
C process to find a point at the edge of the visible area and start
C drawing there.  In any case, leave the pen down at the position of
C the new point.
C
                    IF (IPEN.NE.0) THEN
                      XINV=XOLD
                      YINV=YOLD
                      XVIS=XNEW
                      YVIS=YNEW
                      DO 102 IHLF=1,64
                        XHLF=.5*(XINV+XVIS)
                        YHLF=.5*(YINV+YVIS)
                        CALL PCMPXY (IMAP,XHLF,YHLF,XTMP,YTMP)
                        IF (ICFELL('PLCHMQ',11).NE.0) RETURN
                        IF (XTMP.EQ.OORV) THEN
                          IF (XHLF.EQ.XINV.AND.YHLF.EQ.YINV) GO TO 103
                          XINV=XHLF
                          YINV=YHLF
                        ELSE
                          IF (XHLF.EQ.XVIS.AND.YHLF.EQ.YVIS) GO TO 103
                          XVIS=XHLF
                          YVIS=YHLF
                        END IF
  102                 CONTINUE
  103                 CALL PCMPXY (IMAP,XVIS,YVIS,XTMP,YTMP)
                      IF (ICFELL('PLCHMQ',12).NE.0) RETURN
                      XPEN=CUFX(XTMP)
                      IF (ICFELL('PLCHMQ',13).NE.0) RETURN
                      YPEN=CUFY(YTMP)
                      IF (ICFELL('PLCHMQ',14).NE.0) RETURN
                      CALL PLOTIF (XPEN,YPEN,0)
                    END IF
C
                    CALL PCMPXY (IMAP,XNEW,YNEW,XTMP,YTMP)
                    IF (ICFELL('PLCHMQ',15).NE.0) RETURN
                    XPEN=CUFX(XTMP)
                    IF (ICFELL('PLCHMQ',16).NE.0) RETURN
                    YPEN=CUFY(YTMP)
                    IF (ICFELL('PLCHMQ',17).NE.0) RETURN
                    CALL PLOTIF (XPEN,YPEN,IPEN)
                    IF (ICFELL('PLCHMQ',18).NE.0) RETURN
C
C Check for the next combination.
C
                  ELSE IF (IVSO.NE.0.AND.IVSN.EQ.0) THEN
C
C The old point was visible and the new one is not.  If the line segment
C between them is supposed to be drawn, use a binary-halving process to
C find out where the line segment disappears and extend it to there.
C
                    IF (IPEN.NE.0) THEN
                      XVIS=XOLD
                      YVIS=YOLD
                      XINV=XNEW
                      YINV=YNEW
                      DO 104 IHLF=1,64
                        XHLF=.5*(XINV+XVIS)
                        YHLF=.5*(YINV+YVIS)
                        CALL PCMPXY (IMAP,XHLF,YHLF,XTMP,YTMP)
                        IF (ICFELL('PLCHMQ',19).NE.0) RETURN
                        IF (XTMP.EQ.OORV) THEN
                          IF (XHLF.EQ.XINV.AND.YHLF.EQ.YINV) GO TO 105
                          XINV=XHLF
                          YINV=YHLF
                        ELSE
                          IF (XHLF.EQ.XVIS.AND.YHLF.EQ.YVIS) GO TO 105
                          XVIS=XHLF
                          YVIS=YHLF
                        END IF
  104                 CONTINUE
  105                 CALL PCMPXY (IMAP,XVIS,YVIS,XTMP,YTMP)
                      IF (ICFELL('PLCHMQ',20).NE.0) RETURN
                      XPEN=CUFX(XTMP)
                      IF (ICFELL('PLCHMQ',21).NE.0) RETURN
                      YPEN=CUFY(YTMP)
                      IF (ICFELL('PLCHMQ',22).NE.0) RETURN
                      CALL PLOTIF (XPEN,YPEN,1)
                      IF (ICFELL('PLCHMQ',23).NE.0) RETURN
                    END IF
C
C Check for the next combination.
C
                  ELSE IF (IVSO.NE.0.AND.IVSN.NE.0) THEN
C
C The old and new points are both visible.  An easy case.
C
                    CALL PCMPXY (IMAP,XNEW,YNEW,XTMP,YTMP)
                    IF (ICFELL('PLCHMQ',24).NE.0) RETURN
                    XPEN=CUFX(XTMP)
                    IF (ICFELL('PLCHMQ',25).NE.0) RETURN
                    YPEN=CUFY(YTMP)
                    IF (ICFELL('PLCHMQ',26).NE.0) RETURN
                    CALL PLOTIF (XPEN,YPEN,IPEN)
                    IF (ICFELL('PLCHMQ',27).NE.0) RETURN
C
C End of checks for different combinations.
C
                  END IF
C
C End of check for out-of-range value defined.
C
                END IF
C
C End of interpolation loop.
C
  106         CONTINUE
C
C End of processing of X/Y coordinate pair.
C
            END IF
C
C The pen goes down for the next point.
C
            IPEN=1
C
C Go back for the next digitization pair.
C
            GO TO 101
C
          ELSE
C
C Process an "op-code", which may either force the pen up or stop us.
C
            IF (KYCO(IPNT).NE.77) THEN
              IPEN=0
              GO TO 101
            END IF
C
          END IF
C
        END IF
C
C Move to the lower left-hand corner of the next character.
C
          XLLC=XLLC+60.*XCOV
          YLLC=YLLC+60.*YCOV
C
  107 CONTINUE
C
C Flush the buffer in PLOTIF.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('PLCHMQ',28).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
