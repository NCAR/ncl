C
C	$Id: hfinit.f,v 1.5 2008-07-27 00:17:14 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE HFINIT
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA HFINITX
C
      COMMON /HAFTO3/ XLT        ,YBT        ,SIDE       ,EXT        ,
     1                IOFFM      ,ALPH       ,MXLEV      ,NCRTG      ,
     2                NCRTF      ,IL(135)
      COMMON /HAFTO4/ NPTMAX     ,NPOINT     ,XPNT(50)  ,YPNT(50)
      SAVE
C
C  INITIALIZATION OF INTERNAL PARAMETERS
C
      DATA   XLT,  YBT,SIDE,EXT,IOFFM,ALPH,MXLEV,NCRTG,NCRTF/
     1     0.102,0.102,.805,.25,    0, 1.6,   16,    8, 1024/
      DATA IL(1),IL(2),IL(3),IL(4),IL(5),IL(6),IL(7),IL(8),IL(9),IL(10),
     1IL(11),IL(12),IL(13),IL(14),IL(15),IL(16),IL(17),IL(18),IL(19),
     2IL(20),IL(21),IL(22),IL(23),IL(24),IL(25),IL(26),IL(27),IL(28),
     3IL(29),IL(30),IL(31),IL(32),IL(33),IL(34),IL(35),IL(36),IL(37),
     4IL(38),IL(39),IL(40),IL(41),IL(42),IL(43),IL(44)/
     5    5,11,
     6    4, 8,12,
     7    3, 6,10,13,
     8    2, 5, 8,11,14,
     9    1, 4, 7, 9,12,15,
     +    1, 4, 6, 8,10,12,15,
     1    1, 3, 5, 7, 9,11,13,15,
     2     1, 3, 4, 6, 8, 10, 12, 13, 15/
      DATA IL(45),IL(46),
     1IL(47),IL(48),IL(49),IL(50),IL(51),IL(52),IL(53),IL(54),IL(55),
     2IL(56),IL(57),IL(58),IL(59),IL(60),IL(61),IL(62),IL(63),IL(64),
     3IL(65),IL(66),IL(67),IL(68),IL(69),IL(70),IL(71),IL(72),IL(73),
     4IL(74),IL(75),IL(76),IL(77),IL(78),IL(79),IL(80),IL(81),IL(82),
     5IL(83),IL(84),IL(85),IL(86),IL(87),IL(88),IL(89),IL(90)/
     6    1, 3, 4, 6, 7, 9,10,12,13,15,
     7    1, 2, 3, 5, 6, 8,10,11,13,14,15,
     8    1, 2, 3, 5, 6, 7, 9,10,11,13,14,15,
     9    1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15/
      DATA IL(91),
     1IL(92),IL(93),IL(94),IL(95),IL(96),IL(97),IL(98),IL(99),IL(100),
     2IL(101),IL(102),IL(103),IL(104),IL(105),IL(106),IL(107),IL(108),
     3IL(109),IL(110),IL(111),IL(112),IL(113),IL(114),IL(115),IL(116),
     4IL(117),IL(118),IL(119),IL(120),IL(121),IL(122),IL(123),IL(124),
     5IL(125),IL(126),IL(127),IL(128),IL(129),IL(130),IL(131),IL(132),
     6IL(133),IL(134),IL(135)/
     7    1, 2, 3, 4, 5, 6, 7, 9,10,11,12,13,14,15,
     8    1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,
     9    0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15/
C
C SIZE OF THE COORDINATE BUFFERING ARRAYS FOR POINTS BUFFERING.
      DATA  NPTMAX/50/
C
C-----------------------------------------------------------------------
C
C REVISION HISTORY---
C
C MARCH    1990    FIXED HANDLING OF SET CALLS
C
C JULY     1984    CONVERTED TO FORTAN 77 AND GKS
C
C MARCH    1983    INSTITUTED BUFFERING OF POINTS WITHIN ROUTINE GRAY,
C                  WHICH DRAMATICALLY REDUCES SIZE OF OUTPUT PLOT CODE,
C                  METACODE.  THIS IN TURN GENERALLY IMPROVES THROUGHPUT
C                  OF METACODE INTERPRETERS.
C
C FEBRUARY 1979    MODIFIED CODE TO CONFORM TO FORTRAN 66 STANDARD
C
C JANUARY  1978    DELETED REFERENCES TO THE  *COSY  CARDS AND
C                  ADDED REVISION HISTORY
C
C-----------------------------------------------------------------------
C
      END
