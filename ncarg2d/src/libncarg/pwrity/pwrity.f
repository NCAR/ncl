C
C	$Id: pwrity.f,v 1.6 2008-07-27 00:17:21 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PWRITY (X,Y,ID,N,ISIZE,ITHETA,ICNT)
C
C LATEST REVISION        June, 1987
C
C PURPOSE                PWRITY is a character plotting routine.  It has
C                        some features not found in WTSTR, but is not as
C                        fancy as PWRITX.
C
C
C USAGE                  CALL PWRITY(X,Y,ID,N,ISIZE,ITHETA,ICNT)
C
C ARGUMENTS
C
C ON INPUT               X,Y
C                          Positioning coordinates for the characters to
C                          be drawn.  X and Y are user world coordinates
C                          and are scaled according to the current
C                          normalization transformation.  Also, see ICNT.
C
C                        ID
C                          Character string to be drawn.
C
C                        N
C                          The number of characters in ID.
C
C                         ISIZE
C                            Size of the character:
C                            . If between 0 and 3, ISIZE is chosen as
C                              1., 1.5, 2., or 3. times an 8 plotter
C                              address character width.
C                            . If greater than 3, ISIZE is the character
C                              width in plotter address units.
C
C                        ITHETA
C                          Angle, in degrees, at which the characters are
C                          plotted (counterclockwise from the positive
C                          X axis.)
C
C                        ICNT
C                          Centering option:
C                            = -1  (X,Y) is the center of the left edge
C                                  of the first character.
C                            =  0  (X,Y) is the center of the entire
C                                  string.
C                            =  1  (X,Y) is the center of the right edge
C                                  of the last character.
C
C ON OUTPUT              All arguments are unchanged.
C
C COMMON BLOCKS          PWRCOM
C
C REQUIRED LIBRARY       The SPPS.
C ROUTINES
C
C REQUIRED GKS LEVEL     0A
C
C I/O                    Plots characters.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN
C
C HISTORY                Implemented for use in DASHCHAR.
C                        Made portable in January 1977
C                        for use on computer systems which
C                        support plotters with up to 15 bits resolution.
C                        Converted to FORTRAN77 and GKS in July, 1984.
C
C ALGORITHM              Digitizations of the characters are stored
C                        iternally and adjusted according to X, Y,
C                        ISIZE and ICNT, then plotted.
C
C TIMING                 Slower than WTSTR, faster than PWRITX.
C
C PORTABILITY            FORTRAN
C
C
      SAVE
      CHARACTER*(*)   ID
      CHARACTER*1     JCHAR(46)  ,KCHAR
      DIMENSION       INDEX(46)  ,KX(494)    ,KY(494)
      COMMON /PWRCOM/ USABLE
      LOGICAL         USABLE
      LOGICAL         LENTRY
C
C THE FOLLOWING DATA STATEMENTS ASSOCIATE EACH CHARACTER WITH ITS
C DIGITIZATION.  THAT IS, THE DIGITIZATION FOR THE CHARACTER A STARTS
C AT KX(1) AND KY(1), WHILE B STARTS AT KX(13) AND KY(13), AND SO ON.
C
      DATA JCHAR( 1),INDEX( 1)/'A',  1/
      DATA JCHAR( 2),INDEX( 2)/'B', 13/
      DATA JCHAR( 3),INDEX( 3)/'C', 28/
      DATA JCHAR( 4),INDEX( 4)/'D', 40/
      DATA JCHAR( 5),INDEX( 5)/'E', 49/
      DATA JCHAR( 6),INDEX( 6)/'F', 60/
      DATA JCHAR( 7),INDEX( 7)/'G', 68/
      DATA JCHAR( 8),INDEX( 8)/'H', 82/
      DATA JCHAR( 9),INDEX( 9)/'I', 92/
      DATA JCHAR(10),INDEX(10)/'J',104/
      DATA JCHAR(11),INDEX(11)/'K',113/
      DATA JCHAR(12),INDEX(12)/'L',123/
      DATA JCHAR(13),INDEX(13)/'M',130/
      DATA JCHAR(14),INDEX(14)/'N',137/
      DATA JCHAR(15),INDEX(15)/'O',143/
      DATA JCHAR(16),INDEX(16)/'P',157/
      DATA JCHAR(17),INDEX(17)/'Q',166/
      DATA JCHAR(18),INDEX(18)/'R',182/
      DATA JCHAR(19),INDEX(19)/'S',194/
      DATA JCHAR(20),INDEX(20)/'T',210/
      DATA JCHAR(21),INDEX(21)/'U',219/
      DATA JCHAR(22),INDEX(22)/'V',229/
      DATA JCHAR(23),INDEX(23)/'W',236/
      DATA JCHAR(24),INDEX(24)/'X',245/
      DATA JCHAR(25),INDEX(25)/'Y',252/
      DATA JCHAR(26),INDEX(26)/'Z',262/
      DATA JCHAR(27),INDEX(27)/'0',273/
      DATA JCHAR(28),INDEX(28)/'1',286/
      DATA JCHAR(29),INDEX(29)/'2',296/
      DATA JCHAR(30),INDEX(30)/'3',308/
      DATA JCHAR(31),INDEX(31)/'4',326/
      DATA JCHAR(32),INDEX(32)/'5',339/
      DATA JCHAR(33),INDEX(33)/'6',352/
      DATA JCHAR(34),INDEX(34)/'7',368/
      DATA JCHAR(35),INDEX(35)/'8',378/
      DATA JCHAR(36),INDEX(36)/'9',398/
      DATA JCHAR(37),INDEX(37)/'+',414/
      DATA JCHAR(38),INDEX(38)/'-',423/
      DATA JCHAR(39),INDEX(39)/'*',429/
      DATA JCHAR(40),INDEX(40)/'/',444/
      DATA JCHAR(41),INDEX(41)/'(',448/
      DATA JCHAR(42),INDEX(42)/')',456/
      DATA JCHAR(43),INDEX(43)/'=',464/
      DATA JCHAR(44),INDEX(44)/' ',473/
      DATA JCHAR(45),INDEX(45)/',',476/
      DATA JCHAR(46),INDEX(46)/'.',486/
C
C THE FOLLOWING DATA STATEMENTS CONTAIN THE DIGITIZATIONS OF THE
C CHARACTERS.  THE CHARACTERS ARE DIGITIZED ON A BOX 6 UNITS WIDE AND
C 7 UNITS TALL.  THIS INCLUDES 2 UNITS OF WHITE SPACE TO THE RIGHT OF
C EACH CHARACTER.  IF KX=7, KY IS A FLAG -- KY=0 MEANS THE FOLLOWING
C KX AND KY ARE A PEN UP MOVE (ALL OTHERS ARE PEN DOWN MOVES), AND
C KY=7 MEANS THAT THE END OF THE DIGITIZATION FOR A PARTICULAR CHARAC-
C TER HAS BEEN REACHED.  (HLFW is half of width less white space - DJK)
C
      DATA WIDE,HIGH,HLFW/6.,7.,2./
C
      DATA KX(  1),KX(  2),KX(  3),KX(  4),KX(  5),KX(  6)/0,4,7,0,0,1/
      DATA KY(  1),KY(  2),KY(  3),KY(  4),KY(  5),KY(  6)/3,3,0,3,6,7/
      DATA KX(  7),KX(  8),KX(  9),KX( 10),KX( 11),KX( 12)/3,4,4,7,6,7/
      DATA KY(  7),KY(  8),KY(  9),KY( 10),KY( 11),KY( 12)/7,6,0,0,0,7/
      DATA KX( 13),KX( 14),KX( 15),KX( 16),KX( 17),KX( 18)/0,3,4,4,3,0/
      DATA KY( 13),KY( 14),KY( 15),KY( 16),KY( 17),KY( 18)/7,7,6,5,4,4/
      DATA KX( 19),KX( 20),KX( 21),KX( 22),KX( 23),KX( 24)/7,3,4,4,3,0/
      DATA KY( 19),KY( 20),KY( 21),KY( 22),KY( 23),KY( 24)/0,4,3,1,0,0/
      DATA KX( 25),KX( 26),KX( 27),KX( 28),KX( 29),KX( 30)/7,6,7,7,4,3/
      DATA KY( 25),KY( 26),KY( 27),KY( 28),KY( 29),KY( 30)/0,0,7,0,6,7/
      DATA KX( 31),KX( 32),KX( 33),KX( 34),KX( 35),KX( 36)/1,0,0,1,3,4/
      DATA KY( 31),KY( 32),KY( 33),KY( 34),KY( 35),KY( 36)/7,6,1,0,0,1/
      DATA KX( 37),KX( 38),KX( 39),KX( 40),KX( 41),KX( 42)/7,6,7,0,3,4/
      DATA KY( 37),KY( 38),KY( 39),KY( 40),KY( 41),KY( 42)/0,0,7,7,7,6/
      DATA KX( 43),KX( 44),KX( 45),KX( 46),KX( 47),KX( 48)/4,3,0,7,6,7/
      DATA KY( 43),KY( 44),KY( 45),KY( 46),KY( 47),KY( 48)/1,0,0,0,0,7/
      DATA KX( 49),KX( 50),KX( 51),KX( 52),KX( 53),KX( 54)/0,4,7,3,0,7/
      DATA KY( 49),KY( 50),KY( 51),KY( 52),KY( 53),KY( 54)/7,7,0,4,4,0/
      DATA KX( 55),KX( 56),KX( 57),KX( 58),KX( 59),KX( 60)/0,4,7,6,7,0/
      DATA KY( 55),KY( 56),KY( 57),KY( 58),KY( 59),KY( 60)/0,0,0,0,7,7/
      DATA KX( 61),KX( 62),KX( 63),KX( 64),KX( 65),KX( 66)/4,7,0,3,7,6/
      DATA KY( 61),KY( 62),KY( 63),KY( 64),KY( 65),KY( 66)/7,0,4,4,0,0/
      DATA KX( 67),KX( 68),KX( 69),KX( 70),KX( 71),KX( 72)/7,7,4,3,1,0/
      DATA KY( 67),KY( 68),KY( 69),KY( 70),KY( 71),KY( 72)/7,0,6,7,7,6/
      DATA KX( 73),KX( 74),KX( 75),KX( 76),KX( 77),KX( 78)/0,1,3,4,4,3/
      DATA KY( 73),KY( 74),KY( 75),KY( 76),KY( 77),KY( 78)/1,0,0,1,3,3/
      DATA KX( 79),KX( 80),KX( 81),KX( 82),KX( 83),KX( 84)/7,6,7,0,7,0/
      DATA KY( 79),KY( 80),KY( 81),KY( 82),KY( 83),KY( 84)/0,0,7,7,0,4/
      DATA KX( 85),KX( 86),KX( 87),KX( 88),KX( 89),KX( 90)/4,7,4,4,7,6/
      DATA KY( 85),KY( 86),KY( 87),KY( 88),KY( 89),KY( 90)/4,0,7,0,0,0/
      DATA KX( 91),KX( 92),KX( 93),KX( 94),KX( 95),KX( 96)/7,7,1,3,7,2/
      DATA KY( 91),KY( 92),KY( 93),KY( 94),KY( 95),KY( 96)/7,0,7,7,0,7/
      DATA KX( 97),KX( 98),KX( 99),KX(100),KX(101),KX(102)/2,7,1,3,7,6/
      DATA KY( 97),KY( 98),KY( 99),KY(100),KY(101),KY(102)/0,0,0,0,0,0/
      DATA KX(103),KX(104),KX(105),KX(106),KX(107),KX(108)/7,7,0,1,3,4/
      DATA KY(103),KY(104),KY(105),KY(106),KY(107),KY(108)/7,0,1,0,0,1/
      DATA KX(109),KX(110),KX(111),KX(112),KX(113),KX(114)/4,7,6,7,0,7/
      DATA KY(109),KY(110),KY(111),KY(112),KY(113),KY(114)/7,0,0,7,7,0/
      DATA KX(115),KX(116),KX(117),KX(118),KX(119),KX(120)/0,4,7,2,4,7/
      DATA KY(115),KY(116),KY(117),KY(118),KY(119),KY(120)/3,7,0,5,0,0/
      DATA KX(121),KX(122),KX(123),KX(124),KX(125),KX(126)/6,7,7,0,0,4/
      DATA KY(121),KY(122),KY(123),KY(124),KY(125),KY(126)/0,7,0,7,0,0/
      DATA KX(127),KX(128),KX(129),KX(130),KX(131),KX(132)/7,6,7,0,2,4/
      DATA KY(127),KY(128),KY(129),KY(130),KY(131),KY(132)/0,0,7,7,3,7/
      DATA KX(133),KX(134),KX(135),KX(136),KX(137),KX(138)/4,7,6,7,0,4/
      DATA KY(133),KY(134),KY(135),KY(136),KY(137),KY(138)/0,0,0,7,7,0/
      DATA KX(139),KX(140),KX(141),KX(142),KX(143),KX(144)/4,7,6,7,4,7/
      DATA KY(139),KY(140),KY(141),KY(142),KY(143),KY(144)/7,0,0,7,7,0/
      DATA KX(145),KX(146),KX(147),KX(148),KX(149),KX(150)/4,4,3,1,0,0/
      DATA KY(145),KY(146),KY(147),KY(148),KY(149),KY(150)/1,6,7,7,6,1/
      DATA KX(151),KX(152),KX(153),KX(154),KX(155),KX(156)/1,3,4,7,6,7/
      DATA KY(151),KY(152),KY(153),KY(154),KY(155),KY(156)/0,0,1,0,0,7/
      DATA KX(157),KX(158),KX(159),KX(160),KX(161),KX(162)/0,3,4,4,3,0/
      DATA KY(157),KY(158),KY(159),KY(160),KY(161),KY(162)/7,7,6,5,4,4/
      DATA KX(163),KX(164),KX(165),KX(166),KX(167),KX(168)/7,6,7,7,0,0/
      DATA KY(163),KY(164),KY(165),KY(166),KY(167),KY(168)/0,0,7,0,1,6/
      DATA KX(169),KX(170),KX(171),KX(172),KX(173),KX(174)/1,3,4,4,3,1/
      DATA KY(169),KY(170),KY(171),KY(172),KY(173),KY(174)/7,7,6,1,0,0/
      DATA KX(175),KX(176),KX(177),KX(178),KX(179),KX(180)/0,7,2,4,7,6/
      DATA KY(175),KY(176),KY(177),KY(178),KY(179),KY(180)/1,0,2,0,0,0/
      DATA KX(181),KX(182),KX(183),KX(184),KX(185),KX(186)/7,0,3,4,4,3/
      DATA KY(181),KY(182),KY(183),KY(184),KY(185),KY(186)/7,7,7,6,5,4/
      DATA KX(187),KX(188),KX(189),KX(190),KX(191),KX(192)/0,7,2,4,7,6/
      DATA KY(187),KY(188),KY(189),KY(190),KY(191),KY(192)/4,0,4,0,0,0/
      DATA KX(193),KX(194),KX(195),KX(196),KX(197),KX(198)/7,7,0,1,3,4/
      DATA KY(193),KY(194),KY(195),KY(196),KY(197),KY(198)/7,0,1,0,0,1/
      DATA KX(199),KX(200),KX(201),KX(202),KX(203),KX(204)/4,3,1,0,0,1/
      DATA KY(199),KY(200),KY(201),KY(202),KY(203),KY(204)/3,4,4,5,6,7/
      DATA KX(205),KX(206),KX(207),KX(208),KX(209),KX(210)/3,4,7,6,7,7/
      DATA KY(205),KY(206),KY(207),KY(208),KY(209),KY(210)/7,6,0,0,7,0/
      DATA KX(211),KX(212),KX(213),KX(214),KX(215),KX(216)/0,4,7,2,2,7/
      DATA KY(211),KY(212),KY(213),KY(214),KY(215),KY(216)/7,7,0,7,0,0/
      DATA KX(217),KX(218),KX(219),KX(220),KX(221),KX(222)/6,7,7,0,0,1/
      DATA KY(217),KY(218),KY(219),KY(220),KY(221),KY(222)/0,7,0,7,1,0/
      DATA KX(223),KX(224),KX(225),KX(226),KX(227),KX(228)/3,4,4,7,6,7/
      DATA KY(223),KY(224),KY(225),KY(226),KY(227),KY(228)/0,1,7,0,0,7/
      DATA KX(229),KX(230),KX(231),KX(232),KX(233),KX(234)/7,0,2,4,7,6/
      DATA KY(229),KY(230),KY(231),KY(232),KY(233),KY(234)/0,7,0,7,0,0/
      DATA KX(235),KX(236),KX(237),KX(238),KX(239),KX(240)/7,7,0,0,2,4/
      DATA KY(235),KY(236),KY(237),KY(238),KY(239),KY(240)/7,0,7,0,4,0/
      DATA KX(241),KX(242),KX(243),KX(244),KX(245),KX(246)/4,7,6,7,4,7/
      DATA KY(241),KY(242),KY(243),KY(244),KY(245),KY(246)/7,0,0,7,7,0/
      DATA KX(247),KX(248),KX(249),KX(250),KX(251),KX(252)/0,4,7,6,7,7/
      DATA KY(247),KY(248),KY(249),KY(250),KY(251),KY(252)/7,0,0,0,7,0/
      DATA KX(253),KX(254),KX(255),KX(256),KX(257),KX(258)/0,2,4,7,2,2/
      DATA KY(253),KY(254),KY(255),KY(256),KY(257),KY(258)/7,4,7,0,4,0/
      DATA KX(259),KX(260),KX(261),KX(262),KX(263),KX(264)/7,6,7,7,3,1/
      DATA KY(259),KY(260),KY(261),KY(262),KY(263),KY(264)/0,0,7,0,4,4/
      DATA KX(265),KX(266),KX(267),KX(268),KX(269),KX(270)/7,0,4,0,4,7/
      DATA KY(265),KY(266),KY(267),KY(268),KY(269),KY(270)/0,7,7,0,0,0/
      DATA KX(271),KX(272),KX(273),KX(274),KX(275),KX(276)/6,7,7,4,3,1/
      DATA KY(271),KY(272),KY(273),KY(274),KY(275),KY(276)/0,7,0,1,0,0/
      DATA KX(277),KX(278),KX(279),KX(280),KX(281),KX(282)/0,0,1,3,4,4/
      DATA KY(277),KY(278),KY(279),KY(280),KY(281),KY(282)/1,6,7,7,6,1/
      DATA KX(283),KX(284),KX(285),KX(286),KX(287),KX(288)/7,6,7,7,1,2/
      DATA KY(283),KY(284),KY(285),KY(286),KY(287),KY(288)/0,0,7,0,6,7/
      DATA KX(289),KX(290),KX(291),KX(292),KX(293),KX(294)/2,7,1,3,7,6/
      DATA KY(289),KY(290),KY(291),KY(292),KY(293),KY(294)/0,0,0,0,0,0/
      DATA KX(295),KX(296),KX(297),KX(298),KX(299),KX(300)/7,7,0,1,3,4/
      DATA KY(295),KY(296),KY(297),KY(298),KY(299),KY(300)/7,0,6,7,7,6/
      DATA KX(301),KX(302),KX(303),KX(304),KX(305),KX(306)/4,0,0,4,7,6/
      DATA KY(301),KY(302),KY(303),KY(304),KY(305),KY(306)/5,1,0,0,0,0/
      DATA KX(307),KX(308),KX(309),KX(310),KX(311),KX(312)/7,7,0,1,3,4/
      DATA KY(307),KY(308),KY(309),KY(310),KY(311),KY(312)/7,0,6,7,7,6/
      DATA KX(313),KX(314),KX(315),KX(316),KX(317),KX(318)/4,3,1,7,3,4/
      DATA KY(313),KY(314),KY(315),KY(316),KY(317),KY(318)/5,4,4,0,4,3/
      DATA KX(319),KX(320),KX(321),KX(322),KX(323),KX(324)/4,3,1,0,7,6/
      DATA KY(319),KY(320),KY(321),KY(322),KY(323),KY(324)/1,0,0,1,0,0/
      DATA KX(325),KX(326),KX(327),KX(328),KX(329),KX(330)/7,7,3,3,2,0/
      DATA KY(325),KY(326),KY(327),KY(328),KY(329),KY(330)/7,0,0,7,7,4/
      DATA KX(331),KX(332),KX(333),KX(334),KX(335),KX(336)/0,4,7,2,4,7/
      DATA KY(331),KY(332),KY(333),KY(334),KY(335),KY(336)/3,3,0,0,0,0/
      DATA KX(337),KX(338),KX(339),KX(340),KX(341),KX(342)/6,7,7,0,1,3/
      DATA KY(337),KY(338),KY(339),KY(340),KY(341),KY(342)/0,7,0,1,0,0/
      DATA KX(343),KX(344),KX(345),KX(346),KX(347),KX(348)/4,4,3,0,0,4/
      DATA KY(343),KY(344),KY(345),KY(346),KY(347),KY(348)/1,3,4,4,7,7/
      DATA KX(349),KX(350),KX(351),KX(352),KX(353),KX(354)/7,6,7,7,4,3/
      DATA KY(349),KY(350),KY(351),KY(352),KY(353),KY(354)/0,0,7,0,6,7/
      DATA KX(355),KX(356),KX(357),KX(358),KX(359),KX(360)/1,0,0,1,3,4/
      DATA KY(355),KY(356),KY(357),KY(358),KY(359),KY(360)/7,6,1,0,0,1/
      DATA KX(361),KX(362),KX(363),KX(364),KX(365),KX(366)/4,3,1,0,7,6/
      DATA KY(361),KY(362),KY(363),KY(364),KY(365),KY(366)/3,4,4,3,0,0/
      DATA KX(367),KX(368),KX(369),KX(370),KX(371),KX(372)/7,7,0,0,4,4/
      DATA KY(367),KY(368),KY(369),KY(370),KY(371),KY(372)/7,0,6,7,7,6/
      DATA KX(373),KX(374),KX(375),KX(376),KX(377),KX(378)/2,2,7,6,7,7/
      DATA KY(373),KY(374),KY(375),KY(376),KY(377),KY(378)/1,0,0,0,7,0/
      DATA KX(379),KX(380),KX(381),KX(382),KX(383),KX(384)/1,0,0,1,3,4/
      DATA KY(379),KY(380),KY(381),KY(382),KY(383),KY(384)/4,5,6,7,7,6/
      DATA KX(385),KX(386),KX(387),KX(388),KX(389),KX(390)/4,3,1,0,0,1/
      DATA KY(385),KY(386),KY(387),KY(388),KY(389),KY(390)/5,4,4,3,1,0/
      DATA KX(391),KX(392),KX(393),KX(394),KX(395),KX(396)/3,4,4,3,7,6/
      DATA KY(391),KY(392),KY(393),KY(394),KY(395),KY(396)/0,1,3,4,0,0/
      DATA KX(397),KX(398),KX(399),KX(400),KX(401),KX(402)/7,7,0,1,3,4/
      DATA KY(397),KY(398),KY(399),KY(400),KY(401),KY(402)/7,0,1,0,0,1/
      DATA KX(403),KX(404),KX(405),KX(406),KX(407),KX(408)/4,3,1,0,0,1/
      DATA KY(403),KY(404),KY(405),KY(406),KY(407),KY(408)/6,7,7,6,4,3/
      DATA KX(409),KX(410),KX(411),KX(412),KX(413),KX(414)/3,4,7,6,7,7/
      DATA KY(409),KY(410),KY(411),KY(412),KY(413),KY(414)/3,4,0,0,7,0/
      DATA KX(415),KX(416),KX(417),KX(418),KX(419),KX(420)/0,4,7,2,2,7/
      DATA KY(415),KY(416),KY(417),KY(418),KY(419),KY(420)/3,3,0,5,1,0/
      DATA KX(421),KX(422),KX(423),KX(424),KX(425),KX(426)/6,7,7,0,4,7/
      DATA KY(421),KY(422),KY(423),KY(424),KY(425),KY(426)/0,7,0,3,3,0/
      DATA KX(427),KX(428),KX(429),KX(430),KX(431),KX(432)/6,7,7,0,4,7/
      DATA KY(427),KY(428),KY(429),KY(430),KY(431),KY(432)/0,7,0,1,5,0/
      DATA KX(433),KX(434),KX(435),KX(436),KX(437),KX(438)/2,2,7,4,0,7/
      DATA KY(433),KY(434),KY(435),KY(436),KY(437),KY(438)/5,1,0,3,3,0/
      DATA KX(439),KX(440),KX(441),KX(442),KX(443),KX(444)/0,4,7,6,7,4/
      DATA KY(439),KY(440),KY(441),KY(442),KY(443),KY(444)/5,1,0,0,7,7/
      DATA KX(445),KX(446),KX(447),KX(448),KX(449),KX(450)/7,6,7,7,3,2/
      DATA KY(445),KY(446),KY(447),KY(448),KY(449),KY(450)/0,0,7,1,7,6/
      DATA KX(451),KX(452),KX(453),KX(454),KX(455),KX(456)/2,3,7,6,7,7/
      DATA KY(451),KY(452),KY(453),KY(454),KY(455),KY(456)/1,0,0,0,7,0/
      DATA KX(457),KX(458),KX(459),KX(460),KX(461),KX(462)/1,2,2,1,7,6/
      DATA KY(457),KY(458),KY(459),KY(460),KY(461),KY(462)/7,6,1,0,0,0/
      DATA KX(463),KX(464),KX(465),KX(466),KX(467),KX(468)/7,7,4,0,7,0/
      DATA KY(463),KY(464),KY(465),KY(466),KY(467),KY(468)/7,0,5,5,0,2/
      DATA KX(469),KX(470),KX(471),KX(472),KX(473),KX(474)/4,7,6,7,7,6/
      DATA KY(469),KY(470),KY(471),KY(472),KY(473),KY(474)/2,0,0,7,0,0/
      DATA KX(475),KX(476),KX(477),KX(478),KX(479),KX(480)/7,7,1,2,2,1/
      DATA KY(475),KY(476),KY(477),KY(478),KY(479),KY(480)/7,0,0,1,2,2/
      DATA KX(481),KX(482),KX(483),KX(484),KX(485),KX(486)/1,2,7,6,7,7/
      DATA KY(481),KY(482),KY(483),KY(484),KY(485),KY(486)/1,1,0,0,7,0/
      DATA KX(487),KX(488),KX(489),KX(490),KX(491),KX(492)/2,1,1,2,2,7/
      DATA KY(487),KY(488),KY(489),KY(490),KY(491),KY(492)/0,0,1,1,0,0/
      DATA KX(493),KX(494)                                /6,7        /
      DATA KY(493),KY(494)                                /0,7        /
C
C NSIZE IS THE LENGTH OF JCHAR AND INDEX.
C LEN IS THE LENGTH OF KX AND KY.
C LENTRY TELLS IF THIS IS THE FIRTST CALL TO PWRITY.
C LRES IS THE NUMBER OF BITS OF ACCURACY USED FOR INTEGER INPUT TO
C THE SYSTEM PLOT PACKAGE.
C
      DATA NSIZE/46/
      DATA LEN/494/
      DATA LENTRY/.FALSE./
      DATA LRES/15/
      DATA DEGRAD/0.017453293/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PWRYBD
C
      IF (USABLE) GO TO 101
C
C THIS IS A PWRITY CALL
C
      CALL Q8QST4 ('GRAPHX','PWRITY','PWRITY','VERSION  1')
  101 USABLE = .FALSE.
C
C SEE IF THIS IS THE FIRST CALL TO PWRITY.
C
      IF (LENTRY) GO TO 103
C
C MARK THAT FUTURE CALLS NEED NOT DO THIS CODE.
C
      LENTRY = .TRUE.
C
C RECORD THE LOCATION OF THE BLANK SO IT CAN BE USED FOR UNKNOWN
C CHARACTERS.
C
      IBLKPT = INDEX(44)
C
C SORT JCHAR MAINTAINING THE RELATIONSHIP BETWEEN JCHAR AND INDEX.
C (THAT IS, IF JCHAR(I)='B', THEN INDEX(I)=13 FROM THE ABOVE DATA STMT.)
C THIS WILL ENABLE CHARACTERS TO BE QUICKLY FOUND IN ALL SUBSEQUENT
C CALLS TO PWRITY.
C
      CALL PWRYSO (JCHAR,INDEX,NSIZE)
C
C ALL ONE-TIME INITIALIZATION NOW FINISHED.
C
C TRANSFORM THE INPUT COORDINATES TO INTEGER SPACE.
C
  103 CALL FL2INT (X,Y,IX,IY)
C
      NN = N
      IF (NN .LE. 0) GO TO 113
      FNNM1 = NN-1
      JCNT = ICNT
C
C GET USER SET RESOLUTION.
C
      CALL GETUSV ('XF',LXSAVE)
      CALL GETUSV ('YF',LYSAVE)
C
C PUT RELATIVE SIZE IN Q.
C
      Q = ISIZE
      IF (Q .LE. 3.) GO TO 104
      Q = Q/REAL(ISHIFT(6,LXSAVE-10))
      GO TO 105
  104 Q = (1.+.5*(REAL(INT(Q)+INT(Q)/3)))*4./3.
  105 Q = Q*REAL(ISHIFT(1,LRES-10))
C
C CALCULATE COMBINED TRANSFORMATION.
C
      THETA = REAL(ITHETA)*DEGRAD
      CT = Q*COS(THETA)
      ST = Q*SIN(THETA)
C
C FIND PLOTTER ADDRESS COORDINATES FOR BEGINNING.
C
      XC = IX
      YC = IY
C
C CORRECT FOR CHARACTER DATA BEING LOWER-LEFT-HAND POSITIONED.
C
      XC = XC-HLFW*CT+HIGH*.5*ST
      YC = YC-HLFW*ST-HIGH*.5*CT
C
C CORRECT FOR CENTERING IF TURNED ON.
C
      JCENT = MAX(-1,MIN(1,JCNT))+2
      GO TO (107,106,108),JCENT
  106 XC = XC-CT*FNNM1*WIDE*.5
      YC = YC-ST*FNNM1*WIDE*.5
      GO TO 109
  107 XC = XC+CT*HLFW
      YC = YC+ST*HLFW
      GO TO 109
  108 XC = XC-CT*HLFW
      YC = YC-ST*HLFW
      XC = XC-CT*FNNM1*WIDE
      YC = YC-ST*FNNM1*WIDE
C
C SET PLOTTER TO STARTING POINT.
C
  109 CALL PLOTIT (INT(XC),INT(YC),0)
C
C PLOT ALL THE CHARACTERS IN THE INPUT STRING.
C
      DO 112 K=1,NN
         YB = YC
         XB = XC
         IP = 1
C
C EXTRACT CHARACTER NUMBER K FROM THE STRING.
C
         KCHAR = ID(K:K)
C
C FIND THE TABLE ENTRY.
C
         CALL PWRYGT (KCHAR,JCHAR,INDEX,NSIZE,IPOINT)
         IF (IPOINT .EQ. -1) IPOINT = IBLKPT
C
C DRAW INDIVIDUAL CHARACTER.
C
         L = 0
  110    ISUB = IPOINT+L
         NX = KX(ISUB)
         FNX = NX
         NY = KY(ISUB)
         FNY = NY
         L = L+1
C
C TEST FOR OP-CODE OR DX AND DY.
C
         IF (NX .NE. 7) GO TO 111
C
C OP-CODE
C
         IP = 0
         IF (NY-7) 110,112,110
C
C DX AND DY
C
  111    XC = XB+FNX*CT-FNY*ST
         YC = YB+FNX*ST+FNY*CT
C
C CALL PLOTTING ROUTINE. MODE DETERMINED BY OP-CODE.
C
         CALL PLOTIT (INT(XC+.5),INT(YC+.5),IP)
         IP = 1
         GO TO 110
  112 CONTINUE
C
  113 CONTINUE
C
C     FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
      END
