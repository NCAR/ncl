C
C	$Id: tplot.f,v 1.1.1.1 1992-04-17 22:34:35 ncargd Exp $
C
      PROGRAM TPLOT
C
C     This program produces a small plotting instruction stream.
C     The primary purpose of this program is to test a local
C     implementation of the NCAR GKS 0A package, including PLOTIT
C     of SPPS.  Only a trivial subset of the package is used in
C     this minimal test.  Execution of this program using NCAR's
C     GKS package will produce a CGM.  Hex and octal dumps of this
C     CGM follow:
C
C
C In HEX:
C
C    Record 1:
C        0225 3400 003f 003d 3c20 2020 2020 2020
C        2020 2020 2020 2020 2020 2020 2020 2020
C        2020 2020 2020 2020 2020 2020 2020 2020
C        2020 2020 2020 2020 2020 2020 2020 2020
C        2020 2020 2000 1022 0001 1059 184e 4341
C        525f 474b 5330 412d 2d56 4552 5349 4f4e
C        5f33 2e30 3000 1166 0001 ffff 0000 11bf
C        01b7 0744 4546 4155 4c54 1a48 4552 5348
C        4559 3a43 4152 544f 4752 4150 4849 435f
C        524f 4d41 4e1a 4845 5253 4845 593a 4341
C        5254 4f47 5241 5048 4943 5f47 5245 454b
C        1548 4552 5348 4559 3a53 494d 504c 4558
C        5f52 4f4d 414e 1548 4552 5348 4559 3a53
C        494d 504c 4558 5f47 5245 454b 1648 4552
C        5348 4559 3a53 494d 504c 4558 5f53 4352
C        4950 5415 4845 5253 4845 593a 434f 4d50
C        4c45 585f 524f 4d41 4e15 4845 5253 4845
C        593a 434f 4d50 4c45 585f 4752 4545 4b16
C        4845 5253 4845 593a 434f 4d50 4c45 585f
C        5343 5249 5054 1648 4552 5348 4559 3a43
C        4f4d 504c 4558 5f49 5441 4c49 4318 4845
C        5253 4845 593a 434f 4d50 4c45 585f 4359
C        5249 4c4c 4943 1448 4552 5348 4559 3a44
C        5550 4c45 585f 524f 4d41 4e15 4845 5253
C        4845 593a 5452 4950 4c45 585f 524f 4d41
C        4e16 4845 5253 4845 593a 5452 4950 4c45
C        585f 4954 414c 4943 1548 4552 5348 4559
C        3a47 4f54 4849 435f 4745 524d 414e 1648
C        4552 5348 4559 3a47 4f54 4849 435f 454e
C        474c 4953 4816 4845 5253 4845 593a 474f
C        5448 4943 5f49 5441 4c49 414e 1448 4552
C        5348 4559 3a4d 4154 485f 5359 4d42 4f4c
C        5313 4845 5253 4845 593a 5359 4d42 4f4c
C        5f53 4554 3113 4845 5253 4845 593a 5359
C        4d42 4f4c 5f53 4554 3200 0000 0000 0000
C        0000 0000 0000 0000 0000 0000 0000 0000
C
C          .
C          .
C          .
C
C        (53 rows of zeros as in the above line.)
C
C          .
C          .
C          .
C
C        0000 0000 0000 0000 0000 0000 0000 0000
C
C
C    Record 2:
C        00a6 3800 007f 003d 3c20 2020 2020 2020
C        2020 2020 2020 2020 2020 2020 2020 2020
C        2020 2020 2020 2020 2020 2020 2020 2020
C        2020 2020 2020 2020 2020 2020 2020 2020
C        2020 2020 2000 0080 5444 0000 0000 5444
C        01cc cccc 30c2 0001 4034 0000 0000 7fff
C        0000 7fff 7fff 0000 7fff 0000 0000 524c
C        0002 0003 0000 0000 0000 0000 51e2 0300
C        5208 0000 0300 0300 0000 409c 3fff 3fff
C        0000 154e 4341 5220 474b 5320 5061 636b
C        6167 6520 5465 7374 00a0 0000 0000 0000
C        0000 0000 0000 0000 0000 0000 0000 0000
C
C          .
C          .
C          .
C
C        (77 rows of zeros as in the previous line.)
C
C          .
C          .
C          .
C
C        0000 0000 0000 0000 0000 0000 0000 0000
C
C Only the first 170 bytes of record 2 are significant, and there
C may be non-zero values in the remaining 1330 bytes of that
C record due to the fact that the metafile output buffer has
C not been cleared from previous writes (clearing is not
C necessary since the record contains a count of how many
C bytes are significant in the record.)
C
C    Record 3:
C        0002 3200 0040 0000 0000 0000 0000 0000
C        0000 0000 0000 0000 0000 0000 0000 0000
C
C          .
C          .
C          .
C
C        (87 rows of zeros as in the previous line.)
C
C          .
C          .
C          .
C
C        0000 0000 0000 0000 0000 0000 0000 0000
C
C Only the first 6 bytes of record 3 are significant, and there
C may be non-zero values in the remaining 1434 bytes of that
C record due to the fact that the metafile output buffer has
C not been cleared from previous writes (clearing is not
C necessary since the record contains a count of how many
C bytes are significant in the record.)
C
C
C In OCTAL bytes :
C
C    Record 1:
C        002 045 064 000 000 077 000 075 074 040 040 040 040 040 040 040
C        040 040 040 040 040 040 040 040 040 040 040 040 040 040 040 040
C        040 040 040 040 040 040 040 040 040 040 040 040 040 040 040 040
C        040 040 040 040 040 040 040 040 040 040 040 040 040 040 040 040
C        040 040 040 040 040 000 020 042 000 001 020 131 030 116 103 101
C        122 137 107 113 123 060 101 055 055 126 105 122 123 111 117 116
C        137 063 056 060 060 000 021 146 000 001 377 377 000 000 021 277
C        001 267 007 104 105 106 101 125 114 124 032 110 105 122 123 110
C        105 131 072 103 101 122 124 117 107 122 101 120 110 111 103 137
C        122 117 115 101 116 032 110 105 122 123 110 105 131 072 103 101
C        122 124 117 107 122 101 120 110 111 103 137 107 122 105 105 113
C        025 110 105 122 123 110 105 131 072 123 111 115 120 114 105 130
C        137 122 117 115 101 116 025 110 105 122 123 110 105 131 072 123
C        111 115 120 114 105 130 137 107 122 105 105 113 026 110 105 122
C        123 110 105 131 072 123 111 115 120 114 105 130 137 123 103 122
C        111 120 124 025 110 105 122 123 110 105 131 072 103 117 115 120
C        114 105 130 137 122 117 115 101 116 025 110 105 122 123 110 105
C        131 072 103 117 115 120 114 105 130 137 107 122 105 105 113 026
C        110 105 122 123 110 105 131 072 103 117 115 120 114 105 130 137
C        123 103 122 111 120 124 026 110 105 122 123 110 105 131 072 103
C        117 115 120 114 105 130 137 111 124 101 114 111 103 030 110 105
C        122 123 110 105 131 072 103 117 115 120 114 105 130 137 103 131
C        122 111 114 114 111 103 024 110 105 122 123 110 105 131 072 104
C        125 120 114 105 130 137 122 117 115 101 116 025 110 105 122 123
C        110 105 131 072 124 122 111 120 114 105 130 137 122 117 115 101
C        116 026 110 105 122 123 110 105 131 072 124 122 111 120 114 105
C        130 137 111 124 101 114 111 103 025 110 105 122 123 110 105 131
C        072 107 117 124 110 111 103 137 107 105 122 115 101 116 026 110
C        105 122 123 110 105 131 072 107 117 124 110 111 103 137 105 116
C        107 114 111 123 110 026 110 105 122 123 110 105 131 072 107 117
C        124 110 111 103 137 111 124 101 114 111 101 116 024 110 105 122
C        123 110 105 131 072 115 101 124 110 137 123 131 115 102 117 114
C        123 023 110 105 122 123 110 105 131 072 123 131 115 102 117 114
C        137 123 105 124 061 023 110 105 122 123 110 105 131 072 123 131
C        115 102 117 114 137 123 105 124 062 000 000 000 000 000 000 000
C        000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000
C
C          .
C          .
C          .
C
C        (53 rows of zeros as in the above line.)
C
C          .
C          .
C          .
C
C        000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000
C
C
C    Record 2:
C        000 246 070 000 000 177 000 075 074 040 040 040 040 040 040 040
C        040 040 040 040 040 040 040 040 040 040 040 040 040 040 040 040
C        040 040 040 040 040 040 040 040 040 040 040 040 040 040 040 040
C        040 040 040 040 040 040 040 040 040 040 040 040 040 040 040 040
C        040 040 040 040 040 000 000 200 124 104 000 000 000 000 124 104
C        001 314 314 314 060 302 000 001 100 064 000 000 000 000 177 377
C        000 000 177 377 177 377 000 000 177 377 000 000 000 000 122 114
C        000 002 000 003 000 000 000 000 000 000 000 000 121 342 003 000
C        122 010 000 000 003 000 003 000 000 000 100 234 077 377 077 377
C        000 000 025 116 103 101 122 040 107 113 123 040 120 141 143 153
C        141 147 145 040 124 145 163 164 000 240 000 000 000 000 000 000
C        000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000
C
C          .
C          .
C          .
C
C        (77 rows of zeros as in the previous line.)
C
C          .
C          .
C          .
C
C        000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000
C
C Only the first 170 bytes of record 2 are significant, and there
C may be non-zero values in the remaining 1330 bytes of that
C record due to the fact that the metafile output buffer has
C not been cleared from previous writes (clearing is not
C necessary since the record contains a count of how many
C bytes are significant in the record.)
C
C    Record 3:
C        000 002 062 000 000 100 000 000 000 000 000 000 000 000 000 000
C        000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000
C
C          .
C          .
C          .
C
C        (87 rows of zeros as in the previous line.)
C
C          .
C          .
C          .
C
C        000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000
C
C Only the first 6 bytes of record 3 are significant, and there
C may be non-zero values in the remaining 1434 bytes of that
C record due to the fact that the metafile output buffer has
C not been cleared from previous writes (clearing is not
C necessary since the record contains a count of how many
C bytes are significant in the record.)
C
      DATA N/32767/
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,ISIZ)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C     TRIVIAL FRAME
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLOTIT (0,0,0)
      CALL PLOTIT (N,0,1)
      CALL PLOTIT (N,N,1)
      CALL PLOTIT (0,N,1)
      CALL PLOTIT (0,0,1)
      CALL WTSTR(.5,.5,'NCAR GKS Package Test',3,0,0)
      CALL FRAME
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
