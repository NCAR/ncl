C
C	$Id: gzusat.f,v 1.4 2008-07-27 00:21:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GZUSAT(SRFLG,CICNTX,CFCNTX)
C
C  Save/restore/set the attribute context in support of the
C  user entry NGSRAT.  This entry cannot be merged with GZSRAT
C  since the save/restore commands might be out of sync.
C
C    If SRFLG = 0, then save the current context
C    If SRFLG = 1, then restore the context from the most recent
C                  saved state (or set it to the default state if
C                  the state has not been saved before).
C    If SRFLG = 2, then return the current context in the output arrays
C                  CICNTX and CFCNTX.
C    If SRFLG = 3, then set the context to the values specified
C                  in the CICNTX and CFCNTX arrays.
C
C
C  The current integer attribute context returned in CICNTX:
C    CICNTX( 1) = Clip indicator
C    CICNTX( 2) = Polyline index
C    CICNTX( 3) = Line type
C    CICNTX( 4) = Polyline color index
C    CICNTX( 5) = Polymarker index
C    CICNTX( 6) = Marker type
C    CICNTX( 7) = Polymarker color index
C    CICNTX( 8) = Text index
C    CICNTX( 9) = Text font
C    CICNTX(10) = Text precision
C    CICNTX(11) = Text color index
C    CICNTX(12) = Text path
C    CICNTX(13) = Text horizontal alignment
C    CICNTX(14) = Text vertical alignment
C    CICNTX(15) = Fill area index
C    CICNTX(16) = Fill area interior style
C    CICNTX(17) = Fill are style index
C    CICNTX(18) = Fill area color index
C    CICNTX(19) = Linetype ASF
C    CICNTX(20) = Linewidth scale factor ASF
C    CICNTX(21) = Polyline color index ASF
C    CICNTX(22) = Marker type ASF
C    CICNTX(23) = Marker size scale factor ASF
C    CICNTX(24) = Polymarker color index ASF
C    CICNTX(25) = Text font and precision ASF
C    CICNTX(26) = Character expansion factor ASF
C    CICNTX(27) = Character spacing ASF
C    CICNTX(28) = Text color index ASF
C    CICNTX(29) = Fill area interior style ASF
C    CICNTX(30) = Fill area style index ASF
C    CICNTX(31) = Fill area color index ASF
C
C  The current real attribute context returned in CICNTX:
C    CFCNTX( 1) = Linewidth scale factor
C    CFCNTX( 2) = Marker scale factor
C    CFCNTX( 3) = Character expansion factor
C    CFCNTX( 4) = Character spacing
C    CFCNTX( 5) = Character height (world coordinates)
C    CFCNTX( 6) = Character up vector, X component (world coordinates)
C    CFCNTX( 7) = Character up vector, Y component (world coordinates)
C    CFCNTX( 8) = Pattern size, X component (world coordinates)
C    CFCNTX( 9) = Pattern size, Y component (world coordinates)
C    CFCNTX(10) = Pattern reference point, X component (world coordinates)
C    CFCNTX(11) = Pattern reference point, Y component (world coordinates)
C    CFCNTX(12) = Clip rectangle, left X (NDC)
C    CFCNTX(13) = Clip rectangle, right X (NDC)
C    CFCNTX(14) = Clip rectangle, bottom Y (NDC)
C    CFCNTX(15) = Clip rectangle, top Y (NDC)
C    CFCNTX(16) = X component in NDC of a vector parallel to the character
C                 up vector with length of character height.
C    CFCNTX(17) = Y component in NDC of a vector parallel to the character
C                 up vector with length of character height.
C    CFCNTX(18) = X component in NDC of a vector parallel to the character
C                 base vector.
C    CFCNTX(19) = Y component in NDC of a vector parallel to the character
C                 base vector.
C
      include 'gkscom.h'
C
      INTEGER CICNTX(31)
      REAL    CFCNTX(19)
C
      INTEGER SRFLG,LASF(13)
C
      INTEGER   SCLIP , SPLI  , SLN   , SPLCI , SPMI  , SMK   , SPMCI ,       
     +          STXI  , STXFP , STXCI , STXP  , STXAL , SFAI  , SFAIS , 
     +          SFASI , SFACI 
      DIMENSION STXFP(2), STXAL(2)
      REAL      SLWSC , SMKS  , SCHXP , SCHSP , SCHH  , SCHUP , SPA   , 
     +          SPARF
      DIMENSION SCHUP(2), SPA(2), SPARF(2)
      INTEGER   SLNA  , SLWSCA, SPLCIA, SMKA  , SMKSA , SPMCIA, STXFPA,
     +          SCHXPA, SCHSPA, STXCIA, SFAISA, SFASIA, SFACIA
C
      SAVE
C
      IF (SRFLG .EQ. 0) THEN
        SCLIP    = CCLIP
        SPLI     = CPLI
        SLN      = CLN
        SLWSC    = CLWSC
        SPLCI    = CPLCI
        SPMI     = CPMI
        SMK      = CMK
        SMKS     = CMKS
        SPMCI    = CPMCI
        STXI     = CTXI
        STXFP(1) = CTXFP(1)
        STXFP(2) = CTXFP(2)
        SCHXP    = CCHXP
        SCHSP    = CCHSP
        STXCI    = CTXCI
        SCHH     = CCHH
        SCHUP(1) = CCHUP(1)
        SCHUP(2) = CCHUP(2)
        STXP     = CTXP
        STXAL(1) = CTXAL(1)
        STXAL(2) = CTXAL(2)
        SFAI     = CFAI
        SFAIS    = CFAIS
        SFASI    = CFASI
        SFACI    = CFACI
        SPA(1)   = CPA(1)
        SPA(2)   = CPA(2)
        SPARF(1) = CPARF(1)
        SPARF(2) = CPARF(2)
C
        SLNA     = CLNA
        SLWSCA   = CLWSCA
        SPLCIA   = CPLCIA
        SMKA     = CMKA
        SMKSA    = CMKSA
        SPMCIA   = CPMCIA
        STXFPA   = CTXFPA
        SCHXPA   = CCHXPA
        SCHSPA   = CCHSPA
        STXCIA   = CTXCIA
        SFAISA   = CFAISA
        SFASIA   = CFASIA
        SFACIA   = CFACIA
      ELSE IF (SRFLG .EQ. 1) THEN
        CCLIP    = SCLIP
        CALL GSCLIP (CCLIP)
        CPLI     = SPLI
        CALL GSPLI  (CPLI )
        CLN      = SLN
        CALL GSLN   (CLN  )
        CLWSC    = SLWSC
        CALL GSLWSC (CLWSC)
        CPLCI    = SPLCI
        CALL GSPLCI (CPLCI)
        CPMI     = SPMI
        CALL GSPMI  (CPMI )
        CMK      = SMK
        CALL GSMK   (CMK  )
        CMKS     = SMKS
        CALL GSMKSC (CMKS )
        CPMCI    = SPMCI
        CALL GSPMCI (CPMCI)
        CTXI     = STXI
        CALL GSTXI  (CTXI )
        CTXFP(1) = STXFP(1)
        CTXFP(2) = STXFP(2)
        CALL GSTXFP (CTXFP(1),CTXFP(2))
        CCHXP    = SCHXP
        CALL GSCHXP (CCHXP)
        CCHSP    = SCHSP
        CALL GSCHSP (CCHSP)
        CTXCI    = STXCI
        CALL GSTXCI (CTXCI)
        CCHH     = SCHH
        CALL GSCHH  (CCHH )
        CCHUP(1) = SCHUP(1)
        CCHUP(2) = SCHUP(2)
        CALL GSCHUP (CCHUP(1),CCHUP(2))
        CTXP     = STXP
        CALL GSTXP  (CTXP )
        CTXAL(1) = STXAL(1)
        CTXAL(2) = STXAL(2)
        CALL GSTXAL (CTXAL(1),CTXAL(2))
        CFAI     = SFAI
        CALL GSFAI  (CFAI )
        CFAIS    = SFAIS
        CALL GSFAIS (CFAIS)
        CFASI    = SFASI
        CALL GSFASI (CFASI)
        CFACI    = SFACI
        CALL GSFACI (CFACI)
        CPA(1)   = SPA(1)
        CPA(2)   = SPA(2)
        CALL GSPA   (CPA  (1),CPA  (2))
        CPARF(1) = SPARF(1)
        CPARF(2) = SPARF(2)
        CALL GSPARF (CPARF(1),CPARF(2))
C
        CLNA     = SLNA
        CLWSCA   = SLWSCA
        CPLCIA   = SPLCIA
        CMKA     = SMKA
        CMKSA    = SMKSA
        CPMCIA   = SPMCIA
        CTXFPA   = STXFPA
        CCHXPA   = SCHXPA
        CCHSPA   = SCHSPA
        CTXCIA   = STXCIA
        CFAISA   = SFAISA
        CFASIA   = SFASIA
        CFACIA   = SFACIA
        LASF( 1) = CLNA
        LASF( 2) = CLWSCA
        LASF( 3) = CPLCIA
        LASF( 4) = CMKA
        LASF( 5) = CMKSA
        LASF( 6) = CPMCIA
        LASF( 7) = CTXFPA
        LASF( 8) = CCHXPA
        LASF( 9) = CCHSPA
        LASF(10) = CTXCIA
        LASF(11) = CFAISA
        LASF(12) = CFASIA
        LASF(13) = CFACIA
        CALL GSASF (LASF)
      ELSE IF (SRFLG .EQ. 2) THEN
C
C  Return the current context.
C
        CICNTX( 1) = CCLIP
        CICNTX( 2) = CPLI
        CICNTX( 3) = CLN
        CICNTX( 4) = CPLCI
        CICNTX( 5) = CPMI
        CICNTX( 6) = CMK
        CICNTX( 7) = CPMCI
        CICNTX( 8) = CTXI
        CICNTX( 9) = CTXFP(1)
        CICNTX(10) = CTXFP(2)
        CICNTX(11) = CTXCI
        CICNTX(12) = CTXP
        CICNTX(13) = CTXAL(1)
        CICNTX(14) = CTXAL(2)
        CICNTX(15) = CFAI
        CICNTX(16) = CFAIS
        CICNTX(17) = CFASI
        CICNTX(18) = CFACI
        CICNTX(19) = CLNA
        CICNTX(20) = CLWSCA
        CICNTX(21) = CPLCIA
        CICNTX(22) = CMKA
        CICNTX(23) = CMKSA
        CICNTX(24) = CPMCIA
        CICNTX(25) = CTXFPA
        CICNTX(26) = CCHXPA
        CICNTX(27) = CCHSPA
        CICNTX(28) = CTXCIA
        CICNTX(29) = CFAISA
        CICNTX(30) = CFASIA
        CICNTX(31) = CFACIA
C
        CFCNTX( 1) = CLWSC
        CFCNTX( 2) = CMKS
        CFCNTX( 3) = CCHXP
        CFCNTX( 4) = CCHSP
        CFCNTX( 5) = CCHH
        CFCNTX( 6) = CCHUP(1)
        CFCNTX( 7) = CCHUP(2)
        CFCNTX( 8) = CPA(1)
        CFCNTX( 9) = CPA(2)
        CFCNTX(10) = CPARF(1)
        CFCNTX(11) = CPARF(2)
        CALL GQCLIP(IER,ICL,CFCNTX(12))
C
C  Create the normalized orientation vectors for text.
C
        SCL = 1./SQRT(CCHUP(1)*CCHUP(1)+CCHUP(2)*CCHUP(2))
        XP = CCHH*SCL*CCHUP(1)
        YP = CCHH*SCL*CCHUP(2)
        XB =  YP
        YB = -XP
        CALL GZW2NX(1,XP,XTMP)
        CALL GZW2NY(1,YP,YTMP)
        CALL GZW2NX(1,0.,ZXTMP)
        CALL GZW2NY(1,0.,ZYTMP)
        CFCNTX(16) = XTMP-ZXTMP
        CFCNTX(17) = YTMP-ZYTMP
        CALL GZW2NX(1,XB,XTMP)
        CALL GZW2NY(1,YB,YTMP)
        CFCNTX(18) = XTMP-ZXTMP
        CFCNTX(19) = YTMP-ZYTMP
      ELSE IF (SRFLG .EQ. 3) THEN
        CCLIP    = CICNTX(1)
        CALL GSCLIP (CCLIP)
        CPLI     = CICNTX(2)
        CALL GSPLI  (CPLI )
        CLN      = CICNTX( 3)
        CALL GSLN   (CLN  )
        CLWSC    = CFCNTX(1)
        CALL GSLWSC (CLWSC)
        CPLCI    = CICNTX(4)
        CALL GSPLCI (CPLCI)
        CPMI     = CICNTX(5)
        CALL GSPMI  (CPMI )
        CMK      = CICNTX(6)
        CALL GSMK   (CMK  )
        CMKS     = CFCNTX(2)
        CALL GSMKSC (CMKS )
        CPMCI    = CICNTX(7)
        CALL GSPMCI (CPMCI)
        CTXI     = CICNTX(8)
        CALL GSTXI  (CTXI )
        CTXFP(1) = CICNTX(9)
        CTXFP(2) = CICNTX(10)
        CALL GSTXFP (CTXFP(1),CTXFP(2))
        CCHXP    = CFCNTX(3)
        CALL GSCHXP (CCHXP)
        CCHSP    = CFCNTX(4)
        CALL GSCHSP (CCHSP)
        CTXCI    = CICNTX(11)
        CALL GSTXCI (CTXCI)
        CCHH     = CFCNTX(5)
        CALL GSCHH  (CCHH )
        CCHUP(1) = CFCNTX(6)
        CCHUP(2) = CFCNTX(7)
        CALL GSCHUP (CCHUP(1),CCHUP(2))
        CTXP     = CICNTX(12)
        CALL GSTXP  (CTXP )
        CTXAL(1) = CICNTX(13)
        CTXAL(2) = CICNTX(14)
        CALL GSTXAL (CTXAL(1),CTXAL(2))
        CFAI     = CICNTX(15)
        CALL GSFAI  (CFAI )
        CFAIS    = CICNTX(16)
        CALL GSFAIS (CFAIS)
        CFASI    = CICNTX(17)
        CALL GSFASI (CFASI)
        CFACI    = CICNTX(18)
        CALL GSFACI (CFACI)
        CPA(1)   = CFCNTX(8)
        CPA(2)   = CFCNTX(9)
        CALL GSPA   (CPA  (1),CPA  (2))
        CPARF(1) = CFCNTX(10)
        CPARF(2) = CFCNTX(11)
        CALL GSPARF (CPARF(1),CPARF(2))
C
        LASF( 1) = CICNTX(19)
        LASF( 2) = CICNTX(20)
        LASF( 3) = CICNTX(21)
        LASF( 4) = CICNTX(22)
        LASF( 5) = CICNTX(23)
        LASF( 6) = CICNTX(24)
        LASF( 7) = CICNTX(25)
        LASF( 8) = CICNTX(26)
        LASF( 9) = CICNTX(27)
        LASF(10) = CICNTX(28)
        LASF(11) = CICNTX(29)
        LASF(12) = CICNTX(30)
        LASF(13) = CICNTX(31)
        CALL GSASF (LASF)
C
      ENDIF
C
      RETURN
      END 
