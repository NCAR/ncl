C
C	$Id: gzsrat.f,v 1.1 1993-01-09 02:04:32 fred Exp $
C
      SUBROUTINE GZSRAT(SRFLG)
C
C  Save/restore the attribute context.
C
C    If SRFLG = 0, then save the context;
C    if SRFLG = 1, then restore the context.
C
      include 'gkscom.h'
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
      ELSE IF (SRFLG .EQ.1) THEN
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
      ENDIF
C
      RETURN
      END 
