C
C	$Id: fngbkd.f,v 1.5 2008-07-27 00:59:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FNGBKD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA FNGBKDX
C
      PARAMETER ( NUM=71 , NUMA=468 )
      COMMON /SUBNMS/SUBNAM,ALLNAM
      CHARACTER*10 SUBNAM(NUM),ALLNAM(NUMA)
C
      DATA (SUBNAM(I),I=1,NUM)/
     -     'AXES'  ,'CONOP1','CONOP2','CONOP3','CONOP4',
     -     'CURVE' ,'CURVE3','CURVED','DASHLN','DASHD' ,
     -     'ENCODE','FENCE3','FL2INT','FLASH1','FLASH2','FLASH3',
     -     'FLASH4','FLUSH' ,'FLUSHB','FRAME' ,'FRST3' ,'FRSTD' ,
     -     'FRSTPT','GETCHR','GETCND','GETOPT','GETSET','GETSI' ,
     -     'INTT'  ,'JUSTFY','LINE'  ,'LINE3' ,'LINED' ,'LOC'   ,
     -     'MXMY'  ,'OPTION','OPTN'  ,'PACKUM','PERIM3','PERROR',
     -     'PLOTIT','POINT' ,'POINT3','POINTS','PORGN' ,'PREOUT',
     -     'PSCALE','PSYM'  ,'PSYM3' ,'PUT42' ,'PUTINS','PWRIT' ,
     -     'PWRITX','PWRITY','PWRM'  ,'PWRT'  ,'PWRX'  ,'PWRY'  ,
     -     'SET'   ,'SET3'  ,'SETCHR','SETCND','SETI'  ,'TICK3' ,
     -     'TICK43','TRANS' ,'ULIBER','VECT3' ,'VECTD' ,'VECTOR',
     -     'WRITEB'/
C
      DATA (ALLNAM(LL),LL=  1,108)  /
     -     'ADDCHR','AGAXIS','AGBACK','AGBNCH','AGCHAX','AGCHCU',
     -     'AGCHIL','AGCHNL','AGCTCS','AGCTKO','AGCURV','AGDASH',
     -     'AGDFLT','AGDLCH','AGDSHN','AGEXAX','AGEXUS','AGEZSU',
     -     'AGFPBN','AGFTOL','AGGETC','AGGETF','AGGETI','AGGETP',
     -     'AGGTCH','AGINIT','AGKURV','AGLBLS','AGMAXI','AGMINI',
     -     'AGNUMB','AGPPID','AGPWRT','AGQURV','AGRPCH','AGRSTR',
     -     'AGSAVE','AGSCAN','AGSETC','AGSETF','AGSETI','AGSETP',
     -     'AGSRCH','AGSTCH','AGSTUP','AGUTOL','ANOTAT','ARINT' ,
     -     'AXES'  ,'BD1'   ,'BD2'   ,'BLANK' ,'BOUND' ,'BUFF'  ,
     -     'C2W'   ,'CA2B'  ,'CALCNT','CARDIN','CCHECK','CCON'  ,
     -     'CERMSG','CFVLD' ,'CH8'   ,'CHKCYC','CLGEN' ,'CLSET' ,
     -     'CMP2'  ,'COMBIN','CONBDN','CONBND','CONCAL','CONCLD',
     -     'CONCLS','CONCOM','CONDET','CONDRW','CONDSD','CONECD',
     -     'CONGEN','CONINT','CONLCM','CONLIN','CONLOC','CONLOD',
     -     'CONOP1','CONOP2','CONOP3','CONOP4','CONOT2','CONOUT',
     -     'CONPDV','CONPMM','CONPMS','CONRAN','CONRAQ','CONRAS',
     -     'CONREC','CONREO','CONSET','CONSLD','CONSSD','CONSTP',
     -     'CONSWV','CONTLK','CONTNG','CONTOR','CONXCH','COPY'   /
      DATA (ALLNAM(LL),LL=109,216)  /
     -     'CREB15','CREBIN','CTCELL','CTRINT','CURVE' ,'CURVE3',
     -     'CURVED','CURVEW','CUTUP' ,'DANDR' ,'DASHBD','DASHD' ,
     -     'DASHLN','DASHMC','DAT1'  ,'DAT2'  ,'DAT3'  ,'DAT4'  ,
     -     'DAT5'  ,'DAT6'  ,'DAT7'  ,'DAT8'  ,'DCHECK','DCON'  ,
     -     'DGMOD' ,'DISPLA','DOTEDG','DPORT' ,'DRAWI' ,'DRAWMC',
     -     'DRAWPV','DRAWS' ,'DRAWT' ,'DRCNTR','CRDRLN','DRWBRB',
     -     'DRWSTR','DRWVEC','DTOA'  ,'E9RIN' ,'ENCD'  ,'ENCODE',
     -     'ENTSR' ,'EPRIN' ,'ERROF' ,'ETOA'  ,'EXPAND','EZBARB',
     -     'EZCNTR','EZHFTN','EZISOS','EZMXY' ,'EZMY'  ,'EZSRFC',
     -     'EZSTRM','EZVEC' ,'EZXY'  ,'EZY'   ,'FDUM'  ,'FDVDLD',
     -     'FENCE3','FILL'  ,'FILLBD','FILLCK','FILLIN','FILLNC',
     -     'FILLOP','FILLPA','FL2INT','FLASH1','FLASH2','FLASH3',
     -     'FLASH4','FLMHDR','FLSALL','FLSH'  ,'FLUSH' ,'FLUSHB',
     -     'FRADV' ,'FRAME' ,'FRST3' ,'FRSTC' ,'FRSTD' ,'FRSTPT',
     -     'FRSTS' ,'FRSTW' ,'FTOI'  ,'G16UPK','GAP'   ,'GASCCH',
     -     'GENCHR','GET16' ,'GETCHR','GETCND','GETDAT','GETFLT',
     -     'GETHOL','GETINT','GETOPT','GETRNG','GETSET','GETSI' ,
     -     'GETTYP','GNEWPT','GRAY'  ,'GRDINT','GRID'  ,'GRIDAL' /
      DATA (ALLNAM(LL),LL=217,324)  /
     -     'GRIDL' ,'GTCH'  ,'GTNUM' ,'GTNUMB','HAFTON','HALFAX',
     -     'HEADER','HFINIT','HILO'  ,'I8SAV' ,'IDICTL','IDIOT' ,
     -     'IMPCHK','INIT3D','INITZI','INITZS','INITZT','INSTP' ,
     -     'INTERT','INTT'  ,'ISOSRB','ISOSRF','JLM2'  ,'JUSTFY',
     -     'KURV1S','KURV2S','LABEL' ,'LABMOD','LASTD' ,'LGFILE',
     -     'LINE'  ,'LINE3' ,'LINE3W','LINEAR','LINED' ,'LINEW' ,
     -     'LOC'   ,'MAPBD' ,'MAPCEM','MAPCHI','MAPDRW','MAPEOS',
     -     'MAPFST','MAPGRD','MAPGTC','MAPGTI','MAPGTL','MAPGTR',
     -     'MAPINT','MAPIO' ,'MAPIQ' ,'MAPIT' ,'MAPLBL','MAPLMB',
     -     'MAPLOT','MAPPOS','MAPROJ','MAPRS' ,'MAPRST','MAPSAV',
     -     'MAPSET','MAPSTC','MAPSTI','MAPSTL','MAPSTR','MAPTRE',
     -     'MAPTRN','MAPTRP','MAPUSR','MAPVEC','MAPVP' ,'MARKL' ,
     -     'MAXMIN','MCSCAL','MCTR'  ,'MCTRAN','MCTRHX','MFCLOS',
     -     'MFCMDL','MFCPFR','MFED'  ,'MFEWT' ,'MFFSTF','MFGATH',
     -     'MFGETD','MFINIT','MFNREC','MFOPEN','MFPAR1','MFPAR2',
     -     'MFPAR4','MFPAR5','MFPAR6','MFPAR7','MFPARS','MFPERR',
     -     'MFREAD','MFRMBL','MFSORT','MFTYPE','MFWRIT','MINMAX',
     -     'MKMSK' ,'MMASK' ,'MOVCHR','MTITLE','MXMY'  ,'MXMYW'  /
      DATA (ALLNAM(LL),LL=325,432)  /
     -     'NAMEFR','NERRO' ,'NEWPEN','NUFRAM','OPTION','OPTN'  ,
     -     'OPTN2' ,'OUTZRO','PACKUM','PASS2' ,'PCRBIN','PERIM' ,
     -     'PERIM3','PERIML','PERROR','PLOT'  ,'PLOTIT','PLOTIW',
     -     'PLOTMC','PLOTS' ,'PLTGRD','PNUM'  ,'POINT' ,'POINT3',
     -     'POINTS','POINTW','PORGN' ,'POSITN','PRCARG','PREFIX',
     -     'PREOUT','PREP'  ,'PRGO'  ,'PRINT' ,'PRSET' ,'PRSIMX',
     -     'PRTOD' ,'PSCALE','PSTART','PSYM'  ,'PSYM3' ,'PUT42' ,
     -     'PUTINS','PWRIQ' ,'PWRIT' ,'PWRITV','PWRITW','PWRITX',
     -     'PWRITY','PWRM'  ,'PWRT'  ,'PWRX'  ,'PWRXBD','PWRY'  ,
     -     'PWRYBD','PWRYGT','PWRYMC','PWRYSO','PWRYW' ,'PWRZ'  ,
     -     'PWRZGI','PWRZGS','PWRZGT','PWRZI' ,'PWRZOI','PWRZOS',
     -     'PWRZOT','PWRZS' ,'PWRZT' ,'PWWBKD','PWWGET','PWWSOR',
     -     'QUICK' ,'RDAMRC','RDLINE','RDSYS' ,'RDWT'  ,'REMOVE',
     -     'REORD' ,'RESET' ,'RETSR' ,'RITE'  ,'SCALEB','SCAN'  ,
     -     'SCPLTW','SCPWTW','SCRLBD','SCROLL','SET'   ,'SET3'  ,
     -     'SET3D' ,'SETCHR','SETCND','SETER' ,'SETI'  ,'SETR'  ,
     -     'SETUM' ,'SETW'  ,'SKFILE','SKIP'  ,'SPACE' ,'SPLINT',
     -     'SRFABD','SRFACE','STCNTR','STLINE','STRMLN','SUPCON' /
      DATA (ALLNAM(LL),LL=433,468)  /
     -     'SUPMAP','T20'   ,'T268'  ,'T400'  ,'TERM'  ,'TEXT'  ,
     -     'THREBD','TICK3' ,'TICK4' ,'TICK43','TICKS' ,'TPGPHC',
     -     'TR32'  ,'TRANS' ,'TRIDIP','TRN32I','TRN32S','TRN32T',
     -     'UERRBD','ULIBER','UNHEX' ,'VECT3' ,'VECTD' ,'VECTOR',
     -     'VECTW' ,'VELDAT','VELVCT','VELVEC','VMAP'  ,'W2C'   ,
     -     'WINSYS','WNDBRB','WNDOUT','WRITEB','XTCH'  ,'XTCHDT' /
      END
