C
C	$Id: gsegdt.f,v 1.1 1993-01-09 02:02:31 fred Exp $
C
      BLOCKDATA GSEGDT
C
C  Define all constant data for the segment interpreter.
C
      include 'trstat.h'
      include 'trbufr.h'
      include 'trpars.h'
      include 'trinst.h'
      include 'trcode.h'
      include 'trdefl.h'
C
C  Initialize the record pointer.
C
      DATA METREC /1/
C
C  Default polyline info.
C
      DATA POLIDF, LINTDF, LINWDF, TLNCDF /1, 1, 1.0, 1/
C
C  Default marker info.
C
      DATA MARIDF, MARTDF, MARSDF, TMRCDF, PMRSDF /1, 3, 1.0, 1, 320/
C
C  Default text and character info.
C
      DATA HORIDF, VERTDF, PATHDF, CHIGDF, XUDF, YUDF /0,0,0,320,0,1/
      DATA XBDF, YBDF, TXTIDF, TTXCDF, FINDDF, CEXPDF /1,0,1,1,1,1.0/
      DATA CSPADF, TXTPDF /0.0, 0/
C
C  Default fill info.
C
      DATA FILIDF, INTSDF, HATIDF, PATIDF, TFLCDF /1, 0, 1, 1, 1/
      DATA FILRDF /0,0/
C
C  Define the data field default bit sizes.
C
      DATA MOPDLN, MCIDPR, MFLDPR, MWHDPR /16, 8, 16, 16/
      DATA MIXDPR, MENDPR /16,16/
      DATA BYTSIZ, CONFLG /8, 1/
C
C  Define the default aspect source flags.
C
      DATA ASFSDF /ASFMAX*0/
C
C  Control element codes.
C
        DATA CELMVI, CELMCR, CELMCI /1,5,6/
C
C  Graphical primitive element codes.
C
        DATA GPELPL, GPELPM, GPELTX, GPELPG, GPELCA, GPELGP
     1          /1, 3, 4, 7, 9, 10/
C
C  Define the attribute element codes.
C       
        DATA ATELLI, ATELLT, ATELLW, ATELLC /1, 2, 3, 4/
        DATA ATELMB, ATELMT, ATELMZ, ATELMC /5, 6, 7, 8/
        DATA ATELTI, ATELTF, ATELTP, ATELCE, ATELCS /9, 10, 11, 12, 13/
        DATA ATELTC, ATELCH, ATELCO, ATELTH, ATELTA /14,15,16,17,18/
        DATA ATELFI, ATELIS, ATELFC, ATELHI, ATELPI /21,22,23,24,25/
        DATA ATELFR, ATELPT, ATELPS, ATELCB, ATELAS /31,32,33,34,35/
C
      END
