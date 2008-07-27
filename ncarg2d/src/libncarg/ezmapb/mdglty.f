C
C $Id: mdglty.f,v 1.2 2008-07-27 00:17:06 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDGLTY (IARG)
C
        INTEGER IARG
C
C This routine may be called by a user from the line-processing routine
C specified as the final argument in a call to MDLNDM in order to obtain
C the type of the line being processed.
C
        COMMON /MAPCMZ/  NNMS,ILTY,IAIL,IAIR,BLAG,SLAG,BLOG,SLOG,
     +                   PNTS(200)
        INTEGER          NNMS,ILTY,IAIL,IAIR
        REAL             BLAG,SLAG,BLOG,SLOG,PNTS
        SAVE   /MAPCMZ/
C
        IARG=ILTY
C
        RETURN
C
      END
