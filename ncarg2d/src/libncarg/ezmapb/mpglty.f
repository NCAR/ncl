C
C $Id: mpglty.f,v 1.1 1998-04-16 20:45:38 kennison Exp $
C
      SUBROUTINE MPGLTY (IARG)
C
C This routine may be called by a user from the line-processing routine
C specified as the final argument in a call to MPLNDM in order to obtain
C the type of the line being processed.
C
        COMMON /MAPCMZ/ NNMS,ILTY,IAIL,IAIR,BLAG,SLAG,BLOG,SLOG,
     +                  PNTS(200)
        SAVE   /MAPCMZ/
C
        IARG=ILTY
C
        RETURN
C
      END
