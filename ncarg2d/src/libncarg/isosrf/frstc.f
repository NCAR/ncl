C
C	$Id: frstc.f,v 1.1.1.1 1992-04-17 22:31:26 ncargd Exp $
C
C
C The subroutine FRSTC.
C --- ---------- ------
C
      SUBROUTINE FRSTC (MXN,MYN,IENT)
C
C This routine provides an interface to ISPLTF for codes still using
C coordinates in the metacode coordinate system.
C
        CALL ISPLTF (REAL(MXN)/32767.,REAL(MYN)/32767.,IENT)
C
C Done.
C
        RETURN
C
      END
