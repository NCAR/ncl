C
C     $Id: 
C
C     interface to dashchar package. set the gap character to an
C     underscore.
      SUBROUTINE SETDASHCHAR()
C
      COMMON /BLGASO/ IBLK,IGAP,ISOL
      CHARACTER*1     IBLK,IGAP,ISOL
C
      IGAP = '_'
C
      RETURN
C
      END
C
      SUBROUTINE RESETDASHCHAR()
C
      COMMON /BLGASO/ IBLK,IGAP,ISOL
      CHARACTER*1     IBLK,IGAP,ISOL
C
      IGAP = ''''
C
      RETURN
      END
