C
C	$Id: pcbdmq.f,v 1.1.1.1 1992-04-17 22:32:22 ncargd Exp $
C
C
C***********************************************************************
C B L O C K   D A T A   R O U T I N E S   -   D E F A U L T S
C***********************************************************************
C
      BLOCK DATA PCBDMQ
C
C Specify default values of internal parameters of PLCHMQ.
C
C COMMON block declarations.
C
      COMMON /PCPFMQ/ RHTW
      SAVE   /PCPFMQ/
C
C Define the default value of the ratio of character height to width.
C
      DATA RHTW / 1.75 /
C
      END
