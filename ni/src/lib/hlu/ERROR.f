C
C $Id: ERROR.f,v 1.1 1994-05-12 23:51:07 boote Exp $
C
C****************************************************************
C								*
C			Copyright (C)  1994			*
C	University Corporation for Atmospheric Research		*
C			All Rights Reserved			*
C								*
C****************************************************************
C
C      File:            ERROR.f
C
C      Author:          Jeff W. Boote
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Fri Apr 15 16:52:28 MDT 1994
C
C      Description:     
C

C****************************************************************
C
C	Private functions used by Error
C
C****************************************************************
C
C find out if iunit is connected - return 0 if not 1 if is
C
      subroutine nhl_finqunit(iunit,iconn,ierr)
	integer iunit,iconn
	logical opn

	inquire(iunit,OPENED=opn,IOSTAT=ierr)
	if(ierr .EQ. 0) then
		if (opn .EQ. .TRUE.) then
			opn = 1
		else
			opn = 0
		endif
	endif
      end
C
C open the given file with the given unit number
C
      subroutine nhl_fopnunit(iunit,fname,fname_len,ierr)
	integer iunit,fname_len,ierr
	character*(*) fname

	open(iunit,FILE=fname(:fname_len),IOSTAT=ierr)
      end
C
C close the given unit number
C
      subroutine nhl_fclsunit(iunit,ierr)
	integer iunit,ierr
	close(iunit,IOSTAT=ierr)
      end

C
C print a message to the unit number
C

      subroutine nhl_fprnmes(iunit,ermess,ierlen)
      	external integer i1mach
      	integer iunit,ierlen,ierr
     	character*(*) ermess
	
      	write(iunit,*,IOSTAT=ierr) ermess(:ierlen)
      	if(ierr .NE. 0) then
      		write(i1mach(4),*,IOSTAT=ierr)
     % 				'Unable to print Error Messages???'
      	endif

      end
C
C****************************************************************
C
C	Public Functions
C
C****************************************************************
C
      subroutine nhlfperror(slevel,ienum,estring)
	character*(*) slevel,estring
	integer ienum
	call nhl_fperror(slevel,len(slevel),ienum,estring,len(estring))
      end
C
C
C
      subroutine nhlferrgetid(id_ret)
	integer id_ret
	call nhl_ferrgetid(id_ret)
      end
C
C
C
      subroutine nhlferrnummsgs(nummsg)
	integer nummsg
	call nhl_ferrnummsgs(nummsg)
      end
C
C
C
      subroutine nhlferrgetmsg(imsg,ilevel,emess,enum,smess,line,file,
     %	ierr)
	integer imsg,ilevel,enum,line,ierr
	character*(*) emess,smess,file
	call nhl_ferrgetmsg(imsg,ilevel,emess,len(emess),enum,smess,
     %		len(smess),line,file,len(file),ierr)
      end
C
C
C
      subroutine nhlferrclearmsgs(ierr)
	call nhl_ferrclearmsgs(ierr)
      end
C
C
C
      subroutine nhlferrsprintmsg(smess,imsg)
	character*(*) smess
	integer imsg
	call nhl_ferrsprintmsg(smess,len(smess),imsg)
      end
C
C
C
      subroutine nhlferrfprintmsg(iunit,imsg)
	integer iunit,imsg
	character*(10240) smsg

	call nhlferrsprintmsg(smsg,imsg)
	call nhl_fprnmes(iunit,smsg,len(smsg))
      end
