C
C	$Id: lblank.f,v 1.1 1994-08-11 16:29:55 haley Exp $
C
c Fortran function lblank
c
c Returns:	Index of last character in string
c
c Side Effects:	None
c
c Purpose:	To provide the ability to concatenate two
c		character strings based on what is in them
c		rather than their length.
c
c Comments:	Used to be a relatively standard function,
c		before the onset (onslaught?) of System V.
c
c Author:	Don Middleton
c		NCAR Graphics Group
c		November 1988
c
      integer function lblank(str)
      character*(*) str

      do 10 i=len(str), 1, -1
	if (str(i:i).ne.' ') then
	  lblank = i
	  return
	endif
10    continue

      lblank = 1
      end
