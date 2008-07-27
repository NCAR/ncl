#!/bin/csh -f -b
#
#	$Id: ncargsrcy.csh,v 1.6 2008-07-27 00:59:06 haley Exp $
#                                                                      
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
#

set path = (/bin /usr/bin)

set bnpath = $0
set bindir = $bnpath:h
set l = `$bindir/ncargpath SED_LIBDIR`

if ($status != 0) then
        exit 1
endif

set nlibs = "$l/srcncarg.a $l/srcncarg_gks.a"
set alibs = ""
set blibs = ""

echo ""
echo "Remember that NCAR Graphics source code is copyrighted."
echo ""

echo "`who am i` `date` $*" >>! $l/ncargsrc.stats

if ($#argv == 1 && "$1" == "-index") then

  cat << END > INDEX
Contents of the library srcncarg.a:

`ar t $l/srcncarg.a | sort | pr -t -5 -l1`

Contents of the library srcncarg_gks.a:

`ar t $l/srcncarg_gks.a | sort | pr -t -5 -l1`

Contents of the library srcagupwrtx.a:

`ar t $l/srcagupwrtx.a | sort | pr -t -5 -l1`

Contents of the library srcconraq.a:

`ar t $l/srcconraq.a | sort | pr -t -5 -l1`

Contents of the library srcconras.a:

`ar t $l/srcconras.a | sort | pr -t -5 -l1`

Contents of the library srcconrcqck.a:

`ar t $l/srcconrcqck.a | sort | pr -t -5 -l1`

Contents of the library srcconrcspr.a:

`ar t $l/srcconrcspr.a | sort | pr -t -5 -l1`

Contents of the library srcdashline.a:

`ar t $l/srcdashline.a | sort | pr -t -5 -l1`

Contents of the library srcdashsmth.a:

`ar t $l/srcdashsmth.a | sort | pr -t -5 -l1`

Contents of the library srcdashsupr.a:

`ar t $l/srcdashsupr.a | sort | pr -t -5 -l1`
END

  chmod 644 INDEX

  exit 0

endif

foreach arg ($argv)

  switch ($arg)

  case "-agupwrtx":

    set alibs = "$alibs $l/srcagupwrtx.a"
    set blibs = "$l/srcagupwrtx.a $blibs"
    breaksw

  case "-quick":

    set alibs = "$alibs $l/srcconrcqck.a $l/srcconraq.a $l/srcdashline.a"
    set blibs = "$l/srcdashline.a $l/srcconraq.a $l/srcconrcqck.a $blibs"
    breaksw

  case "-smooth":

    set alibs = "$alibs $l/srcdashsmth.a"
    set blibs = "$l/srcdashsmth.a $blibs"
    breaksw

  case "-super":

    set alibs = "$alibs $l/srcconrcspr.a $l/srcconras.a $l/srcdashsupr.a"
    set blibs = "$l/srcdashsupr.a $l/srcconras.a $l/srcconrcspr.a $blibs"
    breaksw

  case "-conrecquick":

    set alibs = "$alibs $l/srcconrcqck.a"
    set blibs = "$l/srcconrcqck.a $blibs"
    breaksw

  case "-conrecsmooth":

    set alibs = "$alibs $l/srcdashsmth.a"
    set blibs = "$l/srcdashsmth.a $blibs"
    breaksw

  case "-conrecsuper":

    set alibs = "$alibs $l/srcconrcspr.a $l/srcdashsupr.a"
    set blibs = "$l/srcdashsupr.a $l/srcconrcspr.a $blibs"
    breaksw

  case "-conranquick":

    set alibs = "$alibs $l/srcconraq.a"
    set blibs = "$l/srcconraq.a $blibs"
    breaksw

  case "-conransmooth":

    set alibs = "$alibs $l/srcdashsmth.a"
    set blibs = "$l/srcdashsmth.a $blibs"
    breaksw

  case "-conransuper":

    set alibs = "$alibs $l/srcconras.a $l/srcdashsupr.a"
    set blibs = "$l/srcdashsupr.a $l/srcconras.a $blibs"
    breaksw

  case "-dashline":
  case "-dashquick":

    set alibs = "$alibs $l/srcdashline.a"
    set blibs = "$l/srcdashline.a $blibs"
    breaksw

  case "-dashsmooth":

    set alibs = "$alibs $l/srcdashsmth.a"
    set blibs = "$l/srcdashsmth.a $blibs"
    breaksw

  case "-dashsuper":

    set alibs = "$alibs $l/srcdashsupr.a"
    set blibs = "$l/srcdashsupr.a $blibs"
    breaksw

  case "-index":

    set index
    breaksw

  case "-*":

    echo "Unknown option <$arg>."
    exit 1

  default:

    set arg = `echo $arg | tr '[A-Z]' '[a-z]'`

    unset fileinormalibrary

    set errorflag = "notfoundyet"

    foreach lib ($alibs)
      set errorflag = `ar x $lib $arg |& cat`
      if (".$errorflag" == ".") then
	break
      endif
    end

    if (".$errorflag" != ".") then
      foreach lib ($nlibs)
	set errorflag = `ar x $lib $arg |& cat`
	if (".$errorflag" == ".") then
	  set fileinormalibrary
	  break
	endif
      end
    endif

    if (".$errorflag" != ".") then
      set clibs = ""
      foreach lib ($l/srcagupwrtx.a $l/srcconraq.a   $l/srcconras.a \
		   $l/srcconrcqck.a $l/srcconrcspr.a $l/srcdashline.a \
		   $l/srcdashsmth.a $l/srcdashsupr.a)
	unset found
	foreach alib ($alibs)
	  if ("$lib" == "$alib") then
	    set found
	    break
	  endif
	end
	if (! $?found) set clibs = "$clibs $lib"
      end
      foreach lib ($clibs)
	set errorflag = `ar x $lib $arg |& cat`
	if (".$errorflag" == ".") then
	  break
	endif
      end
    endif

    if (".$errorflag" == ".") then
      chmod 644 $arg
      if ("$arg:r" == "$arg") then
	if ($?index) then
	  mv $arg $arg.index
	else
	  set filelist = `cat $arg`
	  ar x $lib $filelist
	  if ($?fileinormalibrary) then
	    foreach lib ($blibs)
	      set bfiles = `ar t $lib`
	      set gfiles = ""
	      foreach file ($bfiles)
		if ("$file:r" != "$file") then
		  set gfiles = "$gfiles $file"
		  if (! -e $file) then
		    set filelist = "$filelist $file"
		  endif
		endif
	      end
	      ar x $lib $gfiles
	    end
	  endif
	  cat $filelist >! $arg
	  rm -f $filelist
	endif
      endif
    else
      echo "Cannot find file <$arg>."
    endif
    breaksw

  endsw

end

exit 0
