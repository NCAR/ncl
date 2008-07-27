#!/bin/csh -f
#
#	$Id: ncargsrc.csh,v 1.7 2008-07-27 00:59:06 haley Exp $
#                                                                      
#                Copyright (C)  2000
#        University Corporation for Atmospheric Research
#                All Rights Reserved
#
# The use of this Software is governed by a License Agreement.
#

# If no arguments, tell user how "ncargsrc" works.

if ($#argv == 0) then

  more << "END"
The command "ncargsrc" may be used to acquire copies of NCAR Graphics source
files.  This material is copyrighted; redistribution is not permitted except
with written permission from UCAR (the University Corporation for Atmospheric
Research).

Note:  For various reasons, files retrieved by "ncargsrc" are temporarily put
in a subdirectory called "NCARGSOURCE" and then moved to another subdirectory
called "ncargsource".  Among other things, this prevents overwriting user
files accidentally.

The argument list is processed from left to right.  Arguments which do not
begin with a dash are the names of source files to be retrieved; those with
a suffixed ".f", ".c", or ".h" are names of specific routines, while those
which have no such suffix refer to collections of routines comprising a
complete utility.  For example, the command

    ncargsrc areas

will retrieve, in a file called "areas", the entire collection of routines
comprising the utility called AREAS, while the command

    ncargsrc argeti.f

will retrieve, in a file called "argeti.f", one member of that collection,
called ARGETI.

Arguments which begin with a dash select options to be applied from then on.

The argument "-index" directs that what should be retrieved for an argument
like "areas" is not the code for the utility called "AREAS", but a file
called "areas.index" containing the names of the routines which make up that
utility.  You can execute a command like

    ncargsrc -index areas

and then, after examining the contents of the file "areas.index", execute
a command like

    ncargsrc arscam.f

to retrieve a specific selected routine from "areas".

The command

    ncargsrc -index

(in which the sole argument is "-index") produces a file called "INDEX" that
contains a listing of the contents of all the source libraries.

The following two source libraries are normally searched first:

    srcncarg.a      -  NCAR Graphics utilities (including the default members
		       of the CONRAN, CONREC, and DASH families).
    srcncarg_gks.a  -  NCAR Graphics GKS package.

The following eight source libraries are normally searched after the first
two:

    srcagupwrtx.a   -  a version of the AUTOGRAPH routine AGPWRT that
		       uses PWRITX.
    srcconraq.a     -  the "quick" member of the CONRAN family.
    srcconras.a     -  the "super" member of the CONRAN family.
    srcconrcqck.a   -  the "quick" member of the CONREC family.
    srcconrcspr.a   -  the "super" member of the CONREC family.
    srcdashline.a   -  the "quick" member of the DASH family.
    srcdashsmth.a   -  the "smooth" member of the DASH family.
    srcdashsupr.a   -  the "super" member of the DASH family.

The following command-line arguments may be used to move libraries to the
beginning of the search list:

    Argument                    Libraries moved to beginning of list
    -------------------------   -----------------------------------------
    -agupwrtx                   srcagupwrtx.a
    -quick                      srcconrcqck.a, srcconraq.a, srcdashline.a
    -smooth                     srcdashsmth.a
    -super                      srcconrcspr.a, srcconras.a, srcdashsupr.a
    -conrecquick                srcconrcqck.a
    -conrecsmooth               srcdashsmth.a
    -conrecsuper                srcconrcspr.a, srcdashsupr.a
    -conranquick                srcconraq.a
    -conransmooth               srcdashsmth
    -conransuper                srcconras.a, srcdashsupr.a
    -dashquick (or -dashline)   srcdashline.a
    -dashsmooth                 srcdashsmth.a
    -dashsuper                  srcdashsupr.a

Moving a library to the beginning of the search list has a simple effect on
the retrieval of a specific file.  The command

     ncargsrc conrec.f

retrieves the default version of "conrec.f", while the command

     ncargsrc -quick conrec.f

retrieves the "quick" version.

The effect (of moving a library to the beginning of the search list) on the
retrieval of an entire utility is not quite so simple.  The command

     ncargsrc conrec

retrieves, in a single file called "conrec", all the routines of the default
version of the utility CONREC, while the command

     ncargsrc -quick conrec

retrieves, in a single file called "conrec", all the routines of the default
version of the utility CONREC, replaces those routines for which there are
"quick" versions with those versions, and then appends all other routines
from the "-quick" libraries to the end of the file "conrec".  The intent of
this is to provide the user with all of the source code required by the
version of the utility implied by the argument list, but it does not always
work quite as one might like.  Some recommendations:

    To get             Use the following command
    ---------------    ---------------------------------------
    "normal" CONRAN    ncargsrc conran conterp concom
    "quick" CONRAN     ncargsrc conraq conterp concom
    "smooth" CONRAN    ncargsrc conran conterp concom dashsmth
    "super" CONRAN     ncargsrc conras conterp concom dashsupr
    "quick" CONREC     ncargsrc -conrecquick conrec
    "smooth" CONREC    ncargsrc -conrecsmooth conrec
    "super" CONREC     ncargsrc -conrecsuper conrec
    "normal" DASH      ncargsrc dashchar
    "quick" DASH       ncargsrc dashline
    "smooth" DASH      ncargsrc dashsmth
    "super" DASH       ncargsrc dashsupr

The following table lists all of the named collections of routines in the
source libraries:

    srcncarg.a      -  areas autogrph colconv common concom conpack conran
		       conrec conterp dashchar dashpack ezmap ezmapa gflash gridal
		       hafton histgr isosrf isosrfhr labelbar plotchar polypack
		       pwritx pwrity pwrzi pwrzs pwrzt softfill spps srface
		       stitle strmln support threed velvct wmap
    srcncarg_gks.a  -  awi bwi
    srcagupwrtx.a   -  agupwrtx
    srcconraq.a     -  conraq
    srcconras.a     -  conras
    srcconrcqck.a   -  conrcqck
    srcconrcspr.a   -  conrcspr
    srcdashline.a   -  dashline
    srcdashsmth.a   -  dashsmth
    srcdashsupr.a   -  dashsupr

"END"

  exit 0

endif

# Make sure "ncargpath" works by getting a path to the binary directory.

set b = `ncargpath SED_BINDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$b") then
  echo "Binary directory <$b> does not exist."
  exit 1
endif

# Create directories into which to copy the desired source files.

if (! -e NCARGSOURCE) then
  /bin/mkdir NCARGSOURCE
  /bin/chmod 777 NCARGSOURCE
endif

if (! -e ncargsource) then
  /bin/mkdir ncargsource
  /bin/chmod 700 ncargsource
endif

# Get the desired source files in "NCARGSOURCE".  Because the "set-uid" bit
# of "ncargsrcx" is on, the files will be owned by whoever owns "ncargsrcx".

cd NCARGSOURCE

$b/ncargsrcx $argv

# Copy the source files to "ncargsource" so they will have the right owner
# and set the permissions to read/write for the user only.

cd .. >& /dev/null

/bin/cp NCARGSOURCE/* ncargsource
/bin/rm -rf NCARGSOURCE
/bin/chmod 600 ncargsource/*

# Done.

exit 0
