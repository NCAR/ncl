#!/bin/csh -f
#
#	$Id: ncargCex.csh,v 1.2 1992-10-30 18:47:09 haley Exp $
#

set example_dir = `ncargpath SED_EXAMPLESDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$example_dir") then
  echo "Example directory <$example_dir> does not exist."
  exit 1
endif

set test_dir = `ncargpath SED_TESTSDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$test_dir") then
  echo "Test directory <$test_dir> does not exist."
  exit 2
endif

set example_list=(\
c_agex01 c_agex02 c_agex03 c_agex04 c_agex05 c_agex06 c_agex07 c_agex08 \
c_agex09 c_agex10 c_agex11 c_agex12 c_agex13 c_arex01 c_mpex01 c_mpex02 \
c_mpex03 c_mpex04 c_mpex05 c_mpex06 c_mpex07 c_mpex08 c_mpex09 c_mpex10 \
c_cpex02 c_cpex03 c_cpex04 c_cpex05 c_cpex06 c_cpex07 c_cpex08 c_cpex09 \
c_eezmpa c_elblba c_epltch c_cbex01 c_coex01 c_coex02 c_coex03 c_stex01 \
c_sfex01 c_sfex02 c_srex01 c_example c_mpexfi)

set test_list=(\
c_tagupw c_tareas c_tautog c_tconpa c_tezmap c_tezmpa c_tsoftf c_tsrfac \
c_tstrml c_tstitl c_tdashl c_tdashc c_tdashp c_tdashs c_tgflas c_tgrida \
c_thstgr c_tthree c_tpltch c_tpwrzi c_tpwrzt c_tpwrzs c_tisosr c_tvelvc \
c_tcolcv c_tlblba c_tst1 c_tst2 c_tst3 c_tst4 c_tst5 c_tst6 c_tst8 c_tst9 \
c_tst10)

if ($#argv < 1) then
echo "usage: ncargCex [-all] [-allexamples,-E] [-alltests,-T] [-clean] [-n]"
echo "                [-onebyone] names                                    "
echo "                                                                     "
echo "See <man ncargCex>                                                   "
exit
endif

set names

while ($#argv > 0)
    
    switch ($1)

        case "-all":
        case "-A":
            shift
            set names=($example_list $test_list)
            breaksw

        case "-allexamples":
        case "-E":
            shift
            set names=($example_list)
            breaksw

        case "-alltests":
        case "-T":
            shift
            set names=($test_list)
            breaksw
        
        case "-clean":
            shift
            set CleanOption
            breaksw

        case "-n":
            shift
            set NoRunOption
            breaksw
        
        case "-onebyone":
            shift
            set OneByOneOption
            breaksw

        case "-*":
            echo "$0 : Unknown option <$1>"
            exit 1
            breaksw

        default:
            set names=($names $1)
            shift
            breaksw
    endsw
end

foreach name ($names)

set rmfiles

set type="Unknown"

foreach known ($example_list)
    if ("$name" == "$known") then
        set type="Example"
    endif
end

foreach known ($test_list)
    if ("$name" == "$known") then
        set type="Test"
    endif
end

################################################################
#
# Code for handling examples
#
################################################################

if ("$type" == "Example") then

echo ""
echo "NCAR Graphics C Example <$name>"

set c_files = $name.c

foreach file (cpex01 cpex02 cpex03 cpex04 cpex05 cpex06 cpex07 cpex08 cpex09)
    if ("$name" == "c_$file") then
        set c_files=($c_files c_cpexcc.c)
        set rmfiles="c_cpexcc.o"
    endif
end

set copy_files="$c_files"

if ( "$name" == "c_cbex01" ) then
    set copy_files=($copy_files c_cbex01-.f)
    set rmfiles="c_cbex01-.o"
endif

if ( "$name" == "c_mpexfi" ) then
    set copy_files=($copy_files mpexfi.dat)
endif
if ( "$name" == "c_srex01" ) then
    set copy_files=($copy_files srex01.dat)
endif
if ( "$name" == "c_agex13" ) then
    set copy_files=($copy_files agda13.dat)
endif

set rmfiles=($rmfiles $copy_files)

foreach file($copy_files)
    echo "  Copying $file"
    cp $example_dir/$file .
end

if (! $?NoRunOption) then
    echo ""
    echo "Compiling and Linking..."
    if ($name == "c_cbex01") then
        f77 -c c_cbex01-.f
        set c_files=($c_files c_cbex01-.o)
    endif
    ncargcc -o $name $c_files
    if ($status != 0) then
            echo ""
            echo "The compile and link failed"
            exit -1
    endif
    echo ""
    echo "Executing <$name>..."
    switch( $name )
    case c_mpexfi:
        ncargrun -o $name.ncgm $name < mpexfi.dat
        breaksw
    case c_srex01:
        ncargrun -o $name.ncgm $name < srex01.dat
        breaksw
    case c_agex13:
        ncargrun -o $name.ncgm $name < agda13.dat
        breaksw
    default:
        ncargrun -o $name.ncgm $name
    endsw
    set rmfiles = ($rmfiles $name.o $name)
    echo "Metafile is named $name.ncgm"
endif

if ("$name" == "c_stex01") then
    set rmfiles = ($rmfiles GNFB09)
endif

endif

################################################################
#
# Code for handling tests
#
################################################################

if ("$type" == "Test") then

set alias_name="$name"

echo ""
echo "NCAR Graphics Test Program <$name.c>"

rm -f $alias_name.c

set files=$name.c
set rmfiles=($name $name.c $name.o)

cp $test_dir/$name.c .

echo ""
foreach file ($files)
    echo "  Copying $file"
    cp $test_dir/$file .
end

if (! $?NoRunOption) then

set ncargccflags

switch ($name)
# quick routines
    case c_tdashl:
        set ncargccflags = "-quick"
        breaksw

# smooth routines (default)
    case c_tdashs:
        set ncargccflags = "-smooth"
        breaksw

# super routines
    case c_tdashp:
        set ncargccflags = "-super"
        breaksw

# autograph with pwritx for character generation

    case tagupwc:
        set ncargccflags = "-agupwrtx"
        breaksw
endsw

echo ""
echo "Compiling and Linking..."

ncargcc $ncargccflags -o $alias_name $files

if ($status != 0) then
    echo "The compile and link failed"
    exit -1
endif

if ("$name" == "c_tcolcv" || "$name" == "colconv") then
    echo ""
    echo "Executing <$alias_name> - no metafile produced"
    $alias_name
else
    echo ""
    echo "Executing <$alias_name>..."
    ncargrun -o $alias_name.ncgm $alias_name
    echo "Metafile is named $alias_name.ncgm"
endif

endif

if ("$name" == "c_tgflas") then
    set rmfiles = ($rmfiles GNFB01 GNFB02 GNFB03 GNFB04)
endif

if ("$name" == "c_tstitl") then
    set rmfiles = ($rmfiles GNFB09)
endif

endif

# Code for handling inappropriate requests

if ("$type" == "Unknown") then

echo "ncargCex: <$name> is not a known example or test"

endif

# Clean out unwanted files.

if ($?CleanOption) then
    rm -f $rmfiles
endif

if ($?OneByOneOption) then
    ctrans -d X11 -geometry 1142x865+10+0 $name.ncgm
    rm -f $name.ncgm $rmfiles
endif

end
