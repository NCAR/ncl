#!/bin/csh -f
#
#   $Id: ncargex.csh,v 1.15 1993-01-26 16:29:00 haley Exp $
#

set example_dir=`ncargpath SED_EXAMPLESDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$example_dir") then
  echo "Example directory <$example_dir> does not exist."
  exit 1
endif

set test_dir=`ncargpath SED_TESTSDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$test_dir") then
  echo "Test directory <$test_dir> does not exist."
  exit 2
endif

set tutor_dir=`ncargpath SED_TUTORIALDIR`
if ($status != 0) then
        exit 1
endif

if (! -d "$tutor_dir") then
  echo "Test directory <$tutor_dir> does not exist."
  exit 2
endif

set example_list=(\
agex01 agex02 agex03 agex04 agex05 agex06 agex07 agex08 agex09 agex10 \
agex11 agex12 agex13 arex01 mpex01 mpex02 mpex03 mpex04 mpex05 mpex06 \
mpex07 mpex08 mpex09 mpex10 mpexfi cpex01 cpex02 cpex03 cpex04 cpex05 \
cpex06 cpex07 cpex08 cpex09 eezmpa elblba epltch cbex01 coex01 coex02 \
coex03 srex01 slex01 sfex01 sfex02 stex01 stex02 stex03 vvex01 vvex02 \
example nglogy ngrevx ngset1 ngset2)

set test_list=(\
tagupw tareas tautog tcnqck tcnsmt tcnsup tcolcv tconan tconaq \
tconas tconpa tconre tdashc tdashl tdashp tdashs tezmap tezmpa \
tgflas tgrida thafto thstgr tisohr tisosr tlblba tpltch tpwrtx \
tpwry tpwrzi tpwrzs tpwrzt tsoftf tsrfac tstitl tstrml tthree \
tvelvc)

set tutor_conpack=(\
ccpback ccpcff ccpcfx ccpcir ccpcis ccpcit ccpclc ccpcld ccpcldm ccpcldr \
ccpcll ccpclu ccpcnrc ccpdflt ccpezct ccpfil ccpga ccphand ccphl ccphlt \
ccpila ccpils ccpilt ccpklb ccplbam ccplbdr ccpline ccpllb ccpllc ccplll \
ccpllo ccpllp ccpllt ccpllw ccpmap ccpmpxy ccpncls ccpnet ccpnof ccpnsd \
ccppc ccppc1 ccppc2 ccppc3 ccppc4 ccppkcl ccprc ccprect ccprwc ccprwu \
ccpscam ccpset ccpsps1 ccpsps2 ccpspv ccptitle ccpvp ccpvs cidsfft colcon)

set tutor_areas=(cardb1 caredg carline cardb2 carfill carmap)

set tutor_ezmap=(\
cezmap1 cezmap2 cezmap3 cmpclr cmpdd cmpdrw cmpel cmpfil cmpgci \
cmpgrd cmpgrp cmpita cmpitm cmplab cmplbl cmplot cmpmsk cmpou cmppos \
cmpsat cmpsup cmptit cmptra cmpusr)
 
set tutor_softfill=(csfwrld csfsgfa)

set tutor_list=( $tutor_areas $tutor_ezmap $tutor_conpack $tutor_softfill )

set alias_list=(\
agupwrtx areas autograph conrecq conrecs conrecsup colconv conran \
conranq conransup conpack conrec dashchar dashline dashsmth \
dashsupr ezmap ezmapa gflash gridal hafton histgr isosrf isosrfhr \
labelbar plotchar pwritx pwrity pwrzi pwrtz pwrzs pwrzt softfill \
srface stitle strmln threed velvct)

if ($#argv < 1) then
echo "usage: ncargex [-all,-A] [-allexamples,-E] [-alltests,-T]"
echo "               [-alltutorial,-U] [-clean] [-n] [-onebyone] names"
echo "                                                              "
echo "See <man ncargex>                                             "
exit
endif

set X11_option = "-noX11"
set names

while ($#argv > 0)
    
    switch ($1)

        case "-all":
        case "-A":
            shift
            set names=($example_list $test_list $tutor_list)
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
        
        case "-alltutorial":
        case "-U":
            shift
            set names=($tutor_list "mpex03" "mpex05" "arex01" "sfex01" "tsoftf")
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

        case "-unique"
            shift
            set Unique
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

foreach known ($tutor_list)
    if ("$name" == "$known") then
        set type="Tutorial"
    endif
end

foreach known ($alias_list)
    if ("$name" == "$known") then
        set type="TestAlias"
    endif
end

################################################################
#
# Code for handling examples
#
################################################################

if ("$type" == "Example") then

echo ""
echo "NCAR Graphics Fortran Example <$name>"

if ($?Unique && -f $name.ncgm) goto theend

set f_files = $name.f

foreach file (cpex01 cpex02 cpex03 cpex04 cpex05 cpex06 cpex07 cpex08 cpex09)
    if ("$name" == "$file") then
        set f_files=($f_files cpexcc.f)
        set rmfiles="cpexcc.o"
    endif
end

foreach file (vvex01 vvex02)
    if ("$name" == "$file") then
        set f_files=($f_files vvexcc.f)
        set rmfiles="vvexcc.o"
    endif
end

foreach file (mpex01 mpex02 mpex03 mpex04 mpex05 mpex06 mpex07 mpex08 mpex09 mpex10 mpexfi)
    if ("$name" == "$file") then
        set f_files=($f_files mpexcc.f)
        set rmfiles="mpexcc.o"
    endif
end

set copy_files="$f_files"

if ( "$name" == "mpexfi" ) then
    set copy_files=($copy_files mpexfi.dat)
endif
if ( "$name" == "srex01" ) then
    set copy_files=($copy_files srex01.dat)
endif
if ( "$name" == "agex13" ) then
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
    ncargf77 $X11_option -o $name $f_files
    if ($status != 0) then
        echo ""
        echo "The compile and link failed"
        exit -1
    endif
    echo ""
    echo "Executing <$name>..."

    switch( $name )
    case mpexfi:
        ncargrun -o $name.ncgm $name < mpexfi.dat
        breaksw
    case srex01:
        ncargrun -o $name.ncgm $name < srex01.dat
        breaksw
    case agex13:
        ncargrun -o $name.ncgm $name < agda13.dat
        breaksw
    default:
        ncargrun -o $name.ncgm $name
    endsw
    set rmfiles = ($rmfiles $name.o $name)
    echo "Metafile is named $name.ncgm"
endif

if ("$name" == "slex01") then
    set rmfiles = ($rmfiles GNFB09)
endif

endif

################################################################
#
# Code for handling tutorial exercises
#
################################################################

if ("$type" == "Tutorial") then

echo ""
echo "NCAR Graphics Tutorial Exercise <$name>"

if ($?Unique && -f $name.ncgm) goto theend

set f_files = $name.f

set copy_files="$f_files"

if ( "$name" == "ccpcir" || "$name" == "ccpcnrc" || "$name" == "ccpezct" || \
     "$name" == "ccphl" || "$name" == "ccpmap" || "$name" == "ccpvp" ) then
    set copy_files=($copy_files ggdini.f)
    set f_files=($f_files ggdini.f)
    set rmfiles="ggdini.o"
endif

if ( "$name" == "ccpmpxy" ) then
    set copy_files=($copy_files cpmpxy1.dat cpmpxy2.dat)
    set f_files=($f_files)
    set rmfiles=(cpmpxy1.dat cpmpxy2.dat)
endif

if ( "$name" == "ccpila" ) then
    set copy_files=($copy_files ccpila.dat)
    set f_files=($f_files)
    set rmfiles="ccpila.dat"
endif

if ( "$name" == "ccpils" || "$name" == "ccpilt" || "$name" == "ccplbdr" || \
     "$name" == "ccptitle" ) then
    set copy_files=($copy_files ccpex.dat)
    set f_files=($f_files)
    set rmfiles="ccpex.dat"
endif

set rmfiles=($rmfiles $copy_files)

foreach file($copy_files)
    echo "  Copying $file"
    cp $tutor_dir/$file .
end

if (! $?NoRunOption) then
    echo ""
    echo "Compiling and Linking..."
    ncargf77 $X11_option -o $name $f_files
    if ($status != 0) then
        echo ""
        echo "The compile and link failed"
        exit -1
    endif
    echo ""
    echo "Executing <$name>..."

    ncargrun -o $name.ncgm $name
    set rmfiles = ($rmfiles $name.o $name)
    if ( "$name" == "ccpcff" ) then
      rm -f ccpcff.ncgm 
    else
      echo "Metafile is named $name.ncgm"
    endif
endif

endif

################################################################
#
# Code for handling tests
#
################################################################

if ("$type" == "Test" || "$type" == "TestAlias") then

set alias_name="$name"

set rmfiles = ($alias_name.f $alias_name.o $alias_name)

if ("$type" == "TestAlias") then

switch ($alias_name)

case autograph:
    set name=tautog
    breaksw

case agupwrtx:
    set name=tagupw
    breaksw

case areas:
    set name=tareas
    breaksw

case conrec:
    set name=tconre
    breaksw

case conrecq:
    set name=tcnqck
    breaksw

case conrecs:
    set name=tcnsmt
    breaksw

case conrecsup:
    set name=tcnsup
    breaksw

case conran:
    set name=tconan
    breaksw

case conranq:
    set name=tconaq
    breaksw

case conransup:
    set name=tconas
    breaksw

case dashchar:
    set name=tdashc
    breaksw

case dashline:
    set name=tdashl
    breaksw

case dashsupr:
    set name=tdashp
    breaksw

case dashsmth:
    set name=tdashs
    breaksw

case ezmap:
    set name=tezmap
    breaksw

case ezmapa:
    set name=tezmpa
    breaksw

case gridal:
    set name=tgrida
    breaksw

case hafton:
    set name=thafto
    breaksw

case histgr:
    set name=thstgr
    breaksw

case isosrfhr:
    set name=tisohr
    breaksw

case isosrf:
    set name=tisosr
    breaksw

case pwritx:
    set name=tpwrtx
    breaksw

case pwrity:
    set name=tpwry
    breaksw

case pwrzi:
    set name=tpwrzi
    breaksw

case pwrzs:
    set name=tpwrzs
    breaksw

case pwrzt:
    set name=tpwrzt
    breaksw

case srface:
    set name=tsrfac
    breaksw

case strmln:
    set name=tstrml
    breaksw

case threed:
    set name=tthree
    breaksw

case velvct:
    set name=tvelvc
    breaksw

case colconv:
    set name=tcolcv
    breaksw

case plotchar:
    set name=tpltch
    breaksw

case conpack:
    set name=tconpa
    breaksw

case gflash:
    set name=tgflas
    breaksw

case stitle:
    set name=tstitl
    breaksw

case labelbar:
    set name=tlblba
    breaksw

default:
    echo "ncargex: Unfamiliar with <$name>"
    exit 1

endsw

echo "Note: <$alias_name> is an alias for <$name>"
endif

echo ""
echo "NCAR Graphics Test Program <$name.f>"

if ($?Unique && -f $name.ncgm) goto theend

rm -f $alias_name.f

if ($name != tcolcv) then
cat <<'EOF' >>$alias_name.f
C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
'EOF'
endif

cat <<'EOF' >>$alias_name.f
C
C INVOKE DEMO DRIVER
C
'EOF'

if ($name == tisohr) then
cat <<EOF >>$alias_name.f
      CALL ISOSCR()
EOF
endif

echo "      call $name(ierr)" >>$alias_name.f

if ($name != tcolcv) then
cat <<'EOF' >>$alias_name.f
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
'EOF'
endif

cat <<'EOF' >>$alias_name.f
      STOP
      END
'EOF'

echo ""
echo "Copying source code..."

cat $test_dir/$name.f >>$alias_name.f

if (! $?NoRunOption) then

set ncargf77flags

switch ($name)
# quick routines
    case tdashl:
    case tcnqck:
    case tconaq:
        set ncargf77flags = "-quick"
        breaksw

# smooth routines (default)
    case tdashs:
    case tcnsmt:
    case tconan:
        set ncargf77flags = "-smooth"
        breaksw

# super routines
    case tdashp:
    case tcnsup:
    case tconas:
        set ncargf77flags = "-super"
        breaksw

# autograph with pwritx for character generation

    case tagupw:
        set ncargf77flags = "-agupwrtx"
        breaksw
endsw

echo ""
echo "Compiling and Linking..."

ncargf77 $X11_option $ncargf77flags -o $alias_name $alias_name.f

if ($status != 0) then
    echo ""
    echo "The compile and link failed"
    exit -1
endif

if ("$name" == "tcolcv" || "$name" == "colconv") then
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

if ("$name" == "tgflas") then
    set rmfiles = ($rmfiles GNFB01 GNFB02 GNFB03 GNFB04)
endif

if ("$name" == "tstitl") then
    set rmfiles = ($rmfiles GNFB09)
endif

endif

# Code for handling inappropriate requests

if ("$type" == "Unknown") then

echo "ncargex: <$name> is not a known example or test"

endif

# Clean out unwanted files.

if ($?CleanOption) then
    rm -f $rmfiles
endif

if ($?OneByOneOption) then
    ctrans -d X11 -geometry 1142x865+10+0 $name.ncgm
    rm -f $name.ncgm $rmfiles
endif

theend:

end

