#!/bin/csh -f
#
#	Name		gcaps
#
#	Author		John Clyne
#
#	Date		Wed Jun 27 13:23:39 MDT 1990
#
#	Desc.		Report all the graphcaps installed in the graphcap 
#			directory.
# 

set dir = "SED_LIBDIR"

set dir = "${dir}/graphcaps"

cd $dir
echo "The following graphcaps are installed in ${dir}:"
foreach gcap (*)
	echo "	$gcap"
end

echo ""
echo "The following device specifiers are also valid:"
foreach device (DEVICES)
	echo "	$device"
end
