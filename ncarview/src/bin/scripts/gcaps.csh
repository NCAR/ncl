#
#	$Id: gcaps.csh,v 1.2 1991-07-31 17:13:40 clyne Exp $
#
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

set dir = `ncargpar GRAPHCAPDIR`

if ($status != 0) then
	exit 1
endif

if (! -d "$dir") then
  echo "Graphcap directory <$dir> does not exist."
  exit 1
endif

cd $dir
echo "The following graphcaps are installed in ${dir}:"
foreach gcap ("" *)
	echo "	$gcap"
end

echo ""
echo "The following device specifiers are also valid:"
foreach device ("" DEVICES)
	echo "	$device"
end
