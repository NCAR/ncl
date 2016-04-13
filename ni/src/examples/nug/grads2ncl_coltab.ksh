#!/bin/ksh
#-----------------------------------------------------------------------
#-- KSH  -  NCL Doc Example script:
#--
#-- convert GrADS color tables to NCL color tables
#-- used by   gsn_define_colormap(wks,"NCL_BYR-03")
#--
#-- Example GrADS colo table file:    BYR-03
#--
#-- usage:  grads2ncl_coltab.ksh BYR-03
#--         ---> creates new file NCL_BYR-03.rgb
#--
#-- KMF 03.05.13
#-----------------------------------------------------------------------
in=$1

grads_coltab=${in##*/}
coltab=NCL_${grads_coltab%.*}.rgb

#-- count the number of colors plus foreground and background color.
ncols=$(cat ${in} | grep -v "\*" | grep -v "\#" | wc -l)
ncols=$(expr ${ncols} + 2)

#-- insert background (1) and foreground (0) colors
#-- NCL starts with color index 2
cat << EOF > ${coltab}
ncolors=${ncols}
# r   g   b
0 0 0
1 1 1
EOF

#-- read and write the color values
cat ${in} | grep -v "\*" | grep -v "\#" | sed -e "s/'//g" | awk '{print $4" "$5" "$6}' >> ${coltab}

exit
