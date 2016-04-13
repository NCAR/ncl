#!/usr/bin/ksh
#-----------------------------------------------------------------------
#-- KSH  - NCL Doc Example script:
#--
#-- convert GMT color tables to NCL color tables
#-- used by   gsn_define_colormap(wks,"NCL_GMT-BYR-03")
#--
#-- Example GMT color table file:   GMT-BYR-03.cpt
#--
#-- usage:  gmt2coltab.ksh GMT-BYR-03.cpt
#--         ---> creates new file NCL_GMT-BYR-03.rgb
#--
#-- KMF 03.05.13
#-----------------------------------------------------------------------
in=$1

gmt_coltab=${in##*/}
coltab=NCL_${gmt_coltab%.*}.rgb

#-- count number of colors plus foreground and background color.
#-- BUT, delete the last 3 color entries from the GMT color table
#-- they're not needed --> number of colors + 2 - 3  =  number of colors -1

ncols=$(cat ${in} | grep -v "\#" | wc -l)
ncols=$(expr ${ncols} - 1)

#-- insert background (1) and foreground (0) colors
#-- NCL starts with color index 2
cat << EOF > tmp_col.rgb
ncolors=${ncols}
# r   g   b
0 0 0
1 1 1
EOF

#-- read and write the color values
cat ${in} | grep -v "\#" | sed -e "s/'//g" | awk '{print $2" "$3" "$4}' >> tmp_col.rgb
head -n -3 tmp_col.rgb > ${coltab}

\rm -rf tmp_col.rgb

exit
