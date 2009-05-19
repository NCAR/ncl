#!/bin/sh
# This script was provided by Carter Borst, who got it to work
# with V5.1.0.
# 
#  NAME: 	Build-NCARG_NCL5.0.sh
#  PURPOSE:	Install NCARG, NCL, and dependent software packages
#  DEPENDS:	zlib, jpeg-6b, libpng, netcdf, udunits, jasper, flex
#		    Additional dependent software is included in this build 
#			Including: gctp, g2clib, HDF4.2r3, HDF-EOS2.14v1.00, ncl_ncarg-5.0.0
#  CLI ARGS:	1-software version, 2-software prefix, 3-netcdf version 4-default install-yes/no 
#  example line:  sh Build-NCL.sh -v 5.0.0 -p /opt -c pgi -n netcdf-3.6.3-pgi -d 
#
message_hdr() { echo "--------------------" ; echo "$@" ; echo "--------------------" ;}
debug() { [ -n "$SNAT_DEBUG" ] && echo "SNAT_DEBUG: " "$@" 1>&2 ; }
repo=${repo_root_dir:-/opt/repo}
scripts=${scripts_root_dir:-${repo}/app_support/build_inst_scripts}
srcdir=${source_root_dir:-${repo}/app_support/src}
config_data=${config_data_dir:-${repo}/projects/config_data}
func_file=${func_file_loc:-${scripts}/FUNCTIONS.sh}
source ${func_file}
parse_cmd_args "$@" 
software=ncl_ncarg
sw_link=ncl
sw_link2=ncarg
build_sum=${sw_root}/build_summary
base_lib=/usr/lib64
base_inc=/usr/include
instdir=${sw_root}/${software}-${version}-${compiler}

echo Building pre-ncl software in a dev directory ${srcdir}/ncl_dev
## !!! ### Clean up past installs and prep for new install
export ncl_dev=${srcdir}/ncl_dev
export ncl_dev_lib=${ncl_dev}/lib
export ncl_dev_inc=${ncl_dev}/include
export ncl_src=${srcdir}/ncl_src
export ncl_inst=${srcdir}/ncl_inst
rm -rf $ncl_dev $ncl_src $ncl_inst
mkdir -p $ncl_dev $ncl_src $ncl_inst
cd $ncl_dev
mkdir -p bin lib include rangs man
cd $ncl_inst
mkdir -p bin lib include rangs man

# Know the ncl_ncarg src software dependencies::
# OS: flex, libpng, zlib, jpeg-6b
# opt: udunits, netcdf, jasper
# Packages to build: HDF4.2f3, g2clib-1.0.04-NCAR, HDF-EOS2.14v1.00, gctp
# Actual software package: ncl_ncar-${version}
szip_src_file=szip-2.1.tar.gz
hdf4_src_file=HDF4.2r3.tar.gz
hdf_eos_src_file=HDF-EOS2.14v1.00.tar.gz
gctp_src_file=gctp.tar.gz
g2clib_src_file=g2clib-1.0.4-NCAR.tar.gz
sw_src_file=${software}_src-${version}.tar.gz

## Setting Environment
## From Mary Haley, 15-16 Sept 2008
if [ $compiler == "pgi" ]; then
	echo Building NCL with PGI . . .
	export NCARG_ROOT=${ncl_dev}
	export NCARG=${ncl_src}/${software}-${version}
	export YMAKE_DEV_FILE=${NCARG}/ymakedevfile
	export PGI=/opt/pgi
	export PATH=${ncl_dev}/bin:${PATH}:${PGI}/bin
	export CC=pgcc
	export F90=pgf90
	export F77=pgf90
	export FC=pgf90
	export CFLAGS="-fPIC -DpgiFortran"
	export CPPFLAGS="-fPIC"
	export CXXFLAGS="-fPIC"
	export FFLAGS="-fPIC"
	export F90FLAGS="-fPIC"
elif [ $compiler == "gcc" ]; then
	echo Building NETCDF with GCC . . .
	export CC=/usr/bin/gcc
	export F90=/usr/bin/gfortran
	export FC=/usr/bin/gfortran
	export CXX=/usr/bin/g++
else
	message_hdr Unknown compiler, exiting
	exit 0
fi

## Pre install
cd ${ncl_src}
tar zxf ${srcdir}/${sw_src_file}
tar zxf ${srcdir}/${hdf4_src_file}
tar zxf ${srcdir}/${hdf_eos_src_file}
tar zxf ${srcdir}/${g2clib_src_file}
tar zxf ${srcdir}/${gctp_src_file}
tar zxf ${srcdir}/${szip_src_file}
chown -Rh root:root ${ncl_src}

##  Copy in pre-built, non-OS software include / lib files into *dev/ directories
cp -r ${netcdf_version}/lib/* ${ncl_dev_lib}
cp -r ${udunits_version}/lib/* ${ncl_dev_lib} 
cp -r /opt/jasper/lib/* ${ncl_dev_lib}
cp -r ${netcdf_version}/include/* ${ncl_dev_inc}
cp -r ${udunits_version}/include/* ${ncl_dev_inc}
cp -r /opt/jasper/include/jasper ${ncl_dev_inc}

export MAKEFLAGS="-l 6"

export CPPFLAGS="-I${base_inc} -fPIC -I${ncl_dev}/include -DBIG_LONGS -DSWAP "
export LDFLAGS=-L/opt/repo/app_support/src/ncl_dev/lib

###  Start building packages from source and copying files to the *dev* structure
###
### Build szip for HDF4 ?
cd ${ncl_src}/szip-2.1
./configure --prefix=${ncl_dev} --disable-shared  2>&1 | tee ${build_sum}/szip-2.1-config.`date +%Y%m%d`.out
make all install 2>&1 | tee ${build_sum}/szip-2.1-makeinstall.`date +%Y%m%d`.out
##  How is this implemented in the build for HDF4 via Haley's build?

###
### Build HDF4.2r3 with these options !! important !!
cd ${ncl_src}/HDF4.2r3
./configure --prefix=${ncl_dev} --with-zlib=${base_lib} --with-jpeg=${base_lib} --includedir=${ncl_dev_inc}/hdf --disable-netcdf  2>&1 | tee ${build_sum}/HDF4.2r3-config.`date +%Y%m%d`.out
make all install 2>&1 | tee ${build_sum}/HDF4.2r3-makeinstall.`date +%Y%m%d`.out
cd ${ncl_dev_inc}/hdf
cp -v *.h ../

###
###  Build g2clib-1.0.4-NCAR
cd ${ncl_src}/g2clib-1.0.4-NCAR
cp Makefile Makefile.orig
ed -s Makefile <<"EOF"
H
g/^DEFS/d
g/^CFLAGS/d
g/^CC/d
i
DEFS   = -DUSE_JPEG2000 -DUSE_PNG
CFLAGS = -fPIC $(INC) $(DEFS)
CC     = pgcc
.
wq
EOF
ed -s Makefile <<EOF
H
g/^INC/d
i
INC    = -I${base_inc} -I${ncl_dev_inc}
.
wq
EOF
make all 2>&1 | tee ${build_sum}/g2clib-makeoutput.`date +%Y%m%d`.out
cp -v libgrib2c.a ${ncl_dev_lib}
cp -v grib2.h ${ncl_dev_inc}

###
### Building HDF-EOS2.14v1.00:
cd ${ncl_src}/HDF-EOS2.14v1.00
cp -v bin/INSTALL-HDFEOS bin/INSTALL-HDFEOS.orig
ed -s bin/INSTALL-HDFEOS <<"EOF"
H
/linux)
/CC=gcc/d
i
	PGI=/opt/pgi
	PATH=${PGI}/bin:${PATH}
        CC=pgcc
.
/CFLAGS=/d
i
        CFLAGS="$opt_flag -fPIC"
.
/F77=g77*/d
i
       F77=pgf90
.
/F77FLAGS="$opt_flag"/d
i
        F77FLAGS="$opt_flag -fPIC"
.
wq
EOF
echo HDFINC=${ncl_dev_inc} HDFLIB=${ncl_dev_lib} bin/INSTALL-HDFEOS
HDFINC=${ncl_dev_inc} HDFLIB=${ncl_dev_lib} bin/INSTALL-HDFEOS
cp -v lib/linux/libhdfeos.a ${ncl_dev_lib}
cp -v include/*.h ${ncl_dev_inc}

###
### Building gctp
cd ${ncl_src}/gctp/src
cp -v Makefile Makefile.orig
cp -v makelinux.pgi Makefile
#  Edit Makefile -  change gcc to "pgcc" and add "-fPIC" on the compile line.
ed -s Makefile <<EOF
H
g/CC/d
i
CC  =  pgcc -fPIC -DLINUX -Dunix
.
wq
EOF
make 2>&1 | tee ${build_sum}/gctp-makeoutput.`date +%Y%m%d`.out
cp -v ../lib/linux/libGctp.a  ${ncl_dev_lib}
cp -v ../include/*.h  ${ncl_dev_inc}


###
### Start configuration & build of NCL 
cd $NCARG
cat >> ${ncl_src}/${software}-${version}/ymakedevfile <<EOF
#undef YmakeRoot
#define YmakeRoot ${ncl_inst}
EOF

cp -v config/LINUX LINUX.orig
cp -v config/Project Project.orig
cp -v Makefile Makefile.orig
cp -v ni/src/ncl/NclHDF.c NclHDF.c.orig
cp -v ni/src/ncl/NclHDFEOS.c NclHDFEOS.c.orig
cp -v ${config_data}/NclHDFEOS.c.NCL ni/src/ncl/NclHDFEOS.c 
cp -v ${config_data}/NclHDF.c.NCL ni/src/ncl/NclHDF.c 
cp -v ${config_data}/LINUX.NCL config/LINUX
cp -v ${config_data}/Project.NCL config/Project
cp -v ${config_data}/triangle.* ${ncl_src}/${software}-${version}/ni/src/lib/hlu/
# cp -v ${config_data}/Makefile.NCL Makefile

message_hdr Building NCL with user input
echo Almost ready to start the NCL install, but please take notes here:
echo answer YES for the following:
# echo          Build NCL,  HDF support,  Triangle support,  Udunits support,  HDF-EOS support,  GRIB2 support
echo          Build NCL,  Triangle support,  Udunits support,  HDF-EOS support,  GRIB2 support
echo answer NO for the following:
echo          HDF4 w/szip support, NetCDF-4 support,  Vis5D+ support, OPeNDAP support
# echo          NetCDF-4 support,  Vis5D+ support, OPeNDAP support
echo  For local library search path, enter:
echo    /opt/repo/app_support/src/ncl_dev/lib /usr/lib64 ${netcdf_version}/lib
echo  For local include search path, enter:
echo    /opt/repo/app_support/src/ncl_dev/include /usr/include ${netcdf_version}/include 
message_hdr
read -p "Please hit a key when you are ready to continue:" yes_no

./Configure -v 2>&1 | tee ${build_sum}/ncl_configure.out.`date +%Y%m%d`.out

make Everything 2>&1 | tee ${build_sum}/ncl_makeoutput.`date +%Y%m%d`.out

rm -rf ${sw_root}/${software} ${sw_root}/${sw_link} ${sw_root}/${sw_link2} 
ln -s ${instdir} ${sw_root}/${sw_link}
ln -s ${instdir} ${sw_root}/${software}
ln -s ${instdir} ${sw_root}/${sw_link2}

rsync -avx ${ncl_inst}/ ${instdir}/
for i in ${srcdir}/rangs*zip ${srcdir}/gshh*zip ; do unzip -od ${instdir}/rangs/ $i ; done

chown -Rh root:root $instdir
cd ${repo}

# exby temporary need for ATEC range application
# borst temp fix for above
/bin/ed -s /etc/ld.so.conf <<EOF
H
g/ncl/d
g/ncarg/d
a
/opt/ncarg/lib
/opt/ncl/lib
.
wq
EOF
/sbin/ldconfig

readme_log
exit 0
##================================
##================================
Installing RPM or std OPT packages
#  Installed from source in /opt/udunits-1.12.9
#  Installed from source in /opt/netcdf-3.6.3-pgi
#  tar zxvf jpeg-6b.tgz
#  rpms installed by default in both 32 and 64bit
#  libjpeg-6b-37  &  libjpeg-devel-6b-37
#  README in source tree and in /usr/share/doc/libjpeg-6b are the same
#  tar zxvf jasper-1.900.1.zip
#  Installed from source in /opt/jasper-1.900.1 with NCAR patches
#  libraries and includes installed under /opt/jasper/ 
#  No alterations from std install for ATEC stuff noted
#  tar zxvf zlib-1.2.3.tgz
#  rpms installed by default in both 32 and 64bit
#  zlib-1.2.3-3  &   zlib-devel-1.2.3-3
#  README in source tree and in /usr/share/doc/zlib-1.2.3 are the same
#  tar zxvf flex-2.5.3.tar.gz
#  rpm installed by default in 64bit
#  flex 2.5.4a
#  README in source tree and in /usr/share/doc/flex-2.5.4a are the same
#  tar zxvf libpng-1.2.29.tar.gz
#  rpm installed by default in 32 and 64bit
#  libpng-devel-1.2.10-7.1.el5_0.1 & libpng-1.2.10-7.1.el5_0.1
#  README in source tree and in /usr/share/doc/libpng-1.2.[10 29] are different
#  source version is much newer than rpm install 1.2.10 vs 1.2.29
#cp -r /usr/lib64/libz* /usr/lib64/libjpeg* /usr/lib64/libpng* /usr/lib64/pkgconfig/libpng.pc /usr/lib64/pkgconfig/libpng12.pc ${ncl_dev_lib}
# cp -r /usr/include/zconf.h /usr/include/zlib.h /usr/include/zutil.h /usr/include/jconfig.h /usr/include/jerror.h /usr/include/jmorecfg.h /usr/include/jpeglib.h /usr/include/libpng12 /usr/include/libpng12/png.h /usr/include/libpng12/pngconf.h /usr/include/png.h /usr/include/pngconf.h ${ncl_dev_inc}

Installing RPM or std OPT packages
[root@rap-server build_inst_scripts]# cd ../src/ncl_src/
[root@rap-server ncl_src]# ls -al
total 32
drwxr-xr-x  8 root  root  4096 Dec 18 22:47 .
drwxrwxrwx 16 root  root  4096 Dec 18 22:46 ..
drwxrwxr-x  2 haley games 4096 Aug 22 21:35 g2clib-1.0.4-NCAR
drwxr-xr-x  7 haley games 4096 Dec 11  2002 gctp
drwxr-xr-x  7 haley games 4096 Dec 11  2002 Gctp
drwxr-xr-x  9 haley games 4096 Jul 23 21:06 HDF4.2r3
drwxrwxr-x 11 haley games 4096 Jul 20  2005 HDF-EOS2.14v1.00
drwxrwxr-x  9 haley games 4096 Oct 30  2007 ncl_ncarg-5.0.0

# Steps done Pre-install time::
  4b. Netcdf pre-built for Mary, no steps noted.
  5. I built jpeg-6b with:
       cd /home/haley/src/jpeg-6b
       ./configure --prefix=/home/haley/dev
       make all install install-lib install-headers >&! make-output.091508
     (I didn't worry that it couldn't install man pages.)
  6. I built zlib-1.2.3 with:
       cd /home/haley/src/zlib-1.2.3
        ./configure --prefix=/home/haley/dev
       make all install >&! make-output.091508
  8. I built jasper-1.900.1 with:
       cd /home/haley/src/jasper-1.900.1
       ./configure --prefix=/home/haley/dev
       make all install >&! make-output.091508
  9. I built libpng-1.2.29 with:
        cd /home/haley/src/libpng-1.2.29
        ./configure --prefix=/home/haley/dev --with-pic --disable-shared
        make all install >&! make-output.091508
 13. To install udunits-1.12.4:
       cd /home/haley/src/udunits-1.12.4/src
       ./configure --prefix=/home/haley/dev
       make all install >&! make-output.091508
       You will probably see some errors here regarding "perl". Don't
       worry about these. Type:
       make install
       and make sure libudunits.a and libudport.a get installed to
       /home/haley/dev/lib, and udunits.h to /home/haley/dev/include.
 14. To install flex-2.5.3:
       cd /home/haley/src/flex-2.5.3
       ./configure --prefix=/home/haley/dev
       make all install >&! make-output.091508

###
### Non-automated ncl configuration steps
###
# cd $NCARG
#cp -v config/LINUX.64.PGI config/LINUX
#ed -s config/Project <<EOF
#H
#g/lgrib2c/d
#i
##define GRIB2lib   -lgrib2c -ljasper -lpng -lz
#.
#wq
#EOF
#
#ed -s config/LINUX <<EOF
#H
#g/k8-64/d
#i
##define CCompiler     pgcc 
##define Ccoptions     -fPIC -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
##define FCompiler     pgf90 
##define Fcoptions     -fPIC -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
#.
#g/ArchRecLibSearch/d
#g/ArchRecIncSearch/d
#.
#wq
#EOF
#ed -s ni/src/ncl/NclHDF.c <<EOF
#H
#g/netcdf.h/d
#.
#/ctype.h
#a
##define MAX_VAR_DIMS H4_MAX_VAR_DIMS
##define MAX_NC_NAME H4_MAX_NC_NAME
##define MAX_NC_DIMS H4_MAX_NC_DIMS
#.
#wq
#EOF
#ed -s ni/src/ncl/NclHDFEOS.c <<EOF
#H
#g/netcdf.h/d
#.
#/HdfEosDef.h
#a
##define MAX_VAR_DIMS H4_MAX_VAR_DIMS
##define MAX_NC_NAME H4_MAX_NC_NAME
##define MAX_NC_DIMS H4_MAX_NC_DIMS
#.
#wq
#EOF

#   message_hdr Building NCL with user input
#   echo Almost ready to start the NCL install, but please take notes here:
#   echo answer YES for the following:
#   # echo          Build NCL,  HDF support,  Triangle support,  Udunits support,  HDF-EOS support,  GRIB2 support
#   echo          Build NCL,  Triangle support,  Udunits support,  HDF-EOS support,  GRIB2 support
#   echo answer NO for the following:
#   echo          HDF4 w/szip support, NetCDF-4 support,  Vis5D+ support, OPeNDAP support
#   # echo          NetCDF-4 support,  Vis5D+ support, OPeNDAP support
#   echo  For local library search path, enter:
#   echo    /opt/repo/app_support/src/ncl_dev/lib
#   echo  For local include search path, enter:
#   echo    /opt/repo/app_support/src/ncl_dev/include 
#   message_hdr
#   read -p "Please hit a key when you are ready to continue:" yes_no

# message_hdr  To finalize the install, when not installing from a Control script, run this command:
# rm -rf /tmp/graph_font-cap.fix
# cat >> /tmp/graph_font-cap.fix <<EOF
# #!/bin/sh
# export PGI=/opt/pgi
# export PATH=${PATH}:${PGI}/bin 
# cd $NCARG
# make install
# cd /opt/repo/app_support/src/ncl_dev
# rsync -avx . /opt/ncl/
# rm -rf /tmp/graph_font-cap.fix
# EOF
# chmod 700 /tmp/graph_font-cap.fix
# message_hdr  sh /tmp/graph_font-cap.fix 2>&1 | tee /opt/build_summary/ncl_graphc_fix.`date +%Y%m%d`.out

ed -s Makefile <<EOF
H
g/LIBSEARCH/d
i
LIBSEARCH               = -L${ncl_dev_lib} -L/usr/lib64 -L/opt/netcdf-3.6.3-pgi/lib
.
g/INCSEARCH/d
i
INCSEARCH               = -I${ncl_dev_inc} -I/usr/include -I/opt/netcdf-3.6.3-pgi/include
.
g/LIB_SEARCH/d
i
LIB_SEARCH              = -L${ncl_dev_lib} -L/usr/lib64 -L/opt/netcdf-3.6.3-pgi/lib
.
g/INC_SEARCH/d
i
INC_SEARCH              = -I${ncl_dev_inc} -I/usr/include -I/opt/netcdf-3.6.3-pgi/include
.
wq
EOF
