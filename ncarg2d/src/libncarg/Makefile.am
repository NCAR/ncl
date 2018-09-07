# This is an automake file for NCL.
# Ed Hartnett 8/30/18

SUBDIRS = Iftran areas autogrph colconv common concom conpack		\
conpackt conran conrec conterp dashchar dashpack ezmap ezmapa ezmapb	\
ezmapc gflash gridal hafton histgr isosrf isosrfhr labelbar ngmisc	\
plotchar polypack pwritx pwrity pwrzi pwrzs pwrzt softfill spps		\
srface stitle strmln support tdpack threed vaspackt velvct wmap		\
areasC autogrphC colconvC conpackC conpacktC commonC dashcharC		\
dashpackC ezmapCC gflashC gridalC histgrC isosrfC labelbarC ngmiscC	\
plotcharC polypackC softfillC sppsC srfaceC stitleC supportC strmlnC	\
tdpackC threedC velvctC wmapC

# Assemble libncarg.
lib_LTLIBRARIES = libncarg.la

# These linker flags specify libtool version info.
# See http://www.gnu.org/software/libtool/manual/libtool.html#Libtool-versioning
# for information regarding incrementing `-version-info`.
libncarg_la_LDFLAGS = -version-info 1:0:0

# These convenience libraries are added together to make the NCAR
# graphics library.
libncarg_la_LIBADD = areas/libncarg_areas.la			\
areasC/libncarg_areasc.la autogrph/libncarg_autogrph.la		\
autogrphC/libncarg_autogrphc.la colconv/libncarg_colconv.la	\
colconvC/libncarg_colconvc.la common/libncarg_common.la		\
commonC/libncarg_commonc.la concom/libncarg_concom.la		\
conpack/libncarg_conpack.la conpackC/libncarg_conpackc.la	\
conpackt/libncarg_conpackt.la conpacktC/libncarg_conpacktc.la	\
conran/libncarg_conran.la conrec/libncarg_conrec.la		\
conterp/libncarg_conterp.la dashchar/libncarg_dashchar.la	\
dashcharC/libncarg_dashcharc.la dashpack/libncarg_dashpack.la	\
dashpackC/libncarg_dashpackc.la ezmap/libncarg_ezmap.la		\
ezmapa/libncarg_ezmapa.la ezmapb/libncarg_ezmapb.la		\
ezmapc/libncarg_ezmapc.la ezmapCC/libncarg_ezmapcc.la		\
gflash/libncarg_gflash.la gflashC/libncarg_gflashc.la		\
gridal/libncarg_gridal.la gridalC/libncarg_gridalc.la		\
hafton/libncarg_hafton.la histgr/libncarg_histgr.la		\
histgrC/libncarg_histgrc.la isosrf/libncarg_isosrf.la		\
isosrfC/libncarg_isosrfc.la isosrfhr/libncarg_isosrfhr.la	\
labelbar/libncarg_labelbar.la labelbarC/libncarg_labelbarc.la	\
ngmisc/libncarg_ngmisc.la ngmiscC/libncarg_ngmiscc.la		\
plotchar/libncarg_plotchar.la plotcharC/libncarg_plotcharc.la	\
polypack/libncarg_polypack.la polypackC/libncarg_polypackc.la	\
pwritx/libncarg_pwritx.la pwrity/libncarg_pwrity.la		\
pwrzi/libncarg_pwrzi.la pwrzs/libncarg_pwrzs.la			\
pwrzt/libncarg_pwrzt.la softfill/libncarg_softfill.la		\
softfillC/libncarg_softfillc.la spps/libncarg_spps.la		\
sppsC/libncarg_sppsc.la srface/libncarg_srface.la		\
srfaceC/libncarg_srfacec.la stitle/libncarg_stitle.la		\
stitleC/libncarg_stitlec.la strmln/libncarg_strmln.la		\
strmlnC/libncarg_strmlnc.la support/libncarg_support.la		\
supportC/libncarg_supportc.la tdpack/libncarg_tdpack.la		\
tdpackC/libncarg_tdpackc.la threed/libncarg_threed.la		\
threedC/libncarg_threedc.la vaspackt/libncarg_vaspackt.la	\
velvct/libncarg_velvct.la velvctC/libncarg_velvctc.la		\
wmap/libncarg_wmap.la wmapC/libncarg_wmapc.la

libncarg_la_SOURCES = ncargC.h

# Header get installed in special subdirectory.
ncargdir = $(prefix)/include/ncarg
ncarg_HEADERS = ncargC.h
