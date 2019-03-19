# This is an automake file for NCL.
# Ed Hartnett 9/6/18

# Find ncl/defs.h.
AM_CPPFLAGS = -I$(top_srcdir)/ni/src/ncl

# Find gks.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Find ngmath.h.
AM_CPPFLAGS += -I$(top_srcdir)/ngmath/src/lib

# Find hlu.h.
AM_CPPFLAGS += -I$(top_srcdir)/ni/src/lib/hlu

# Find other headers under ncl/ncarg/ncl.
AM_CPPFLAGS += -I$(top_srcdir)/ni/src/ncl/ncarg/ncl

# Link to fortran nfg library.
AM_LDFLAGS = -I$(top_builddir)/ni/src/lib/nfpfort/libnfpfort.la

lib_LTLIBRARIES = libnfp.la

libnfp_la_SOURCES = areavolW.c betaincW.c bytesW.c calcorcW.c		\
cancorW.c cdf_dpW.c chiinvW.c conformW.c csagridW.c cssgridW.c		\
ctwrapW.c cz2ccmW.c datesW.c drwvctc.c dsgridW.c dtrendW.c echoW.c	\
eofW.c erfW.c extaperW.c ezfftW.c finitediffW.c filterxW.c fitgridW.c	\
fluxEddyW.c fourinfoW.c gausW.c gtripleW.c hydroW.c hyi2hyoW.c indW.c	\
int2pW.c invmatrixW.c julGregW.c linint2W.c linmsgW.c linroodW.c	\
locminmaxW.c mapgciW.c mixhumW.c natgridW.c ncolorW.c nglogoW.c		\
ngmiscW.c nicevalsW.c omcalc_ccmW.c outputgif.c paleoW.c prcwatW.c	\
preshybW.c pressigW.c probtestW.c randomW.c rcmW.c regcoefW.c		\
relhumW.c remapW.c rhombtriW.c shgridW.c simpsonW.c sindexW.c slpW.c	\
skewtW.c smth9W.c sortW.c specxW.c sph2W.c sphintW.c sstoiW.c statW.c	\
svdW.c taperW.c tdpackW.c tempnamW.c trvsphW.c varimaxW.c vibetaW.c	\
waveletW.c wgneW.c wmapW.c writematrixW.c wrunaveW.c wrapper.c		\
utilW.c wrfW.c vinth2pW.c vinth2peW.c vintp2peW.c zonmpsiW.c		\
z2geouvW.c stdatmusW.c ripW.c lspolyW.c wgtvertbetaW.c dimavgwgtW.c	\
studentW.c fft2dW.c sg_toolsW.c colconvW.c convert_datum.c cfftW.c	\
wksmoothW.c dzheightW.c covcormW.c kronprodW.c space_timeW.c		\
spcorrW.c bindataW.c tropwmoW.c pdfW.c mocloopsW.c spanW.c		\
vinth2pnodesW.c vinth2penodesW.c objanalW.c areaconremapW.c nctime.c	\
cdtimeW.c dimnumrunW.c sparse_matrixW.c gammaW.c gamfitW.c spiW.c	\
oceanW.c dgeevxW.c areapolyW.c kolsm2W.c kmeansW.c thornW.c		\
wrf_vinterpW.c wrf_cloud_topW.c wrfwpsW.c speiW.c spei_func.c		\
auxiliary.c lmoments.c pdfs.c thornthwaite.c wetbulbW.c mlegevW.c	\
wrf_cloud_fracW.c kdeW.c DirectVincenty.c directVincenty_W.c rgba2png.c	\
rgba2png_W.c KML_Funcs.c add_NCL_KML_Arrow_W.c 				\
add_NCL_KML_2DGrid_W.c add_NCL_KML_UnstructGrid_W.c 			\
TransformCoordinate.c TransformCoordinate_W.c determinantW.c		\
bfilterW.c trendmankenW.c poissonW.c fftshiftW.c weibullW.c


include_HEADERS = wrapper.h nctime.h
