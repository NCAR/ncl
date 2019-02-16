# This is an automake file for NCL.
# Ed Hartnett 9/5/18

noinst_LTLIBRARIES = libngmath_fitgrid.la

# Find the ngmath.h file.
AM_CPPFLAGS = -I../../../../src/lib

# Find gks.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

libngmath_fitgrid_la_SOURCES = ceezdp.f curv1.f curvd.f curvp1.f	\
curvpi.f curvps.f curvs2.f curvss.f ftstore.f intrvp.f kurv2.f		\
kurvp1.f kurvpd.f strhlp.f surf2.f ceez.f curv2dp.f curvidp.f		\
curvp2dp.f curvppdp.f curvs1dp.f curvsdp.f fcurv2dp.f intrvldp.f	\
kurv1dp.f kurvddp.f kurvp2dp.f snhcshdp.f surf1dp.f termsdp.f curv2.f	\
curvi.f curvp2.f curvpp.f curvs1.f curvs.f fcurv2.f intrvl.f kurv1.f	\
kurvd.f kurvp2.f snhcsh.f surf1.f terms.f curv1dp.f curvddp.f		\
curvp1dp.f curvpidp.f curvpsdp.f curvs2dp.f curvssdp.f ftstoredp.f	\
intrvpdp.f kurv2dp.f kurvp1dp.f kurvpddp.f strhlpdp.f surf2dp.f		\
c_ftcurv.c ftuser.c
#csaplyr.f csaplyrt.f		\
#curvs1.f
# libngmath_cssgrid_la_SOURCES = csaddnod.f csaplyr.f csaplyrt.f		\
# csarcint.f csarclen.f csareas.f csbdyadd.f csblda.f csbnodes.f		\
# cscircum.f csconstr.f cscovsph.f cscrlist.f csdelarc.f csdelnb.f	\
# csdelnod.f csedge.f csfval.f csgetd.f csgeti.f csgetnp.f csgetr.f	\
# csgetsig.f csgivens.f csgradg.f csgradl.f csgrcoef.f cshval.f		\
# csinsert.f csinside.f csintadd.f csintrc0.f csintrc1.f csintrsc.f	\
# csjrand.f csleft.f cslstptr.f csnbcnt.f csnearnd.f csoptim.f		\
# csrotate.f csscoord.f csserr.f cssetd.f csseti.f cssetr.f cssetup.f	\
# cssgprnt.f cssgrid.f cssgridd.f cssig0.f cssig1.f cssig2.f cssmsgs.f	\
# cssmsurf.f cssnhcsh.f csstore.f csstri.f csstrid.f csswap.f		\
# csswptst.f cstrans.f cstrfind.f cstrlist.f cstrlprt.f cstrmesh.f	\
# cstrprnt.f csunif.f csvoro.f csvorod.f cstransd.f csscoordd.f css2c.f	\
# csc2s.f csc2sd.f c_cssuser.c

noinst_HEADERS = ftgvars.h ftproto.h fttypes.h ftuvars.h
