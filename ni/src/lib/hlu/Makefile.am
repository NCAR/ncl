# This is an automake file for NCL.
# Ed Hartnett 9/2/18

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

AM_LDFLAGS = ${top_builddir}/ncarg2d/src/libncarg/libncarg.la

lib_LTLIBRARIES = libhlu.la

libhlu_la_SOURCES = AnnoManager.c App.c Base.c BoundingBox.c		\
bsearch.c CairoWorkstation.c Callbacks.c Close.c cmpf.c CnRenderer.c	\
CnStdRenderer.c CnTriMeshRenderer.c color.c ContourPlot.c Convert.c	\
Converters.c CoordArrays.c CoordArrTable.c Create.c			\
CurvilinearTransObj.c DataComm.c DataItem.c DataMgr.c Destroy.c		\
Draw.c Error.c Format.c Fortran.c Futil.c GetValues.c GraphicStyle.c	\
hlu.c ImageWorkstation.c IrregularPlot.c IrregularTransObj.c		\
IrregularType2TransObj.c LabelBar.c Legend.c linaprox.c LogLinPlot.c	\
LogLinTransObj.c MapDataHandler.c MapPlot.c MapRGDataHandler.c		\
MapTransObj.c MapV40DataHandler.c MapV41DataHandler.c			\
MeshScalarField.c MultiText.c NcgmWorkstation.c nicevals.c NresDB.c	\
Open.c pageutil.c Palette.c PDFWorkstation.c PlotManager.c		\
Primitive.c PSWorkstation.c Quarks.c Reparent.c ResList.c Resources.c	\
ScalarField.c Segments.c SetValues.c simpletrans.c			\
SphericalGeometry.c SphericalTransObj.c SplineCoord.c			\
StreamlinePlot.c Style.c SubStyles.c TextItem.c TickMark.c Title.c	\
Trans.c Transform.c TransObj.c TriMeshTransObj.c VarArg.c VecAnno.c	\
VectorField.c VectorPlot.c View.c wksisact.c wksisopn.c Workspace.c	\
Workstation.c XWorkstation.c XyPlot.c DATACOMMF.f ERRORF.f HLUF.f	\
PLOTMANAGERF.f RLGEN.f RLSET.f RLGET.f TRANSFORMF.f VIEWF.f		\
WORKSPACEF.f WORKSTATIONF.f DATAITEMF.f APPF.f dashchar.f coeff1.f	\
interp.f sdot.f sgesl.f trip.f fitpack.f isamax.f search.f sscal.f	\
saxpy.f sgefa.f terp1.f gerhnd.f trmrectgrid.f gproj.f map_util.f	\
hluarpram.f

include_HEADERS = AnnoManager.h AnnoManagerP.h App.h AppI.h AppP.h	\
Base.h BaseI.h BaseP.h CairoWorkstation.h CairoWorkstationP.h		\
Callbacks.h CallbacksP.h CnRenderer.h CnRendererP.h CnStdRenderer.h	\
CnStdRendererP.h CnTriMeshRenderer.h CnTriMeshRendererP.h color.h	\
ContourPlot.h ContourPlotI.h ContourPlotP.h ConvertersP.h Convert.h	\
ConvertP.h CoordApprox.h CoordArrays.h CoordArraysP.h			\
CoordArrTableFloatP.h CoordArrTable.h CoordArrTableP.h			\
CurvilinearTransObj.h CurvilinearTransObjP.h DataCommF.h DataComm.h	\
DataCommP.h DataItem.h DataItemP.h DataMgrF.h DataMgr.h DataMgrP.h	\
defs.h Error.h ErrorI.h ErrorP.h FormatI.h FortranP.h GraphicStyle.h	\
GraphicStyleP.h hlu.h hluP.h hluutil.h ImageWorkstation.h		\
ImageWorkstationP.h IrregularPlot.h IrregularPlotP.h			\
IrregularTransObj.h IrregularTransObjP.h IrregularType2TransObj.h	\
IrregularType2TransObjP.h LabelBar.h LabelBarP.h Legend.h LegendP.h	\
LogLinPlot.h LogLinPlotP.h LogLinTransObj.h LogLinTransObjP.h		\
MapDataHandler.h MapDataHandlerP.h MapPlot.h MapPlotP.h			\
MapRGDataHandler.h MapRGDataHandlerP.h MapTransObj.h MapTransObjP.h	\
MapV40DataHandler.h MapV40DataHandlerP.h MapV41DataHandler.h		\
MapV41DataHandlerP.h MeshScalarField.h MeshScalarFieldP.h MultiText.h	\
MultiTextP.h NcgmWorkstation.h NcgmWorkstationP.h NgSizeT.h NresDB.h	\
pageutil.h PDFWorkstation.h PDFWorkstationP.h PlotManager.h		\
PlotManagerI.h PlotManagerP.h Primitive.h PrimitiveP.h			\
PSWorkstation.h PSWorkstationP.h ResList.h ResListP.h ResourcesP.h	\
ScalarFieldFloatP.h ScalarField.h ScalarFieldP.h Segments.h		\
SphericalGeometryP.h SphericalTransObj.h SphericalTransObjP.h		\
StreamlinePlot.h StreamlinePlotP.h Style.h StyleP.h SubStylesP.h	\
TextItem.h TextItemP.h TickMark.h TickMarkP.h Title.h TitleP.h		\
Transform.h TransformI.h TransformP.h TransObj.h TransObjI.h		\
TransObjP.h TriMeshTransObj.h TriMeshTransObjP.h VarArg.h VecAnno.h	\
VecAnnoP.h VectorFieldFloatP.h VectorField.h VectorFieldP.h		\
VectorPlot.h VectorPlotP.h View.h ViewI.h ViewP.h Workspace.h		\
WorkspaceI.h WorkspaceP.h Workstation.h WorkstationI.h WorkstationP.h	\
XWorkstation.h XWorkstationP.h XyPlot.h XyPlotP.h
