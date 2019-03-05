# This is an automake file for NCL
# Ed Hartnett 9/5/18

bin_PROGRAMS = ncl

.NOTPARALLEL:

# Create ncarg and ncarg/ncl directories.
DIRS=ncarg ncarg/ncl
$(shell mkdir -p $(DIRS))

# Find gks.h.
AM_CPPFLAGS = -I$(top_srcdir)/ncarg2d/src/libncarg_gks

# Find c.h.
AM_CPPFLAGS += -I$(top_srcdir)/common/src/libncarg_c

# Find ncargC.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarg2d/src/libncarg

# Find hlu.h.
AM_CPPFLAGS += -I$(top_srcdir)/ni/src/lib/hlu

# Link to hlu library.
AM_LDFLAGS = ${top_builddir}/ni/src/lib/hlu/libhlu.la

# Link to ncarg library.
AM_LDFLAGS += ${top_builddir}/ncarg2d/src/libncarg/libncarg.la

# Link to ncarg c library.
AM_LDFLAGS += ${top_builddir}/common/src/libncarg_c/libncarg_c.la

# Link to ncarg gks library.
AM_LDFLAGS += ${top_builddir}/ncarg2d/src/libncarg_gks/libncarg_gks.la

# Link to ngmath library.
AM_LDFLAGS += ${top_builddir}/ngmath/src/lib/gridpack/libngmath.la

# Link to external libraries.
AM_LDFLAGS += ${top_builddir}/ngmath/src/lib/gridpack/libngmath.la
AM_LDFLAGS += ${top_builddir}/external/blas/libblas_ncl.la
AM_LDFLAGS += ${top_builddir}/external/fftpack5_dp/libfftpack5_dp.la
AM_LDFLAGS += ${top_builddir}/external/lapack/liblapack_ncl.la
AM_LDFLAGS += ${top_builddir}/external/sphere3.1_dp/libsphere3.1_dp.la
AM_LDFLAGS += ${top_builddir}/ni/src/lib/nfp/libnfp.la

CREATED_FILES = parser.h TypeSupport.h TypeSupport.c			\
NclMultiDValData.c MathFuncs.c MathFuncs.h NclTypedouble.h		\
NclTypedouble.c NclTypefloat.h NclTypefloat.c NclTypeint.h		\
NclTypeint.c NclTypelong.h NclTypelong.c NclTypeshort.h			\
NclTypeshort.c NclTypelogical.h NclTypelogical.c NclTypestring.h	\
NclTypestring.c NclTypeobj.h NclTypeobj.c NclTypechar.h NclTypechar.c	\
NclTypebyte.h NclTypebyte.c NclTypeint64.h NclTypeint64.c		\
NclTypeushort.h NclTypeushort.c NclTypeuint.h NclTypeuint.c		\
NclTypeulong.h NclTypeulong.c NclTypeuint64.h NclTypeuint64.c		\
NclTypeubyte.h NclTypeubyte.c scanapi.c parseapi.c

AddBuiltIns.c: $(CREATED_FILES)

ncl_SOURCES = AddBuiltIns.c AddFileFormats.c AddHLUObjs.c		\
AddIntrinsics.c AdvancedFileSupport.c AttSupport.c BuiltInFuncs.c	\
BuiltInSupport.c complete.c craybin.c ctoiee.c DataSupport.c date.c	\
editline.c Execute.c FileSupport.c Formats.c GetGrids.c guiFuncs.c	\
h5reader.c h5writer.c HLUFunctions.c HLUSupport.c hphack.c InitData.c	\
io.c javaAddFuncs.c javaAddProto.c ListSupport.c Machine.c Memory.c	\
NclAdvancedFile.c NclAdvancedGroup.c NclAdvancedOGR.c NclApi.c		\
NclAtt.c Ncl.c NclCCM.c NclCoordVar.c NclData.c NclDriver.c NclFile.c	\
NclFileVar.c NclGRIB2.c NclGRIB.c NclGroup.c NclHDF5.c NclHDF.c		\
NclHDFEOS5.c NclHDFEOS.c NclHLUObj.c NclHLUVar.c NclList.c		\
NclMultiDValHLUObjData.c NclMultiDVallistData.c				\
NclMultiDValnclfileData.c NclNetCDF4.c NclNetCdf.c NclNewHDF5.c		\
NclNewHDFEOS5.c NclOGR.c NclOneDValCoordData.c NclProf.c NclTime.c	\
NclType.c NclTypelist.c NclUserFileFormStub.c NclUserFuncStub.c		\
NclUserHLUStub.c NclVar.c openclAddFuncs.c openclAddProto.c		\
OpsFuncs.c PrintToken.c SrcTree.c subprocess.c Symbol.c sysunix.c	\
Translate.c userAddFuncs.c userAddProto.c VarSupport.c yywrap.c		\
$(CREATED_FILES)

include_HEADERS = AdvancedFileSupport.h ApiRecords.h AttSupport.h	\
ccmhdr.h cptec_254_gtb.h DataSupport.h date.h defs.h dwd_002_gtb.h	\
dwd_201_gtb.h dwd_202_gtb.h dwd_203_gtb.h dwd_204_gtb.h dwd_205_gtb.h	\
dwd_206_gtb.h dwd_207_gtb.h ecmwf_128_gtb.h ecmwf_129_gtb.h		\
ecmwf_130_gtb.h ecmwf_131_gtb.h ecmwf_132_gtb.h ecmwf_133_gtb.h		\
ecmwf_140_gtb.h ecmwf_150_gtb.h ecmwf_151_gtb.h ecmwf_160_gtb.h		\
ecmwf_162_gtb.h ecmwf_170_gtb.h ecmwf_171_gtb.h ecmwf_172_gtb.h		\
ecmwf_173_gtb.h ecmwf_174_gtb.h ecmwf_175_gtb.h ecmwf_180_gtb.h		\
ecmwf_190_gtb.h ecmwf_200_gtb.h ecmwf_201_gtb.h ecmwf_210_gtb.h		\
ecmwf_211_gtb.h ecmwf_213_gtb.h ecmwf_214_gtb.h ecmwf_215_gtb.h		\
ecmwf_217_gtb.h ecmwf_218_gtb.h ecmwf_219_gtb.h ecmwf_220_gtb.h		\
ecmwf_221_gtb.h ecmwf_228_gtb.h ecmwf_230_gtb.h ecmwf_234_gtb.h		\
editline.h editline_unix.h FileSupport.h fnmoc_gtb.h fsl0_gtb.h		\
fsl1_gtb.h fsl2_gtb.h h5data_struct.h HluClasses.h HLUFunctions.h	\
HLUSupport.h jma_3_gtb.h jra55_gtb.h Keywords.h ListSupport.h		\
Machine.h mpi_128_gtb.h mpi_180_gtb.h mpi_181_gtb.h mpi_199_gtb.h	\
ncep_128_gtb.h ncep_129_gtb.h ncep_130_gtb.h ncep_131_gtb.h		\
ncep_133_gtb.h ncep_140_gtb.h ncep_141_gtb.h ncep_opn_gtb.h		\
ncep_reanal_gtb.h NclAdvancedFile.h NclAdvancedFileStructure.h		\
NclAdvancedGroup.h NclApi.h NclAtt.h NclBuiltIns.h			\
NclBuiltInSupport.h NclCallBacksI.h NclCCM.h NclCoordVar.h		\
NclDataDefs.h NclData.h NclDriver.h NclExitCode.h NclFile.h		\
NclFileInterfaces.h NclFileVar.h NclGlobalParams.h NclGlobalVars.h	\
NclGRIB2.h NclGRIB.h NclGroup.h NclHDF.h NclHLUObj.h NclHLUVar.h	\
NclList.h NclMdInc.h NclMultiDValData.h NclMultiDValHLUObjData.h	\
NclMultiDVallistData.h NclMultiDValnclfileData.h			\
NclOneDValCoordData.h NclOptions.h NclProf.h NclType.h NclTypelist.h	\
NclVar.h OpsFuncs.h OpsList.h SrcTree.h Symbol.h tables.h		\
VarSupport.h

SHELL = /bin/sh
CP = cp
NUM_OP_TMPL = TypeDivOpTemplate.c.sed TypeFunctionOpTemplate.c.sed	\
TypeMatMulOpTemplate.c.sed TypeIsMonoFunc.c.sed				\
TypeMonoOpTemplate.c.sed TypeResetMissing.c.sed				\
TypeSelectOpTemplate.c.sed TypeSimpleOpTemplate.c.sed			\
TypeXorOpTemplate.c.sed TypeInitClassTemplate.c.sed NclType.c.sed	\
NclType.h.sed TypeAndOpTemplate.c.sed TypeOrOpTemplate.c.sed
STR_OP_TMPL = TypestringCompareOpTemplate.c.sed			\
TypestringSelectOpTemplate.c.sed TypeInitClassTemplate.c.sed	\
NclTypestring.c.sed
FILE_OP_TMPL = NclMultiDValData.c.sed MultiDValOp.c.sed

ioapi.o : io.c
	$(CP) io.c ioapi.c
#	$(CC) -c $(API_DEFS) $(CFLAGS) io.c -o ioapi.o

NclHLUObjapi.o : NclHLUObj.c
	$(CC) -c $(API_DEFS) $(CFLAGS) NclHLUObj.c -o NclHLUObjapi.o

scanapi.c: scanner.c
	$(CP) scanner.c scanapi.c

# scanapi.o: scanapi.c
# 	$(CC) -c $(API_DEFS) $(CFLAGS) $<

parseapi.c: parser.c
	$(CP) parser.c parseapi.c

# parseapi.o: parseapi.c
# 	$(CC) -c $(API_DEFS) $(CFLAGS) $<

scanner.c:	lex.yy.c
	$(CP) lex.yy.c scanner.c

parser.h:	y.tab.h
	$(CP) y.tab.h parser.h
	$(CP) $@ ncarg/ncl

parser.c:	y.tab.c
	$(CP) y.tab.c parser.c

lex.yy.c: ncl.l
	$(LEX) ncl.l

y.tab.c y.tab.h: ncl.y
	$(YACC) -d -t ncl.y

TypeSupport.h: TypeSupport.h.sed TypeSupportMatTemplate.h.sed TypeSupportOpTemplate.h.sed TypeSupportMonoOpTemplate.h.sed
	$(SHELL) type_support_h.sh
	$(CP) $@ ncarg/ncl

TypeSupport.c: TypeSupport.c.sed TypeSupportMatTemplate.c.sed TypeSupportOpTemplate.c.sed TypeSupportMonoOpTemplate.c.sed
	$(SHELL) type_support_c.sh

NclMultiDValData.c: NclMultiDValData.c.sed MultiDValOp.c.sed NclMultiDValData.h math_funcs.sh MatMulDValOp.c.sed
	$(SHELL) multid_data_c.sh

MathFuncs.c: MathFuncs.c.sed MathFuncs.h.sed MathTemplate.c.sed MathTemplate.h.sed math_funcs.sh
	$(SHELL) math_funcs.sh

MathFuncs.h: MathFuncs.c.sed MathFuncs.h.sed MathTemplate2.c.sed MathTemplate.c.sed MathTemplate.h.sed math_funcs.sh
	$(SHELL) math_funcs.sh
	$(CP) $@ ncarg/ncl

NclTypedouble.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh double
	$(CP) $@ ncarg/ncl

NclTypedouble.c:	double_data_c.sh op_funcs.sh NclTypedouble.c.specific $(NUM_OP_TMPL)
	$(SHELL) double_data_c.sh

NclTypefloat.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh float
	$(CP) $@ ncarg/ncl

NclTypefloat.c:	float_data_c.sh op_funcs.sh NclTypefloat.c.specific $(NUM_OP_TMPL)
	$(SHELL) float_data_c.sh

NclTypeint.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh int
	$(CP) $@ ncarg/ncl

NclTypeint.c:	int_data_c.sh op_funcs.sh NclTypeint.c.specific $(NUM_OP_TMPL)
	$(SHELL) int_data_c.sh

NclTypelong.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh long
	$(CP) $@ ncarg/ncl

NclTypelong.c:	long_data_c.sh op_funcs.sh NclTypelong.c.specific $(NUM_OP_TMPL)
	$(SHELL) long_data_c.sh

NclTypeshort.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh short
	$(CP) $@ ncarg/ncl

NclTypeshort.c:	short_data_c.sh op_funcs.sh NclTypeshort.c.specific $(NUM_OP_TMPL)
	$(SHELL) short_data_c.sh

NclTypelogical.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh logical
	$(CP) $@ ncarg/ncl

NclTypelogical.c:	logical_data_c.sh op_funcs.sh NclTypelogical.c.specific $(NUM_OP_TMPL)
	$(SHELL) logical_data_c.sh

NclTypestring.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh string
	$(CP) $@ ncarg/ncl

NclTypestring.c:	string_data_c.sh op_funcs.sh NclTypestring.c.specific $(STR_OP_TMPL)
	$(SHELL) string_data_c.sh

NclTypeobj.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh obj
	$(CP) $@ ncarg/ncl

NclTypeobj.c:	obj_data_c.sh op_funcs.sh NclTypeobj.c.specific $(NUM_OP_TMPL)
	$(SHELL) obj_data_c.sh

NclTypechar.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh char
	$(CP) $@ ncarg/ncl

NclTypechar.c:	char_data_c.sh op_funcs.sh NclTypechar.c.specific $(NUM_OP_TMPL)
	$(SHELL) char_data_c.sh

NclTypebyte.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh byte
	$(CP) $@ ncarg/ncl

NclTypebyte.c:	byte_data_c.sh op_funcs.sh NclTypebyte.c.specific $(NUM_OP_TMPL)
	$(SHELL) byte_data_c.sh

NclTypeint64.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh int64
	$(CP) $@ ncarg/ncl

NclTypeint64.c:	int64_data_c.sh op_funcs.sh NclTypeint64.c.specific $(NUM_OP_TMPL)
	$(SHELL) int64_data_c.sh

NclTypeushort.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh ushort
	$(CP) $@ ncarg/ncl

NclTypeushort.c:	ushort_data_c.sh op_funcs.sh NclTypeushort.c.specific $(NUM_OP_TMPL)
	$(SHELL) ushort_data_c.sh

NclTypeuint.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh uint
	$(CP) $@ ncarg/ncl

NclTypeuint.c:	uint_data_c.sh op_funcs.sh NclTypeuint.c.specific $(NUM_OP_TMPL)
	$(SHELL) uint_data_c.sh

NclTypeulong.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh ulong
	$(CP) $@ ncarg/ncl

NclTypeulong.c:	ulong_data_c.sh op_funcs.sh NclTypeulong.c.specific $(NUM_OP_TMPL)
	$(SHELL) ulong_data_c.sh

NclTypeuint64.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh uint64
	$(CP) $@ ncarg/ncl

NclTypeuint64.c:	uint64_data_c.sh op_funcs.sh NclTypeuint64.c.specific $(NUM_OP_TMPL)
	$(SHELL) uint64_data_c.sh

NclTypeubyte.h:	data_h.sh NclType.h.sed
	$(SHELL) data_h.sh ubyte
	$(CP) $@ ncarg/ncl

NclTypeubyte.c:	ubyte_data_c.sh op_funcs.sh NclTypeubyte.c.specific $(NUM_OP_TMPL)
	$(SHELL) ubyte_data_c.sh

EXTRA_DIST = MathFuncs.c.sed MathFuncs.h.sed MathTemplate2.c.sed	\
MathTemplate.c.sed MathTemplate.h.sed MatMulDValOp.c.sed		\
MultiDValOp.c.sed NclMultiDValData.c.sed NclMultiDValData.h.sed		\
NclType.c.sed NclType.h.sed NclTypestring.c.sed nio_path.c.sed		\
TypeAndOpTemplate.c.sed TypeDivOpTemplate.c.sed				\
TypeFunctionOpTemplate.c.sed TypeInitClassTemplate.c.sed		\
TypeIsMonoFunc.c.sed TypeMatMulOpTemplate.c.sed				\
TypeMonoOpTemplate.c.sed TypeOrOpTemplate.c.sed				\
TypeResetMissing.c.sed TypeSelectOpTemplate.c.sed			\
TypeSimpleOpTemplate.c.sed TypestringCompareOpTemplate.c.sed		\
TypestringSelectOpTemplate.c.sed TypeSupport.c.sed TypeSupport.h.sed	\
TypeSupportMatTemplate.c.sed TypeSupportMatTemplate.h.sed		\
TypeSupportMonoOpTemplate.c.sed TypeSupportMonoOpTemplate.h.sed		\
TypeSupportOpTemplate.c.sed TypeSupportOpTemplate.h.sed			\
TypeXorOpTemplate.c.sed byte_data_c.sh char_data_c.sh data_h.sh		\
double_data_c.sh float_data_c.sh int64_data_c.sh int_data_c.sh		\
logical_data_c.sh long_data_c.sh math_funcs.sh multid_data_c.sh		\
nclfile_data_c.sh obj_data_c.sh op_funcs.sh short_data_c.sh		\
string_data_c.sh syms_lex.sh syms_tab_c.sh syms_tab_h.sh		\
type_support_c.sh type_support_h.sh ubyte_data_c.sh uint64_data_c.sh	\
uint_data_c.sh ulong_data_c.sh ushort_data_c.sh				\
NclTypebyte.c.specific NclTypechar.c.specific				\
NclTypedouble.c.specific NclTypefloat.c.specific			\
NclTypeint64.c.specific NclTypeint.c.specific				\
NclTypelogical.c.specific NclTypelong.c.specific			\
NclTypeobj.c.specific NclTypeshort.c.specific				\
NclTypestring.c.specific NclTypeubyte.c.specific			\
NclTypeuint64.c.specific NclTypeuint.c.specific				\
NclTypeulong.c.specific NclTypeushort.c.specific MathFuncs.h		\
NclTypebyte.h

# Make clean must remove all created files.
CLEANFILES = $(CREATED_FILES)
