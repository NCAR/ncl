#ifndef FALSE
#define FALSE	0
#endif	FALSE

#ifndef TRUE
#define TRUE	1
#endif	TRUE

#define NRIF_MAGIC		"NRIF"

#define NRIF_HEADER_SIZE	36

#define NRIF_BILEVEL		0
#define NRIF_BILEVEL_RLE	1
#define NRIF_INDEXED		2
#define NRIF_INDEXED_RJE	3
#define NRIF_DIRECT		4
#define NRIF_DIRECT_RLE		5
#define NRIF_DIRECT_SEG		6
#define NRIF_DIRECT_SEG_RLE	7

static char	*nrif_types[] = {
	"NRIF_BILEVEL",
	"NRIF_BILEVEL_RLE",
	"NRIF_INDEXED",
	"NRIF_INDEXED_RLE",
	"NRIF_DIRECT",
	"NRIF_DIRECT_RLE",
	"NRIF_DIRECT_SEG",
	"NRIF_DIRECT_SEG_RLE"
};

typedef struct NrifInfoStruct {

	/* Derived from NRIF header */

	unsigned int	encapsulated;
	unsigned int	vplot;

	/* NRIF encapsulated information */

	unsigned int	objtype;
	unsigned int	control;
	unsigned int	recsize;

	/* NRIF header information */

	char		magic[4];
	unsigned int	flags;
	unsigned int	nx;
	unsigned int	ny;
	unsigned int	cmtlen;
	char		*comment;
	unsigned int	device;
	unsigned int	devlen;
	char		*device_info;
	unsigned int	encoding;
	unsigned int	enclen;

	/* Encoding information */

	unsigned int	ncolor;
	unsigned int	ibits;
	unsigned int	cbits;
	unsigned int	rbits;
	unsigned int	fcolred;
	unsigned int	fcolgrn;
	unsigned int	fcolblu;
	unsigned int	bcolred;
	unsigned int	bcolgrn;
	unsigned int	bcolblu;
	unsigned int	pbits;
} NrifInfo;
