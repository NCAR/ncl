#ifndef __RAS_ERROR__
#define __RAS_ERROR__

int	ras_nerr = 5;
int	ras_errno = 0;

char	*ras_errlist[] = {
	"System Error",
	"Only 8-bit pixels supported",
	"Only 8-bit intensities supported",
	"Only 8-bit run lengths supported",
	"Image encoding not supported"
};

#define RAS_E_SYSTEM				0
#define RAS_E_8BIT_PIXELS_ONLY			1
#define RAS_E_8BIT_INTENSITIES_ONLY		2
#define RAS_E_8BIT_RUNLENGTHS_ONLY		3
#define RAS_E_IMAGE_ENCODING_NOT_SUPPORTED	4

#endif __RAS_ERROR__
