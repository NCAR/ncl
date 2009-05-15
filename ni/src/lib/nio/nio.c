
#include "nio.h"
#include "NclType.h"
#include "TypeSupport.h"



void NioInitialize ( void )

{

	_NclInitTypeClasses();
	_NclInitClass(nclFileClass);
	_NclAddFileFormats();

	return;
}
