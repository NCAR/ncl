ROOT_DIR	= /cyclone/X/ncarg/binaries
INS_DIR	= $(ROOT_DIR)/INSTALL
SRC_TAR	= $(ROOT_DIR)/SRC.TAR

TARGETS		=	HPUX_HP9000_7xx AIX_RS6000 SunOS_sun4 \
			Solaris_sun4 ULTRIX_RISC IRIX_IP IRIX64_IP OSF1_alpha \
			UNICOS_YMP UNICOS_J90 UNICOS_C90 LINUX_i486

SHELL		= /bin/sh

all::	dirs

all::
	@/bin/rm -f var/TOC
all::
	@-for name in $(TARGETS); do \
		cp $(ROOT_DIR)/$$name/sizes var/sizes.$$name; \
		cp $(ROOT_DIR)/$$name/version var/version.$$name; \
	done

dirs::
	@if (test ! -d $(INS_DIR)) then mkdir $(INS_DIR); else continue; fi
	@if (test ! -d $(INS_DIR) || test ! -w $(INS_DIR)) then \
	echo "Installation Error - $(INS_DIR) is not writeable"; exit 1; \
	fi
	@/bin/rm -fr $(INS_DIR)/*

install::	all
	tar -cf - . | (cd $(ROOT_DIR)/INSTALL; tar -xf -)
	@echo "Generating distribution source from $(NCARG)
	(cd $(NCARG); tar -cbf 20 $(SRC_TAR) .)

clean::
	/bin/rm -f var/*

