#!/usr/local/bin/perl
#
#      $Id: config.perl,v 1.5 1993-02-22 15:54:38 clyne Exp $
#
#########################################################################
#									#
#			   Copyright (C)  1992				#
#	     University Corporation for Atmospheric Research		#
#			   All Rights Reserved				#
#									#
#########################################################################
#
#	File:		config.perl
#
#	Author:		John Clyne
#			National Center for Atmospheric Research
#			PO 3000, Boulder, Colorado
#
#	Date:		Fri Sep 11 13:02:58 MDT 1992
#
#	Description:	Configure NCAR Graphics installation
#
#	Usage:		config.perl [-v] [-debug]
#
#	Environment:	NCARG	: Path to NCAR G source tree.
#
#	Files:
#
#
#	Options:	-v	Operate in verbose mode.
#			-debug	Operate in debug mode.

sub     usage {
	local($s) = @_;

	if (defined ($s)) {
		print STDERR "$progName: $s\n";
	}

	print STDERR "Usage: $progName [ -verbose ] [-debug ]\n";
	exit(1);
}

sub	cleanup {
	print "Terminating configuration procedure\n";
	exit (0);
}

#
#	return the "head" of a *fully* specified directory path. e.g
#	head("/usr/local/bin") returns "/usr/local"
#
sub	head {
	local($path) = @_;

	split(m;/+;, $path);
	shift @_;
	pop @_;
	
	if (! @_) {
		return("/");
	}
	else {
		return("/" . join("/", @_));
	}
}

#
#	Use the C preprocessor to determine what type of system we
#	are on.
#
sub	get_system {

	local($line, @result, $system, $defines);

	$line = `$instDir/uname -M -s`;
	@result = split(/\s+/,$line);

	#	get arch type and os
	$defines = "-D$result[0] -D$result[1]";

	$line = `cc -E GetSystem $defines | grep SYSTEM_INCLUDE`;
	@result = split(/\s+/,$line);
	$system = $result[2];

	return($system);
}

#
#	Return a list of values for a given macro from the config file for this
#	system
#
sub	get_config_values {
	local($file, $symbol) = @_;

	local($line, @value);

	$line = `grep ^$symbol $file`;
	@value = split(/\s+/,$line);
	shift value;
	shift value;

	return(@value);
}


#
#	Prompt the user for a directory path for a given directory.
#	$msg is a brief description of the directory
#	$default is the default path
#	$dir is the directory tail
#
#	The directory path must end in $dir.
#	If a directory path is selected that is different than the defualt
#	the global isDirty is set to true.
#
sub	get_path_from_user {
	local($msg, $default, $dir) = @_;

	local($parent);

LOOP: for ($done=0; $done==0; ) {
		print "\n$msg: $default\n";
		print "Enter Return (default), ";
		print "new directory path, or q(quit) > ";

		$_ = <STDIN>;
		chop;
		if (/^q/ || /^Q/) {
			do cleanup();
		}
		elsif ($_ eq "") {
			$_ = $default;
		}

		if (/^\./) {
			print STDERR "\n<$_> is not an absolute path name\n";
			next LOOP;
		}
		if (! -d $_) {
			if (-e $_) {	# is it a plain file?
				print STDERR "\n<$_> is not a directory\n";
				next LOOP;
			}
			$parent = &head($_);
			if (! -w $parent) {	# is parent writable?
				print STDERR "\n<$parent> is not writable\n";
				next LOOP;
				}
		}
		elsif (! -w $_) {
			print STDERR "\n<$_> is not writable\n";
			next LOOP;
		}
		split(/\//, $_);
		if ($_[$#_] ne $dir) {
			print STDERR "\n<$_> does not end in '$dir'\n";
			next LOOP;
		}
		$done = 1;
	}

	if ($default ne $_) {
		$isDirty = 1;	# user changed default path
	}
	return($_);
}

#
#
#
sub	get_search_paths {
	local($msg, @default) = @_;

	local($parent);

LOOP1: for ($done=0; $done==0; ) {
		$_ = join(' ', @default);
		print "\n$msg: $_\n";
		print "Enter Return (default), ";
		print "New space-separated directory path list, or q(quit) > ";

		$_ = <STDIN>;
		chop;
		if (/^q/ || /^Q/) {
			do cleanup();
		}
		elsif ($_ eq "") {
			$_ = join(' ', @default);
		}

		split(/\s+/);	# split into @_

		foreach $_ (@_) {

			if (/^\./) {
				print STDERR "\n<$_> is not an absolute path name\n";
				next LOOP1;
			}
			if (! -d $_) {
				print STDERR "\n<$_> is not a directory\n";
				next LOOP1;
			}
			if (! -r $_) {
				print STDERR "\n<$_> is not readable\n";
				next LOOP1;
			}
		}
		$done = 1;
	}

	if (join(' ', @default) ne $_) {
		$isDirty = 1;	# user changed default path
	}
	return(split(/\s+/, $_));
}

#
#	Get the path to the bin directory
#
sub	get_binpath {
	local($file) = @_;

	local($binpath, $done, $parent);

	if ($doVerbose) {
	print <<"EOF";

	It is a good idea to install the NCAR Graphics
	executables to a directory that will be on the "search
	path" of the NCAR Graphics user. This procedure will
	check to make sure that the directory specified is
	writable by the installation process.

EOF
	}


	$binpath = (&get_config_values($file, "BINROOT"))[0];

	if ($doDebug) {
		print "Default bin path <$binpath>\n";
	}

	return(&get_path_from_user("Executable search path", $binpath, "bin"));
}

#
#	Get the path to the library directory
#
sub	get_libpath {
	local($file) = @_;

	local($libpath, $done, $parent);

	if ($doVerbose) {
	print <<"EOF";

	NCAR Graphics libraries may be installed anywhere, but it is
	a good idea, if possible, to install them to a directory
	that is on the default search path for the loader.  This
	procedure will check to make sure that the directory specified
	is writable by the installation process.

EOF
	}

	$libpath = (&get_config_values($file, "LIBROOT"))[0];

	if ($doDebug) {
		print "Default lib path <$libpath>\n";
	}

	return(&get_path_from_user("Library installation path", $libpath, "lib"));
}


#
#	Get the path to the include directory
#
sub	get_incpath {
	local($file) = @_;

	local($incpath, $done, $parent);

	$incpath = (&get_config_values($file, "INCROOT"))[0];

	if ($doDebug) {
		print "Default include path <$incpath>\n";
	}

	return(&get_path_from_user("Include file installation path", $incpath, "include"));
}

#
#	Get the path to the man page directory
#
sub	get_manpath {
	local($file) = @_;

	local($manpath, $done, $parent);

	if ($doVerbose) {
	print <<"EOF";

	NCAR Graphics will install "man" documents and the installation
	directory should be one that the "man" program automatically searches.
	This procedure will check to make sure that the directory specified
	is writable by the installation process.

EOF
	}


	$manpath = (&get_config_values($file, "MANROOT"))[0];

	if ($doDebug) {
		print "Default man path <$manpath>\n";
	}

	return(&get_path_from_user("Man page installation path", $manpath, "man"));
}

#
#	Get the path to the tmp directory
#
sub	get_tmppath {
	local($file) = @_;

	local($tmppath, $done, $parent);

	if ($doVerbose) {
	print <<"EOF";

        A couple of the "ncarview" utilities make use of temporary file space.
        On most systems, the directory "/tmp" is the logical choice.  You may
        specify a different directory to use for this purpose.  The directory
        must be writable by users of the installed "ncarview".  This procedure
        will check only to make sure that the directory specified is writable
        by the installation process.

EOF
	}


	$tmppath = (&get_config_values($file, "TMPROOT"))[0];

	if ($doDebug) {
		print "Default tmp path <$tmppath>\n";
	}

	return(&get_path_from_user("System temp space path", $tmppath, "tmp"));
}

sub	get_local_lib_paths {
	local($file) = @_;

	local(@paths);

	@paths = &get_config_values($file, "LOCAL_LIB_SEARCH");
	if ($doDebug) {
		print "Default local lib paths <@paths>\n";
	}
	@paths = grep(s/-L//,@paths);	# remove the '-L's
	@paths = &get_search_paths("Local library search path(s)", @paths);
	return(join(" ", grep(s/(^.)/-L\1/, @paths)));	# restore -L
}

sub	get_local_inc_paths {
	local($file) = @_;

	local(@paths);

	@paths = &get_config_values($file, "LOCAL_INC_SEARCH");
	if ($doDebug) {
		print "Default local include paths <@paths>\n";
	}
	@paths = grep(s/-I//,@paths);	# remove the '-L's
	@paths = &get_search_paths("Local include search path(s)", @paths);
	return(join(" ", grep(s/(^.)/-I\1/, @paths)));	# restore -L
}


#
#	Succesfull completion procedure
#
sub	finish {

	print <<"EOF";

	You have completed the configuration process. Once you exit this
	program you may initiate the installation procedure by typing:

		make Everything >&make-output &

EOF

	if ($doVerbose) {

	print <<"EOF";

	If this is not your first installation attempt, and you simply
	made some changes in the configuration, restart the process using:

		make All >&make-output &

	This command does not erase object code that has already
	been compiled.

	Both of these commands run the installation procedure
	in the background, and you can track its progress
	by periodically examining the file "make-output" while
	the installation proceeds.

EOF

	}
}

##########################################################################
##
##	M A I N   P R O G R A M
##
##########################################################################

#
# Establish defaults
#
$doVerbose = 0;			# do operate in verbose mode?
$doDebug = 0;			# do print debugging statments?
$isDirty = 0;			# did the user change the configuration?
$pwd = `pwd`; chop $pwd;
$progName = $& if ($0 =~ /\w+$/);

if (! defined ($ncarg = $ENV{'NCARG'})) {
	print STDERR "NCARG environment variable not set\n";
	exit 1;
}

if ($ncarg ne $pwd) {
	print STDERR "NCARG environment must be set to <$pwd>.\n";
	exit 1;
}

$confDir = "$ncarg/config";
$instDir = "$ncarg/install/install";

while ($ARGV[0] =~ /-/) {
	$_ = shift @ARGV;

	if(/-v/) {
		$doVerbose = 1;
	}
	elsif(/-debug/) {
		$doDebug = 1;
	}
	else {
		do usage("Unknown option \"$_\"");
	}
}


#
#	Get the software release version from the version file.
#
$softwareRelease = `cat version`;
chop $softwareRelease;
if ($? != 0) {
	print STDERR "Couldn't get NCAR Graphics version number: $!\n";
	exit (1);
};

print "\n*** Configuration Procedure for Version $softwareRelease of NCAR Graphics\n";

chdir "$confDir"  || die "$progName: Can't cd to $confDir: $!\n";
$system = &get_system;
if ($doVerbose) {
	print "Your system is a <$system>\n";
}

if ($system eq "SystemV") {
	print <<"EOF";

        This configuration procedure is not familiar with your system, so it
        has selected the default configuration file called "$system". It is
        unlikely that you will get through the installation procedure without
        having to make some changes; you should refer to the Release Notice
        shipped with this package.

EOF
	print "Enter Return to continue or q(quit) > ";
	$_ = <STDIN>;
	chop;
	if (/^q/ || /^Q/) {
		do cleanup();
	}
}


if ($doVerbose) {
	print <<"EOF";

	This question and answer session will allow you to configure
	NCAR Graphics for installation. Each supported system (e.g. DEC,
	Sun) has a unique configuration file and this procedure provides
	you with a simple means of editing those parts that may require
	change. This procedure will also check that the directories
	specified are accessible and that the required system support
	is available.

EOF
}


$current_ver = &get_config_values($system, "VERSION");
$isDirty = 1 if ($current_ver ne $softwareRelease);

STARTOVER:

$binPath = &get_binpath($system);
$libPath = &get_libpath($system);

$lib_parent = &head($libPath);
$bin_parent = &head($binPath);

if ($lib_parent ne $bin_parent) {
	print <<"EOF";

	*** Warning: Executable bin directory, <$binPath>, and 
	Library directory, <$libPath>, have different parents, 
	<$bin_parent> and <$lib_parent>. Users 
	will need to set the NCARG_LIB and the NCARG_BIN environment variables
	instead of simply setting the NCARG_ROOT environment variable. Are
	you sure you want to do this ? (y)
EOF
	print "Enter Return(default), y(yes), n(no), or q(quit) > ";
	$_ = <STDIN>;
	chop;
	if (/^q/ || /^Q/) {
		do cleanup();
	}
	elsif (! (($_ eq "") || ($_ eq "y"))) {
		goto STARTOVER;
	}
}

$incPath = &get_incpath($system);
$manPath = &get_manpath($system);
$tmpPath = &get_tmppath($system);
$locLibPaths = &get_local_lib_paths($system);
$locIncPaths = &get_local_inc_paths($system);

print "\nYour installation configuration is:\n\n";
print "\tExecutable bin directory path:     $binPath\n";
print "\tLibrary installation path:         $libPath\n";
print "\tC include file directory path:     $incPath\n";
print "\tMan page directory path:           $manPath\n";
print "\tSystem temp space directory path:  $tmpPath\n";
print "\tLocal library search paths:        $locLibPaths\n";
print "\tLocal include search paths:        $locIncPaths\n";

print "\nSave current configuration ? (y)\n";
print "Enter Return(default), y(yes), n(no), or q(quit) > ";
$_ = <STDIN>;
chop;
if (/^q/ || /^Q/) {
	do cleanup();
}
elsif (! (($_ eq "") || ($_ eq "y"))) {
	goto STARTOVER;
}

if (! $isDirty) {
	print "\nThe configuration file was unchanged\n";
	do finish();
	exit(0);
}

if (! -e $system.dist) {
	print "	*** Warning: <$system> has been previously edited,";
	print "but there is no backup\n";
	print "\n	Making backup copy of <$system> to <$system.dist>\n";
	system ("cp $system $system.dist");
}

open(ED, "| ed - $system > /dev/null") || die "open(,| ed - $system > /dev/null): $!\n";

print ED <<"EOF";
/^VERSION/
c
VERSION		= $softwareRelease
.
/^BINROOT/
c
BINROOT		= $binPath
.
/^LIBROOT/
c
LIBROOT		= $libPath
.
/^INCROOT/
c
INCROOT		= $incPath
.
/^MANROOT/
c
MANROOT		= $manPath
.
/^TMPROOT/
c
TMPROOT		= $tmpPath
.
/^LOCAL_LIB_SEARCH/
c
LOCAL_LIB_SEARCH		= $locLibPaths
.
/^LOCAL_INC_SEARCH/
c
LOCAL_INC_SEARCH		= $locIncPaths
.
w
q
EOF

close ED;

do finish();
exit(0);
