#
#  APPS/MAKEFILE. Compile, install, uninstall a small Orson application.
#
#  Copyright © 2014 James B. Moen.
#
#  This  program is free  software: you  can redistribute  it and/or  modify it
#  under the terms  of the GNU General Public License as  published by the Free
#  Software Foundation,  either version 3 of  the License, or  (at your option)
#  any later version.
#
#  This program is distributed in the  hope that it will be useful, but WITHOUT
#  ANY  WARRANTY;  without even  the  implied  warranty  of MERCHANTABILITY  or
#  FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU General  Public License for
#  more details.
#
#  You should have received a copy of the GNU General Public License along with
#  this program.  If not, see <http://www.gnu.org/licenses/>.
#

#  Each action has a comment that describes exactly what it does to your system
#  and whether you must be root to do it. Please read these comments carefully!
#  This Makefile assumes that an Orson compiler is installed. An Orson compiler
#  is available for free from the author.

#  PROGRAM is the name of of the program to be installed, without a suffix. The
#  default is 'unknown' which will make the Makefile terminate with an error if
#  you forget to define it.

program = unknown

#  The following directories are where PROGRAM will be installed.
#
#    PREFIX/BINDIR   PROGRAM will be installed here.
#    PREFIX/MANDIR   PROGRAM's "man" page will be installed here.
#
#  If these directories do not exist, then they will be created.

prefix = /usr/local
bindir = $(prefix)/bin
mandir = $(prefix)/man/man1

#  COMPILE. Compile PROGRAM, from a source file in the current directory. Leave
#  it in that directory. You need not be root to do this.

compile:
	orson $(program).os
	mv a.out $(program)

#  CLEAN. Undo the effects of COMPILE. You need not be root to do this.

clean:
	rm -f Out.c a.out $(program)

#  INSTALL. Install PROGRAM.
#
#    1. Make BIN DIRECTORY if it doesn't exist.
#    2. Make MAN DIRECTORY if it doesn't exist.
#    3. Compile PROGRAM.
#    4. Move PROGRAM to BIN DIRECTORY.
#    6. Make root the owner of PROGRAM.
#    5. Let nonroots read and run PROGRAM, but not write it.
#    7. Copy PROGRAM's man page to MAN DIRECTORY.
#    8. Make root the owner of PROGRAM's man page.
#    9. Let nonroots read PROGRAM's man page, but not run or write it.
#
#  You must be root to do this.

install:
	mkdir -p $(bindir)
	mkdir -p $(mandir)
	orson $(program).os
	mv a.out $(bindir)/$(program)
	chown root $(bindir)/$(program)
	chmod go-w+rx $(bindir)/$(program)
	if test -e $(program).1; then \
	cp $(program).1 $(mandir)/$(program).1; \
	chown root $(mandir)/$(program).1; \
	chmod go-wx+r $(mandir)/$(program).1; \
	fi

#  UNINSTALL. Undo the effects of MAKE INSTALL, by doing these things:
#
#    1. Undo the effects of COMPILE.
#    2. Delete PROGRAM from BIN DIRECTORY.
#    3. Delete PROGRAM's man page from MAN DIRECTORY.
#
#  Note that BIN DIRECTORY and MAN DIRECTORY will still exist. You must be root
#  to do this.

uninstall:
	rm -f Out.c a.out $(program)
	rm -f $(bindir)/$(program)
	rm -f $(mandir)/$(program).1
