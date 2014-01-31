#
#  EG/MAKEFILE. Compile, install, uninstall Eg.
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
#  
#  The following directories are where Eg will be installed.
#
#    PREFIX/BINDIR   Eg will be installed here.
#    PREFIX/MANDIR   The Eg "man" page will be installed here.
#
#  If these directories do not exist, then they will be created.

prefix = /usr/local
bindir = $(prefix)/bin
mandir = $(prefix)/man/man1

#  ALL. Compile Eg from source files in the current directory. Leave it in that
#  directory. You need not be root to do this.

all:
	orson eg.os
	mv a.out eg

#  CLEAN. Undo the effects of MAKE ALL. You need not be root to do this.

clean:
	rm -f Out.c a.out eg

#  INSTALL. Install Eg.
#
#    1. Make BIN DIRECTORY if it doesn't exist.
#    2. Make MAN DIRECTORY if it doesn't exist.
#    3. Compile Eg.
#    4. Move Eg to BIN DIRECTORY.
#    5. Make root the owner of Eg.
#    6. Let nonroots read and run Eg, but not write it.
#    7. Copy the Eg man page to MAN DIRECTORY.
#    9. Make root the owner of the Eg man page.
#    8. Let nonroots read the Eg man page, but not run or write it.
#
#  You must be root to do this.

install:
	mkdir -p $(bindir)
	mkdir -p $(mandir)
	orson eg.os
	mv a.out $(bindir)/eg
	chown root $(bindir)/eg
	chmod go-w+rx $(bindir)/eg
	cp eg.1 $(mandir)/eg.1
	chown root $(mandir)/eg.1
	chmod go-wx+r $(mandir)/eg.1

#  UNINSTALL. Undo the effects of MAKE INSTALL, by doing these things:
#
#    1. Undo the effects of MAKE ALL.
#    2. Delete Eg from BIN DIRECTORY.
#    3. Delete the Eg man page from MAN DIRECTORY.
#
#  Note that BIN DIRECTORY and MAN DIRECTORY will still exist. You must be root
#  to do this.

uninstall:
	rm -f Out.c a.out eg
	rm -f $(bindir)/eg
	rm -f $(mandir)/eg.1
