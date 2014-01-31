!
!  OX. Write a cross-referenced list of names in a source file.
!
!  Copyright © 2012 James B. Moen.
!
!  This  program is free  software: you  can redistribute  it and/or  modify it
!  under the terms  of the GNU General Public License as  published by the Free
!  Software Foundation,  either version 3 of  the License, or  (at your option)
!  any later version.
!
!  This program is distributed in the  hope that it will be useful, but WITHOUT
!  ANY  WARRANTY;  without even  the  implied  warranty  of MERCHANTABILITY  or
!  FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU General  Public License for
!  more details.
!
!  You should have received a copy of the GNU General Public License along with
!  this program.  If not, see <http://www.gnu.org/licenses/>.
!

(load ''lib.ascii'')    !  Operations on ASCII chars.
(load ''lib.break'')    !  Terminate an iterator.
(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.command'')  !  Process Unix command line arguments.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.dynamic'')  !  Dynamic memory allocation with explicit release.
(load ''lib.encode'')   !  Encode a character as a string.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.path'')     !  Operations on Unix pathnames.
(load ''lib.plain'')    !  Operations on Orson plain names.
(load ''lib.select'')   !  Simulate a CASE clause whose labels are strings.
(load ''lib.string'')   !  Operations on strings.
(load ''lib.width'')    !  Determine columns to write simple objects.

(prog
  var bool asciiing        :− false   !  (-a) Show chars in ASCII.
  var bool accenting       :− false   !  (-z) Show accents with overprints.
  var int  columnsPerName  :− 32      !  (-c) Show this many columns per name.
  var int  digitsPerNumber :− 5       !  (-n) Show this many digits per number.
  inj      maxNameLength   :− 1024    !  Max CHAR0s per name.
  string   me              :− ''ox''  !  Name of this program.
  var int  numbersPerLine  :− 8       !  (-n) Show this many numbers per line.

!  PLACE. Assert that a name appears at a line NUMBER. PLACEs are linked into a
!  linear chain through their NEXT slots, in increasing order of their NUMBERs.

  place :−
   (tuple
     int number,
     var ref place next)

!  TREE. Assert that a NAME appears as described by FIRST through LAST, a chain
!  of PLACEs. TREEs are linked into an unbalanced binary search tree using LEFT
!  and RIGHT slots. LEFT points to a subtree whose names are less than NAME and
!  RIGHT points to a subtree whose names are greater than NAME.

  tree :−
   (tuple
     string        name,
     var ref place first,
     var ref place last,
     var ref tree  left,
     var ref tree  right)
)

(load ''char'')    !  Read chars from a source file.
(load ''record'')  !  Record a name and its line numbers.
(load ''token'')   !  Group chars into tokens.
(load ''parse'')   !  Parse a sequence of tokens.
(load ''report'')  !  Write names and their line numbers.
(load ''main'')    !  Main program.
