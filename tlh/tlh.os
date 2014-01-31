!
!  TLH. Make a two-level perfect hash method for string keys.
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

!  TLH finds a two-level perfect hash method (if it exists) for a set of one or
!  more strings. The hash method accepts a string argument and returns an index
!  into an array where information about the string can be found. The method is
!  PERFECT if no two two strings from the set get the same index (a COLLISION).
!  The method is MINIMAL if the array has no unused elements. TLH's methods are
!  perfect but not necessarily minimal.
!
!  Each of TLH's hash methods work like this. It first converts its string to a
!  nonnegative integer, then MODs that integer by a positive integer PRIMARY (a
!  PRIMARY MODULUS) to get an index. Then it uses that index to get a SECONDARY
!  MODULUS from an array SECONDARIES, and integer OFFSET from an array OFFSETS.
!  Finally it MODs the integer by the secondary modulus, and adds OFFSET to the
!  result. This can be written as the Orson form shown below, where "␣{}" casts
!  a string to an arbitrary nonnegative integer (see TLH/HASH).
!
!    hash :−
!     (form (string chars) int:
!      (with
!        int bits  :− chars{int}
!        int index :− bits mod primary
!       do bits mod secondaries[index] + offsets[index]))
!
!  TLH uses a brute-force algorithm to find PRIMARIES, SECONDARIES, and OFFSETS
!  that minimize the range of indexes returned by HASH on a set of strings, but
!  without creating collisions. This is not always possible.
!
!  You can compile TLH like this.
!
!    orson tlh.os
!    mv a.out tlh
!
!  And you can run it like this.
!
!    tlh -p n₁ -s n₂ φ₁ φ₂ ... φⱼ
!
!  TLH searches for primary moduli p in the range 1 ≤ p ≤ n₁, and for secondary
!  moduli s in the range 1 ≤ s ≤ n₂. Both n₁ and n₂ default to 100. The φ's are
!  pathnames of files containing sets of strings to be used as keys, one string
!  per line. TLH writes an Orson form HASH to OUTPUT that uses a CASE clause to
!  show the primary modulus, secondary moduli, and offsets. It also writes some
!  Orson assignments to OUTPUT, showing the index associated with every string.
!  Searching larger ranges of primary and secondary moduli does not necessarily
!  result in better hash forms.
!
!  TLH is based on a throw-away Pascal program I wrote in 1978, as an undergrad
!  at the University of Minnesota. Like the earlier program, TLH has some weird
!  bugs. I don't understand these bugs well enough to describe them here.

(load ''lib.break'')    !  Terminate an iterator.
(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.command'')  !  Process Unix command line arguments.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.default'')  !  Default values of given types.
(load ''lib.dynamic'')  !  Dynamic memory allocation with explicit release.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.text'')     !  Read characters and lines from a text file.

(prog
  int      maxKeyLength :− 1024   !  Max number of CHAR0s in a key.
  var int  maxPrimary   :− 100    !  Max primary hash modulus (-p option).
  var int  maxSecondary :− 100    !  Max secondary hash modulus (-s option).
  int      minPrimary   :− 1      !  Min primary hash modulus.
  int      minSecondary :− 1      !  Min secondary hash modulus.
)

(load ''array'')  !  Arrays from the heap.
(load ''hash'')   !  Convert strings to arbitrary integers.
(load ''key'')    !  Linear chains of strings used as hash keys.
(load ''tree'')   !  Unbalanced binary search trees of INTs.
(load ''write'')  !  Write results.
(load ''main'')   !  Main program.

(prog

!  SET LIMIT. Convert DIGITS to a nonnegative integer and set LIMIT to it.

  setLimit :−
   (proc (char option, var int limit, string digits) void:
    (for bool ok, int number in convert(int, digits)
     do (if ok ∧ number > 0
         then limit := number
         else fail(''-%c: Nonnegative integer expected.'': option))))

!  BEGIN. Visit each command line option and do what it says.

  begin :−
   (for char option, string value in command(ϵ, '' ps'')
    do (case option
        of 'p': setLimit('p', maxPrimary,   value)
           's': setLimit('s', maxSecondary, value)
           ' ': main(value)))
)
