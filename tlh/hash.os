!
!  TLH/HASH. Convert strings to arbitrary integers.
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

(prog

!  " {}". Convert CHARS to an arbitrary nonnegative INT.

  " {}" :−
   (form (string chars, type int) int:
    (with
      var int    bits   :− 0
      var string chars  :− (past chars)
      int        "01⋯1" :− high(int)
     do (while chars↑
         do bits := (bits ← 1) ~ chars↑
            chars += 1)
        "01⋯1" & bits))

!  HASH. Convert CHARS to an arbitrary nonnegative INT and return the remainder
!  after dividing it by MODULUS.

  hash :−
   (proc (string chars, int modulus) int:
     chars{int} mod modulus)
)
