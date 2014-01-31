!
!  PRIME4. Write prime numbers from 2 to 1000 using the Sieve of Erastothenes.
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

(load ''lib.file'')
(load ''set'')

(prog

!  SIEVE. Write the prime numbers between 2 and N.

  sieve :−
   (form (int n) void:
    (with var set(n) s
     do (for int k in 2, n
         do s += k)
        (for int k in 2, n
         do (if k ∊ s
             then (for int m in k + k, n, k
                   do s −= m)))
        (for int k in 2, n
         do (if k ∊ s
             then writeln(k)))))

!  MAIN. Write the prime numbers between 2 and 1000.

  main :− sieve(1000)
)
