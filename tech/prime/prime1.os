!
!  PRIME1. Write prime numbers from 2 to 1000 using procedures.
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

(prog
  real ε :− 0.001  !  Accuracy of SQRT.

!  ABS. Return the absolute value of NUMBER.

  abs :−
   (proc (real number) real:
    (if number < 0.0
     then − number
     else number))

!  SQRT. Return the square root of nonnegative A to accuracy ε.

 sqrt :−
  (proc (real a) real:
   (with
     var real g :− 1.0
     var real h :− a
    do (while abs(g − h) ≥ ε
        do g := (g + h) / 2.0
           h := a / g)
       g))

!  IS PRIME. Test if NUMBER is prime.

 isPrime :−
  (proc (int number) bool:
   (with
     var int factor :− 2
     var bool going :− true
     int root :− sqrt(number{real}){int}
    do (while going ∧ factor ≤ root
        do going := number mod factor ≠ 0
           factor += 1)
       going))

!  MAIN. Write all prime numbers between 2 and 1000.

 main :−
  (for int number in 2, 1000
   do (if isPrime(number)
       then writeln(number)))
)
