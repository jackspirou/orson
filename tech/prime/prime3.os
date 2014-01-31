!
!  PRIME3. Test during transformation if a number is prime.
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

!  IS PRIME. Test if NUMBER is prime during transformation.

  isPrime :−
   (form (int number) bool:
    (with
      real ε :− 0.001  !  Accuracy of SQRT.

!  ABS. Return the absolute value of NUMBER.

      abs :−
       (form (real number) real:
        (with real number :− (past number)
         do (if number < 0.0
             then − number
             else number)))

!  SQRT. Return the square root of A with accuracy ε.

      sqrt :−
       (form (real a) real:
        (with
          sqrting :−
           (form (real g, real h) real:
            (if abs(g − h) < ε
             then g
             else (with
                    g' :− (g + h) / 2.0
                    h' :− a / g'
                   do sqrting(g', h'))))
         do sqrting(1.0, a)))

!  IS PRIMING. Test if some DIVISOR evenly divides NUMBER.

      isPriming :−
       (form (int divisor) bool:
        (if divisor = 1
         then true
         else if number mod divisor = 0
              then false
              else isPriming(divisor − 1)))

!  Test if NUMBER is prime.

     do isInt(number) ∧ number > 1 ∧ isPriming(sqrt(number{real}){int})))

!  MAIN. Write TRUE if 5 is prime, and write FALSE otherwise.

  main :− writeln((if isPrime(5) then ''true'' else ''false''))
)
