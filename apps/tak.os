!
!  APPS/TAK. Takeuchi function expressed as a form.
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

!  Takeuchi functions are used to test implementations of functional languages.
!  They're described in the following references.
!
!  Henry G. Baker. "A Tachy 'TAK'."  Lisp Pointers, Volume 5, Number 1.  (July-
!  September 1992.)
!
!  Richard P. Gabriel.  Performance and Evaluation of Lisp Systems.  MIT Press,
!  Cambridge, Massachusetts. 1985.
!
!  We implemented a Takeuchi function as an Orson form and used it to test form
!  application and garbage collection in the Orson compiler.

(prog

!  TAK. McCarthy's misremembered version of the Takeuchi function, expressed as
!  a form.

  tak :−
   (form (inj x, inj y, inj z) inj:
    (if y ≥ x
     then z
     else tak(
           tak(x − 1, y, z),
           tak(y − 1, z, x),
           tak(z − 1, x, y))))

!  Baker (or is it Gabriel?) says tak(18, 12, 6) does 63_609 recursive calls to
!  TAK and 47_706 decrements, then returns 7. No argument ever becomes negative
!  and none ever exceeds 18. If we say "orson -d0 tak.os", then we can see what
!  the garbage collector is doing during all this fuss.

  seven :− tak(18, 12, 6)
)
