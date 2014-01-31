!
!  ORSON/LIB/CHAIN. Visit the nodes in a linear linked chain.
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

(load ''lib.break'') 

(prog

!  CHAIN. Iterator. Call BODY on each NODE in a linear linked chain of NODEs. A
!  NODE has a slot whose type is a REF type that points to the next node in the
!  chain. The slot's name is represented as the SYM type NEXT. The iterator can
!  be stopped by calling its BREAKER.

  chain :−
   (gen (type sym next)
     gen (type (with base :− slot(ref base, next) do base) node)
      form (type next, ref node first) foj:
       (alt
        (form (form (ref node) obj body) obj:
         (with var ref node temp :− first
          do (while temp ≠ nil
              do body(temp)
                 temp := temp↑.next))),
        (form (form (breaker() break, ref node) obj body) obj:
         (with
           var bool     going :− true
           var ref node temp  :− first
          do (while going ∧ temp ≠ nil
              do body(makeBreaker(going), temp)
                 temp := temp↑.next)))))
)
