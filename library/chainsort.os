!
!  ORSON/LIB/CHAINSORT. Sort a linear linked chain of execution objects.
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

!  SORTED. Destructively sort a linear linked chain of execution objects into a
!  nondecreasing order, according to a total ordering. Pointers to objects have
!  the type π and their sort keys have the type κ. KEY is a form that takes a π
!  as its argument and returns the corresponding κ. NEXT is a form that takes a
!  π as its argument, and returns the next π in the chain as a variable. "≺" is
!  a form that takes two κ's as its arguments. It tests if the first κ precedes
!  the second in the total ordering.

  sorted :−
   (with

!  K. Return the type of the form KEY.

     k :−
      (form (type ref exe π, type exe κ) type foj:
        form (π) κ)

!  N. Return the type of the form NEXT.

     n :−
      (form (type ref exe π) type foj:
        form (π) var π)

!  O. Return the type of the form "≺".

     o :−
      (form (type exe κ) type foj:
        form (κ, κ) bool)

!  The form itself. We represent a chain as a pair of pointers: one with a name
!  like FIRST X is the first node in the chain, and one with a name like LAST X
!  is the last.

    do (gen (type exe κ, type ref exe π)
         form (π first, o(κ) "≺", k(π, κ) key, n(π) next) π:
          (with
            var π    first :− (past first)  !  First node in FIRST.
            var π    last  :− nil           !  A dummy to satisfy SORTING.
            type exe v     :− var π         !  Abbreviated name for VAR Π.

!  APPEND. Destructively concatenate two chains of nodes.

            append :−
             (form (v leftFirst, v leftLast, π rightFirst, π rightLast) void:
              (if leftFirst = nil
               then leftFirst := rightFirst
                    leftLast  := rightLast
               else if rightFirst ≠ nil
                    then next(leftLast) := rightFirst
                         leftLast := rightLast))

!  INSERT. Destructively add a new node THIS to a chain of nodes.

            insert :−
             (form (var π first, var π last, π this) void:
               next(this) := nil
               (if first = nil
                then first := this
                     last  := this
                else next(last) := this
                     last := this))
 
!  NODES. Iterator. Visit every node in the chain starting with FIRST. It works
!  even if a node's NEXT slot is changed while the node is visited.

            nodes :−
             (form (π first) foj:
              (form (form (π) obj body) obj:
               (with var π this :− first
                do (while this ≠ nil
                    do (with π temp :− next(this)
                        do body(this)
                           this := temp)))))

!  SORTING. Sort a chain of nodes using a recursive D & C algorithm, similar to
!  that of Quicksort.

            sorting :−
             (proc (var π first, var π last) void:
              (if first ≠ nil ∧ next(first) ≠ nil
               then (with
                      var π leftFirst   :− nil
                      var π leftLast    :− nil
                      var π middleFirst :− nil
                      var π middleLast  :− nil
                      κ     pivot       :− key(first)
                      var π rightFirst  :− nil
                      var π rightLast   :− nil
                    do (for π this in nodes(first)
                        do (if key(this) ≺ pivot
                            then insert(leftFirst, leftLast, this)
                            else if key(this) = pivot
                                 then insert(middleFirst, middleLast, this)
                                 else insert(rightFirst, rightLast, this)))
                       sorting(leftFirst, leftLast)
                       sorting(rightFirst, rightLast)
                       append(middleFirst, middleLast, rightFirst, rightLast)
                       append(leftFirst, leftLast, middleFirst, middleLast)
                       first := leftFirst
                       last := leftLast)))

!  This is SORTED's body.

           do sorting(first, last)
              first)))
)
