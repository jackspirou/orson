!
!  SET. Sets of small integers from 0 to a constant upper limit.
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

!  SET. Return the type of set with N elements, or a joker describing all such
!  sets.

  set :−
   (alt
    (form () type tup:
     (tuple
       [] bool Bits,
       int Count)),
    (form (inj n) type tup:
     (tuple
       [n + 1] bool Bits,
       int Count)))

!  INIT. Initialize a set variable S to have zero elements.

  init :−
   (form (var set() s) void:
    (for int e in 0, length(s.Bits) − 1
     do s.Bits[e] := false)
    s.Count := 0)

!  "+=". Add an integer E to a set variable S.

  "+=" :−
   (form (var set() s, int e) void:
    (with int e :− (past e)
     do (if ¬ (e ∊ s)
         then s.Bits[e] := true
              s.Count += 1)))

!  "−=". Remove an integer E from a set variable S.

  "−=" :−
   (form (var set() s, int e) void:
    (with int e :− (past e)
     do (if e ∊ s
         then s.Bits[e] := false
              s.Count −= 1)))

!  "∊". Test if an integer E is a member of a set variable S.

  "∊" :−
   (form (int e, var set() s) bool:
     s.Bits[e])

!  CARD. Cardinality. Return the number of elements in a set variable S.

  card :−
   (form (var set() s) int:
     s.Count)

!  IS EMPTY. Test if a set variable S has zero elements.

  isEmpty :−
   (form (var set() s) bool:
     card(s) = 0)
)
