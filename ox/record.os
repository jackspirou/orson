!
!  OX/RECORD. Record a name and its line numbers.
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

!  MAKE PLACE. Return a PLACE with NUMBER.

  makePlace :−
   (proc (int number) ref place:
    (with ref var place this :− fromHeap(var place)
     do this↑.number := number
        this↑.next := nil
        this{ref place}))

!  MAKE TREE. Return a TREE with NAME. It has one PLACE that holds NUMBER.

  makeTree :−
   (proc (string name, int number) ref tree:
    (with
      ref var tree this :− fromHeap(var tree)
      ref place    that :− makePlace(number)
     do this↑.name  := copy(name)
        this↑.first := that
        this↑.last  := that
        this↑.left  := nil
        this↑.right := nil
        this{ref tree}))

!  RECORD NAME. Add NAME and its line NUMBER to the search tree ROOT.

  ref tree root :− makeTree(ϵ, 0)

  recordName :−
   (alt
    (form (var buffer(maxNameLength) name, int number) void:
      recordName(name{string}, number)),
    (form (string name, int number) void:
      RecordName(name, number)))

  RecordName :−
   (proc (string name, int number) void:
    (with
      var ref tree subtree :− root

!  RECORD NUMBER. Record that the NAME in SUBTREE appears at line NUMBER.

      recordNumber :−
       (form () void:
        (if subtree↑.last↑.number ≠ number
         then (with ref place next :− makePlace(number)
               do subtree↑.last↑.next := next
                  subtree↑.last := next)))

!  This is RECORD NAME's body.

     do (while
         (with int test :− comp(name, subtree↑.name)
          do (if test < 0
              then (if subtree↑.left = nil
                    then subtree↑.left := makeTree(name, number)
                         false
                    else subtree := subtree↑.left
                         true)
              else if test > 0
                   then (if subtree↑.right = nil
                         then subtree↑.right := makeTree(name, number)
                              false
                         else subtree := subtree↑.right
                              true)
                   else recordNumber()
                        false)))))
)
