!
!  APPS/CHAINSAW. Sort a linked chain of tuples.
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

(load ''lib.dynamic'')    !  Dynamic memory allocation with explicit release.
(load ''lib.chainsort'')  !  Sort a linear linked chain of execution objects.
(load ''lib.file'')       !  Input and output on file streams.

(prog

!  NODE. A node in a linear linked chain to be sorted. KEY is the sort key, and
!  NEXT points to the next node in the chain.

  node :−
   (tuple
     int key,
     var ref node next)

!  MAKE NODE. Return a new NODE containing KEY and NEXT.

  makeNode :−
   (proc (int key, ref node next) ref node:
    (with ref var node this :− fromHeap(var node)
     do this↑.key := key
        this↑.next := next
        this{ref node}))

!  MAKE NODES. Return a chain of NODEs with the given KEYS.

   makeNodes :−
    (form (list keys) ref node:
     (if isEmpty(keys)
      then nil
      else makeNode(car(keys), makeNodes(cdr(keys)))))

!  WRITELN. Write the KEYs in a chain of NODEs.

   writeln :−
    (form (ref node first) void:
     (with var ref node this :− first
      do (while this ≠ nil
          do write(''%i '': this↑.key)
             this := this↑.next)
         writeln()))

!  MAIN.  Test CHAINSORT by making an unsorted list of NODE's. Write them, sort
!  them, and write them again.

   main :−
    (with var ref node unsorted :− makeNodes(: 3, 7, 1, 9, 8, 2, 5, 4, 6, 0)
     do writeln(unsorted)
        writeln(
         sorted(unsorted, "<",
          (form (ref node this) int: this↑.key),
          (form (ref node this) var ref node: this↑.next))))
)
