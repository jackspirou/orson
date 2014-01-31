!
!  TLH/TREE. Unbalanced binary search trees of INTs.
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

!  TREE. The root of an unbalanced binary search tree of hash INDEXes. Those in
!  the LEFT subtree are less than INDEX. Those in the RIGHT subtree are greater
!  than INDEX.

  tree :−
   (tuple
     int          index,
     var ref tree left,
     var ref tree right)

!  MAKE TREE. Return a pointer to a search tree with INDEX whose LEFT and RIGHT
!  subtrees are empty. If INDEX is missing, then it defaults to −1.

  makeTree :−
   (alt
    (form () ref tree:
      makeTree(−1)),
    (form (int index) ref tree:
     (with ref var tree this :− fromHeap(var tree)
      do this↑.index := index
         this↑.left  := nil
         this↑.right := nil
         this{ref tree})))

!  UNMAKE TREE. Send the binary search tree ROOT back to the heap.

  unmakeTree :−
   (proc (ref tree root) void:
    (with var ref tree subtree :− root
     do (while subtree ≠ nil
         do unmakeTree(subtree↑.left)
            subtree := (subtree↑.right also toHeap(subtree)))))

!  "∊". Test if INDEX is in the search tree ROOT. If it's not in the tree, then
!  add it.

  "∊" :−
   (form (int index, ref tree root) bool:
    (with
      var bool     found   :− false
      var ref tree subtree :− root
     do (while
         (if index < subtree↑.index
          then (if subtree↑.left = nil
                then subtree↑.left := makeTree(index)
                     false
                else subtree := subtree↑.left
                     true)
          else if index > subtree↑.index
               then (if subtree↑.right = nil
                     then subtree↑.right := makeTree(index)
                          false
                     else subtree := subtree↑.right
                          true)
               else found := true
                    false))
        found))
)
