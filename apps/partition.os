!
!  APPS/PARTITION. Write the partitions of a set.
!
!  Copyright © 2013 James B. Moen.
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

(load ''lib.bitset'')  !  Finite sets of small integers.
(load ''lib.file'')    !  Input and output on file streams.
(load ''lib.lslc'')    !  Linear singly linked chains.

(prog
  inj      limit     :− 32                !  Upper bound on an INT SET.
  type mut intSet    :− set(limit)        !  A set of integers 0 to LIMIT − 1.
  type mut partition :− lslc(var intSet)  !  A chain of INT SET variables.

!  PARTITIONS. Iterator. Call BODY on each partition of a set of INTs from 0 to
!  L − 1, where L is assumed less than LIMIT.  The number of partitions goes up
!  very rapidly with L.

  partitions :−
   (form (int l) foj:
    (form (form (partition) obj body) obj:
     (with
       var partition p :− makeLslc(var intSet)

!  PARTITIONING. Do all the work for PARTITIONS.

       partitioning :−
        (proc (int k) void:
         (if k ≥ 0
          then (with var intSet s :− makeSet(limit:) + k
                do insertRight(p, s)
                   partitioning(k − 1)
                   deleteRight(p)
                   (for var intSet s in elements(p)
                    do s += k
                       partitioning(k − 1)
                       s −= k))
          else body(p)))

!  This is PARTITIONS's body.

      do partitioning(l − 1))))

!  MAIN. Test PARTITIONS by writing all partitions of { 0, 1, 2, 3 }.
!
!    { 0 } { 1 } { 2 } { 3 } 
!    { 0, 1 } { 2 } { 3 } 
!    { 1 } { 0, 2 } { 3 } 
!    { 1 } { 2 } { 0, 3 } 
!    { 0 } { 1, 2 } { 3 } 
!    { 0, 1, 2 } { 3 } 
!    { 1, 2 } { 0, 3 } 
!    { 0 } { 2 } { 1, 3 } 
!    { 0, 2 } { 1, 3 } 
!    { 2 } { 0, 1, 3 } 
!    { 0 } { 1 } { 2, 3 } 
!    { 0, 1 } { 2, 3 } 
!    { 1 } { 0, 2, 3 } 
!    { 0 } { 1, 2, 3 } 
!    { 0, 1, 2, 3 } 

  main :−
   (for partition p in partitions(4)
    do (for var intSet s in elements(p)
        do write(s)
           write(' '))
       writeln())
)
