!
!  TLH/ARRAY. Arrays from the heap.
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

!  MAKE ARRAY. Return a pointer to a heap array that has LENGTH elements of the
!  type VAR BASE TYPE. Initialize its elements to DEFAULT values.

  makeArray :−
   (gen (type mut baseType)
     form (inj length, type var baseType) row var baseType:
      (with
        inj              length :− (past length)
        row var baseType this   :− fromHeap(length, var baseType)
       do (for int index in length
           do this[index] := default(baseType))
          this))

!  UNMAKE ARRAY. Send a heap array back to the heap.

  unmakeArray :− toHeap
)

