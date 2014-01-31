!
!  TLH/MAIN. Main program.
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

!  MAIN. Main program. Read strings from a file with pathname PATH, then search
!  through possible values for BEST PRIMARY, BEST SECONDARIES, and BEST OFFSETS
!  to define a hash form BEST HASH. Write the results if successful.

  main :−
   (form (string path) void:
    (with
      var int         bestLength      :− high(int)
      var row var int bestOffsets     :− nil
      var int         bestPrimary     :− 0
      var row var int bestSecondaries :− nil
      ref key         keys            :− readKeys(path)

!  BEST HASH. The best hash form created by MAIN, if it exists.

      bestHash :−
       (form (string chars) int:
        (with
          int bits  :− chars{int}
          int index :− bits mod bestPrimary
         do bits mod bestSecondaries[index] + bestOffsets[index]))

!  This is MAIN's body.

     do (for int primary in minPrimary, maxPrimary
         do (catch
             (with
               var int     count       :− 0
               var int     length      :− 0
               var int     offset      :− 0
               row var int offsets     :− makeArray(primary, var int)
               row var int secondaries :− makeArray(primary, var int)
              do (for ref key keys in primaries(keys, primary)
                  do (if keys = nil
                      then offsets[count] := offset
                           secondaries[count] := 1
                           count += 1
                      else (with int secondary :− (past secondary)(keys)
                            do (if secondary = 0
                                then unmakeArray(offsets)
                                     unmakeArray(secondaries)
                                     throw()
                                else (if offset + secondary > length
                                      then length := offset + secondary)
                                     offsets[count] := offset
                                     secondaries[count] := secondary
                                     offset += secondary
                                     count += 1))))
                 (if length < bestLength
                  then unmakeArray(bestOffsets)
                       unmakeArray(bestSecondaries)
                       bestLength      := length
                       bestOffsets     := offsets
                       bestPrimary     := primary
                       bestSecondaries := secondaries
                  else unmakeArray(offsets)
                       unmakeArray(secondaries)))))
        (if bestPrimary ≠ 0
         then writeCastForm()
              writeHashForm(bestPrimary, bestSecondaries, bestOffsets)
              writeHashTable(bestHash, bestLength, keys))))

!  PRIMARIES. Iterator. Use the primary modulus PRIMARY to distribute KEYS into
!  a hash table BUCKETS in which collisions are resolved by chaining. Visit the
!  chains of colliding KEYS in BUCKETS. Finally destroy BUCKETS.

  primaries :−
   (form (ref key keys, int primary) foj:
    (form (form (ref key) obj body) obj:
     (with
       var int         index   :− 0
       int             primary :− (past primary)
       row var ref key buckets :− makeArray(primary, var ref key)
      do (for string chars in charses(keys)
          do (with int index :− hash(chars, primary)
              do buckets[index] := chars ⊕ buckets[index]))
         (while index < primary
          do (with ref key keys :− buckets[index]
              do body(keys)
                 index += 1))
         (for int index in primary
          do unmakeKeys(buckets[index]))
         unmakeArray(buckets))))

!  SECONDARY. Search for the secondary modulus that (1) distributes KEYS into a
!  binary search tree ROOT without collisions, and (2) has the minimum range of
!  hash values. Then destroy ROOT and return the secondary modulus. Return 0 if
!  it does not exist.

  secondary :−
   (form (ref key keys) int:
    (with
      var int bestSecondary :− 0
      var int bestRange     :− high(int)
      ref key keys          :− (past keys)
     do (for int secondary in minSecondary, maxSecondary
         do (with
              var bool collide :− false
              var int  lower   :− high(int)
              ref tree root    :− makeTree()
              var int  upper   :− 0
             do (for breaker() break, string chars in charses(keys)
                 do (with int index :− hash(chars, secondary)
                     do (if index ∊ root
                         then collide := true
                              break()
                         else (if index < lower
                               then lower := index)
                              (if index > upper
                               then upper := index))))
                unmakeTree(root)
                (if ¬ collide ∧ upper − lower < bestRange
                 then bestRange     := upper − lower
                      bestSecondary := secondary)))
        bestSecondary))
)
