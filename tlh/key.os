!
!  TLH/KEY. Linear chains of strings used as hash keys.
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

!  KEY. A node in a linked chain of hash keys. The hash key is the string CHARS
!  and NEXT points to the next node in the chain.

  key :−
   (tuple
     string      chars,
     var ref key next)

!  CHARSES. Iterator. Visit the CHARS slot of each KEY in the chain KEYS, in an
!  arbitrary order. We can stop the iterator by calling its breaker.

  charses :−
   (form (ref key keys) foj:
    (alt
     (form (form (string) obj body) obj:
      (with var ref key keys  :− (past keys)
       do (while keys ≠ nil
           do (with string chars :− keys↑.chars
               do body(chars)
                  keys := keys↑.next)))),
    (form (form (breaker(), string) obj body) obj:
     (with
       var bool    going :− true
       var ref key keys  :− (past keys)
      do (while going ∧ keys ≠ nil
          do (with string chars :− keys↑.chars
              do body(makeBreaker(going), chars)
                 keys := keys↑.next))))))

!  "⊕". Return a pointer to a new KEY with given CHARS and NEXT slots.

  "⊕" :−
   (proc (string chars, ref key next) ref key:
    (with ref var key this :− fromHeap(var key)
     do this↑.chars := chars
        this↑.next  := next
        this{ref key}))

!  READ KEYS. Return a chain of KEYS whose CHARS slots are lines of a text file
!  whose pathname is PATH.  Ignore blank lines.  If "␣{}" casts two CHARS slots
!  to the same INT, then TLH reports a "collision". You may be able to fix this
!  by redefining "␣{}" (see TLH/HASH).

  readKeys :−
   (form (string path) ref key:
    (with var ref key keys :− nil
     do (for bool ok, stream input in opened(path, ''r'')
         do (if ok
             then (for var buffer() line in lines(input, maxKeyLength)
                   do (with string chars :− line{string}
                       do (if chars↑
                           then (if chars ∊ keys
                                 then fail(''Collision at '%s'.'': chars)
                                 else keys := copy(chars) ⊕ keys))))
             else fail(''Cannot open '%s'.'': path)))
        keys))

!  "∊". Test if CHARS is cast to the same INT as a CHARS slots in KEYS.

  "∊" :−
   (form (string chars, ref key keys) bool:
    (with
      var bool    found :− false
      var ref key keys  :− (past keys)
      int         temp  :− chars{int}
     do (while ¬ found ∧ keys ≠ nil
         do found := (temp = keys↑.chars{int})
            keys := keys↑.next)
        found))

!  UNMAKE KEYS. Send the KEYs in the chain KEYS back to the heap.

  unmakeKeys :−
   (form (var ref key keys) void:
    (with var ref key keys :− (past keys)
     do (while keys ≠ nil
         do keys := (keys↑.next also toHeap(keys)))))
)
