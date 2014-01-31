!
!  EG/CODON. Objects that represent grammar rules.
!
!  Copyright © 2014 James B. Moen.
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
  foj makeTag   :− enum()     !  Return a new codon tag.
  inj andTag    :− makeTag()  !  Identify a conjunction.
  inj nameTag   :− makeTag()  !  Identify a nonterminal.
  inj orTag     :− makeTag()  !  Identify a disjunction.
  inj rangeTag  :− makeTag()  !  Identify a range.
  inj stringTag :− makeTag()  !  Identify a terminal.

!  CODON. A general codon. Its TAG slot tells what kind of codon it is.

  codon :− (tuple int0 tag)

!  PAIR CODON. A series of grammar fragments. The first fragment is in CAR, and
!  the others are in CDR.

  pairCodon :−
   codon &
    (tuple
      ref codon     car,
      var ref codon cdr)

!  AND CODON. A conjunction. The first conjunct is in CAR and the others are in
!  CDR.

  andCodon :− pairCodon

!  NAME CODON. A nonterminal. Its name is CHARS and its definition is DEF. MARK
!  tells if it appears on the left side of some rule. NEXT links it into one of
!  TABLE's buckets.

  nameCodon :−
   codon &
    (tuple
      var bool          mark,
      string            chars,
      var ref codon     def,
      var ref nameCodon next)

!  OR CODON. A disjunction. The first disjunct is in CAR, and the others are in
!  CDR. The WIDTH is proportional to the probability that this OR CODON will be
!  chosen when generating strings from a grammar.

  orCodon :− pairCodon & (tuple var int width)

!  RANGE CODON. A sequence of BASEs. Its randomly chosen length is greater than
!  or equal to MIN, and less than or equal to MAX.

  rangeCodon :−
   codon &
    (tuple
      ref codon base,
      int       min,
      int       max)

!  STRING CODON. A terminal. Its name is CHARS.

  stringCodon :− codon & (tuple string chars)

!  ".". Return a slot in a codon. It lets us avoid most casts and dereferencers
!  in the rest of the program. We almost wish for OOP.

  "." :−
   (with
     rc :−  ref codon
     rnc :− ref nameCodon
     roc :− ref orCodon
     rpc :− ref pairCodon
     rrc :− ref rangeCodon
    do (alt
        (form (rc  this, type $base)  rc:       this{rrc}↑.base),
        (form (rc  this, type $car)   rc:       this{rpc}↑.car),
        (form (rc  this, type $cdr)   var rc:   this{rpc}↑.cdr),
        (form (rc  this, type $chars) string:   this{rnc}↑.chars),
        (form (rnc this, type $chars) string:   this{rnc}↑.chars),
        (form (rc  this, type $def)   var rc:   this{rnc}↑.def),
        (form (rnc this, type $def)   rc:       this     ↑.def),
        (form (rc  this, type $mark)  var bool: this{rnc}↑.mark),
        (form (rc  this, type $max)   int:      this{rrc}↑.max),
        (form (rc  this, type $min)   int:      this{rrc}↑.min),
        (form (rc  this, type $tag)   int:      this     ↑.tag),
        (form (rc  this, type $width) var int:  this{roc}↑.width)))

!  MAKE AND. Return a new conjunction. Its first conjunct is CAR and its others
!  are missing, but can be filled in later.

  makeAnd :−
   (proc (ref codon car) ref codon:
    (with ref var andCodon this :− fromHeap(var andCodon)
     do this↑.tag := andTag
        this↑.car := car
        this↑.cdr := nil
        this{ref codon}))

!  MAKE OR. Return a new disjunction. Its first disjunct is CAR, and its others
!  are missing, but can be filled in later. Initially its WIDTH is 1, so it has
!  the same probability of being chosen during generation as any OR CODON.

  makeOr :−
   (proc (ref codon car) ref codon:
    (with ref var orCodon this :− fromHeap(var orCodon)
     do this↑.tag   := orTag
        this↑.car   := car
        this↑.cdr   := nil
        this↑.width := 1
        this{ref codon}))

!  TABLE. A bucketed hash table of NAME CODONs that represent nonterminals. Its
!  length BUCKETS is a "large" prime. A bucket is a linear chain of NAME CODONs
!  that are linked through their NEXT slots.

  inj buckets :− 997

  var [buckets] ref nameCodon table :−
   (with var [buckets] ref nameCodon this
    do (for int index in length(this)
        do this[index] := nil)
       this)

!  MAKE NAME. Return a new nonterminal NAME CODON whose name is CHARS.

  makeName :−
   (proc (string chars) ref codon:
    (with

!  HASH. Return a nonnegative INT computed from all the CHAR0s in CHARS.

      hash :−
       (form (string chars) int:
        (with var int bits :− 0
         do (for char0 ch in elements(chars)
             do bits := (bits ← 1) ~ ch)
            (high(int) & bits) mod length(table)))

      int               index :− hash(chars)   !  Index into TABLE.
      var ref nameCodon this  :− table[index]  !  Bucket at INDEX in TABLE.

!  This is MAKE NAME's body. First find THIS, the bucket in TABLE that can have
!  a NAME CODON whose CHARS slot is CHARS. Then linearly search THIS for such a
!  NAME CODON. If we find it, then return it. Otherwise, make a new NAME CODON,
!  whose CHARS slot is a copy of CHARS, add it to THIS, and return it.

     do (while
         (if this = nil
          then (with ref var nameCodon that :− fromHeap(var nameCodon)
                do that↑.tag    := nameTag
                   that↑.mark   := false
                   that↑.chars  := copy(chars)
                   that↑.def    := nil
                   that↑.next   := table[index]
                   this         := that{ref nameCodon}
                   table[index] := this
                   false)
          else if this.chars = chars
               then false
               else this := this↑.next
                    true))
        this{ref codon}))

!  MAKE RANGE. Return a new RANGE whose base is BASE, minimum count is MIN, and
!  maximum count is MAX.

  makeRange :−
   (proc (ref codon base, int min, int max) ref codon:
    (with ref var rangeCodon this :− fromHeap(var rangeCodon)
     do this↑.tag  := rangeTag
        this↑.base := base
        this↑.min  := min
        this↑.max  := max
        this{ref codon}))

!  MAKE START. Return the definition of the nonterminal whose name is START. It
!  is an error if there is no such nonterminal.

  makeStart :−
   (proc () ref codon:
    (with ref codon start :− makeName(''start''){ref nameCodon}↑.def
     do (if start = nil
         then fail(''Undefined nonterminal 'start'.'')
         else start)))

!  MAKE STRING. Return a new terminal whose name is CHARS.

  makeString :−
   (proc (string chars) ref codon:
    (with ref var stringCodon this :− fromHeap(var stringCodon)
     do this↑.tag   := stringTag
        this↑.chars := copy(chars)
        this{ref codon}))
)
