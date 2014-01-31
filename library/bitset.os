!
!  ORSON/LIB/BITSET. Finite sets of small integers.
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

!  This implementation was inspired by the SET type of the programming language
!  Pascal. It uses a bitstring as a finite set of nonnegative integers, so J is
!  in the set iff bit number J of the bitstring is 1. See:
!
!  Kathleen Jensen, Niklaus Wirth. Pascal User Manual and Report. Lecture Notes
!  in Computer Science #18. Springer-Verlag, Berlin, 1974. Pages 50–54.
!
!  Each set has a fixed upper bound on how many elements it can hold. All 2-ary
!  set operators require that their arguments have identical upper bounds.

(load ''lib.break'')     !  Terminate an iterator.
(load ''lib.file'')      !  Input and output on file streams.
(load ''lib.sequence'')  !  Iterators that transform to sequences.

(prog
  Binary  :− form (inj, inj) inj       !  Type of a 2-ary INJ operator.
  IntBits :− 8 × size(int)             !  How many bits in an INT.
  Unary   :− form (inj) inj            !  Type of a 1-ary INJ operator.
  Updater :− form (var int, int) void  !  Type of an INT updater.

!  SET. Return the type of a set of non negative integers that are less than M.
!  (Actually, the smallest multiple of INT BITS greater than or equal to M.) If
!  M is missing, then return a joker that describes an arbitrary set type.

  set :−
   (alt
    (form (inj m) type exe:
     (if isInt(m)
      then (if m ≥ 0
            then (tuple [(m + (− m & (IntBits − 1))) / IntBits] int Bits)
            else error($m, "non negative inj expression expected")
                 set(0))
      else error($m, "constant expected")
           set(0))),
    (form () type exe:
     (tuple [] int Bits)))

!  MAKE SET INT OPERATOR. Return a form that returns a set, computed from a set
!  L and an element R.

  MakeSetIntOperator :−
   (form (Updater a, Unary u) foj:
    (gen (type set() lType)
      form (lType l, inj r) lType:
       (with
         var lType l :− (past l)
         int r       :− (past r)
        do a(l.Bits[Index(r)], u(Mask(r)))
           l)))

!  MAKE SET INT UPDATER. Return a form that adds an element R to a set variable
!  L, or that removes an element R from a set variable L.

  MakeSetIntUpdater :−
   (form (Updater a, Unary n) foj:
    (gen (type set() lType)
      form (var lType l, int r) void:
       (with int r :− (past r)
        do a(l.Bits[Index(r)], n(Mask(r))))))

!  MAKE SET SET OPERATOR. Return a form that returns a set computed from sets L
!  and R.

  MakeSetSetOperator :−
   (form (Updater a, Unary u) foj:
    (gen (type set() lType)
      gen (type lType rType)
       form (lType l, rType r) lType:
        (with
          var lType l :− (past l)
          rType r     :− (past r)
         do (for inj j in Indexes(lType)
             do a(l.Bits[j], u(r.Bits[j])))
            l)))

!  MAKE SET SET UPDATER. Return a form that updates a set variable L using some
!  other set R.

  MakeSetSetUpdater :−
   (form (Updater a, Unary n) foj:
    (gen (type set() lType)
      gen (type lType rType)
       form (var lType l, rType r) void:
        (with rType r :− (past r)
         do (for inj j in Indexes(lType)
             do a(l.Bits[j], n(r.Bits[j]))))))

!  "+", "+=", "∪", "∪=". Add an element to a set or find the union of two sets.
!  We must define this first, because later definitions need "+" as an identity
!  form.

  "+" :−
   (alt
     MakeSetIntOperator("|=", (past "+")),
     MakeSetSetOperator("|=", (past "+")))

  "+=" :−
   (alt
     MakeSetIntUpdater("|=", "+"),
     MakeSetSetUpdater("|=", "+"))

  "∪"  :− "+"
  "∪=" :− "+="

!  "*", "*=", "∩", "∩=". Find the intersection of two sets.

  "*"  :− MakeSetSetOperator("&=", "+")
  "*=" :− MakeSetSetUpdater("&=", "+")

  "∩"  :− "*"
  "∩=" :− "*="

!  "-", "-=", "−", "−=". Remove an element from a set or find the difference of
!  two sets.

  "-" :−
   (alt
     MakeSetIntOperator("&=", "~"),
     MakeSetSetOperator("&=", "~"))

  "−" :− "-"

  "-=" :−
   (alt
     MakeSetIntUpdater("&=", "~"),
     MakeSetSetUpdater("&=", "~"))

  "−=" :− "-="

!  "<", "⊂". Test if the set L is a proper subset of the set R.

  "<" :−
   (gen (type set() lType)
     gen (type lType rType)
      form (lType l, rType r) bool:
       ¬ (l ⊇ r))

  "⊂" :− "<"

!  "<=", "⊆". Test if the set L is a subset of the set R.

  "<=" :−
   (gen (type set() lType)
     gen (type lType rType)
      form (lType l, rType r) bool:
       (with
         lType l :− (past l)
         rType r :− (past r)
         int   w :− length(lType.Bits)
        do (if w > 4
            then (with
                   var int j :− 0
                   var int t :− 0
                  do (while ¬ t ∧ j < w
                      do t := l.Bits[j] & ~ r.Bits[j]
                         j += 1)
                     ¬ t)
            else (with
                   making :−
                    (form (inj j) bool:
                     (if j < w − 1
                      then ¬ (l.Bits[j] & ~ r.Bits[j]) ∧ making(j + 1)
                      else ¬ (l.Bits[j] & ~ r.Bits[j])))
                  do making(0)))))

  "⊆" :− "<="

!  "<>", "≠". Test if the set L is not equal to the set R.

  "<>" :−
   (gen (type set() lType)
     gen (type lType rType)
      form (lType l, rType r) bool:
       ¬ (l = r))

  "≠" :− "<>"

!  "=". Test if the set L is equal to the set R.

  "=" :−
   (gen (type set() lType)
     gen (type lType rType)
      form (lType l, rType r) bool:
       (with
         lType l :− (past l)
         rType r :− (past r)
         int   w :− length(lType.Bits)
        do (if w > 4
            then (with
                   var int  j :− 0
                   var bool t :− true
                  do (while t ∧ j < w
                      do t := (l.Bits[j] = r.Bits[j])
                         j += 1)
                     t)
            else (with
                   making :−
                    (form (inj j) bool:
                     (if j < w − 1
                      then l.Bits[j] = r.Bits[j] ∧ making(j + 1)
                      else l.Bits[j] = r.Bits[j]))
                  do making(0)))))

!  ">", "⊃". Test if the set L is a proper superset of the set R.

  ">" :−
   (gen (type set() lType)
     gen (type lType rType)
      form (lType l, rType r) bool:
       ¬ (l ⊆ r))

  "⊃" :− ">"

!  ">=", "⊇". Test if the set L is a superset of the set R.

  ">=" :−
   (gen (type set() lType)
     gen (type lType rType)
      form (lType l, rType r) bool:
       r ⊆ l)

  "⊇" :− ">="

!  CARD. Return the cardinality of a set S. There are more efficient ways to do
!  this, but they depend on the size of an INT, which we want to avoid. See:
!
!  Henry S. Warren Jr. "The quest for an accelerated population count." In Andy
!  Oran and Greg Wilson (eds).  Beautiful Code. O'Reilly Media, Sebastopol, CA.
!  2007. Pages 147–160.

  card :−
   (gen (type set() sType)
     form (sType s) inj:
      (with
        var int b
        s :− (past s)
        var int t :− 0
       do (for inj j in Indexes(sType)
           do b := s.Bits[j]
              (while b
               do b &= b − 1
                  t += 1))
          t))

!  ELEMENTS. Iterator. Call BODY on every element of the set S. We can stop the
!  iterator by calling its breaker.

  elements :−
   (gen (type set() sType)
     form (sType s) foj:
      (alt
       (form (form (inj) obj body) obj:
        (with s :− (past s)
         do (for int j in limit(sType)
             do (if j ∊ s
                 then body(j))))),
       (form (form (breaker(), inj) obj body) obj:
        (with
          var int j :− 0
          var bool g :− true
          s :− (past s)
         do (while g ∧ j < limit(sType)
             do (if j ∊ s
                 then body(makeBreaker(g), j))
                j += 1)))))

!  INDEX. Return the index of the INT in a set that holds bit number J.

  Index :−
   (form (int j) int:
     j / IntBits)

!  INDEXES. Iterator. Call BODY on each index of the array which represents the
!  set S.  If S is small enough, then we transform to a sequence. If it is not,
!  then we transform to a loop.

  Indexes :−
   (gen (type set() sType)
     form (type sType s) foj:
      (with m :− length(sType.Bits)
       do (if m > 4
           then (form (form (inj) obj body) obj:
                 (with var int j :− 0
                  do (while j < m
                      do body(j)
                         j += 1)))
           else (form (form (inj) obj body) obj:
                 (for inj j in ints(m)
                  do body(j))))))

!  IS IN, "∊". Test if an integer R is an element of a set L.

  isIn :−
   (gen (type set() rType)
     form (inj l, rType r) bool:
      (with l :− (past l)
       do r.Bits[Index(l)] & Mask(l) ≠ 0))

  "∊" :− isIn

!  LIMIT. Return the maximum number of elements in a set, or a set type. It may
!  be larger than the number of elements requested.

  limit :−
   (alt
    (gen (type set() s)
      form (s) inj:
       limit(s)),
    (form (type set() s) inj:
      IntBits × length(s.Bits)))

!  MAKE SET. If M is a nonnegative integer constant, and B contains nonnegative
!  integer constants less than M, then return a set having the elements of B as
!  its elements.

  makeSet :−
   (form (inj m, list b) set():
    (with

!  ARE BITS OK. Test if every element in the list B is an integer constant that
!  is greater than or equal to 0 but less than M. Issue errors for all elements
!  that are not.

      areBitsOk :−
       (form (list b) bool:
        (if isEmpty(b)
         then true
         else if isInt(car(b))
              then (if 0 ≤ car(b) < m
                    then areBitsOk(cdr(b))
                    else areBitsOk(cdr(b))
                         error(b, "out of range", false))
              else areBitsOk(cdr(b))
                   error(b, "constant expected", false)))

!  MAKE INT. Return the INT at index J in the array that represents a set. Bits
!  that correspond to elements greater than or equal to H, but less than L, are
!  1's. The others are 0's.

      makeInt :−
       (form (list b, inj j) int:
        (with
          int l :− IntBits × j
          int h :− l + IntBits
          makingInt :−
           (form (int t, list b) int:
            (if isEmpty(b)
             then t
             else if l ≤ car(b) < h
                  then makingInt(t | Mask(car(b)), cdr(b))
                  else makingInt(t, cdr(b))))
         do makingInt(0, b)))

!  MAKE ASSIGNMENTS. Return a list of pairs (: J, K) where J is an index in the
!  array that represents a set, and K is the nonzero INT to be put at index J.

      makeAssignments :−
       (form (list b) list:
        (with
          makingAssignments :−
           (form (list a, inj j) list:
            (if j < m
             then (with k :− makeInt(b, j)
                   do (if k = 0
                       then makingAssignments(a, j + 1)
                       else makingAssignments(cons((: j, k), a), j + 1)))
             else a))
         do makingAssignments((:), 0)))

!  IS SPARSE. Test if we should initialize the set sparsely. The alternative is
!  to initialize it densely. Here A is a list of pairs as described above.

      isSparse :−
       (form (list a) bool:
         length(a) + 4 < m)

!  DENSELY ASSIGN. Transform to a sequence of M assignments that initialize the
!  array representing the set S. If J is an index in that array, and the list A
!  contains the pair (: J, K), then set the INT at index J to K. Otherwise, set
!  the INT at index J to 0.

      denselyAssign :−
       (form (var set() s, list a) void:
        (with
          denselyAssigning :−
           (form (list a, inj j) void:
            (if j < m
             then (if isEmpty(a) ∨ j ≠ caar(a)
                   then s.Bits[j] := 0
                        denselyAssigning(a, j + 1)
                   else s.Bits[j] := cadar(a)
                        denselyAssigning(cdr(a), j + 1))))
         do denselyAssigning(a, 0)))

!  SPARSELY ASSIGN. Transform to a loop, followed by some assignments. The loop
!  zeroes the array representing the set S. Then, the assignments reset certain
!  elements of this array. If the list A contains a pair (: J, K), then the INT
!  at index J will be reset to K.

      sparselyAssign :−
       (gen (type set() sType)
         form (var sType s, list a) void:
          (for inj j in Indexes(sType)
           do s.Bits[j] := 0)
          (for obj p in elements(a)
           do s.Bits[car(car(a))] := car(cdr(car(a)))))

!  Lost? This is MAKE SET's body. If M and B are legal then transform to a WITH
!  that makes a set S, initializes its elements, and returns it.

     do (if isInt(m)
         then (if m ≥ 0
               then (if isEmpty(b)
                     then (with var set(m) s
                           do (for inj j in Indexes(set(m))
                               do s.Bits[j] := 0)
                              s)
                     else if areBitsOk(b)
                          then (with
                                 var set(m) s
                                 list a :− makeAssignments(b)
                                do (if isSparse(a)
                                    then sparselyAssign(s, a)
                                    else denselyAssign(s, a))
                                   s)
                          else makeSet(m:))
               else error($m, "non negative inj expression expected")
                    makeSet(0:))
         else error($m, "constant expected")
              makeSet(0:))))

!  MASK. Return a mask that isolates bit number J in an INT.

  Mask :−
   (form (inj j) int:
     1{int} ← (j mod IntBits))

!  WRITE. Write a symbolic representation of a set S to an output stream O. For
!  example, { }, { 0 }, and { 0, 1, 2 } might be written. If O does not appear,
!  then it defaults to OUTPUT.

  write :−
   (alt
    (form (set() s) void:
      write(output, s)),
    (form (stream o, set() s) void:
     (with var string d :− '' ''
      do write(o, '{')
         (for inj e in elements(s)
          do write(o, d)
             write(o, e)
             d := '', '')
         write(o, '' }''))))

!  WRITELN. Like WRITE, but terminate output with an end of line character.

  writeln :−
   (alt
    (form (set() s) void:
      writeln(output, s)),
    (form (stream o, set() s) void:
      write(o, s)
      writeln(o)))
)
