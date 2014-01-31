!
!  TLH/WRITE. Write results.
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

!  WRITE CAST FORM.  Write an Orson form "␣{}" to OUTPUT that converts a string
!  to an arbitrary nonnegative INT. (See TLH/HASH.)

  writeCastForm :−
   (form () void:
     writeln(''!  "␣{}". Convert CHARS to an arbitrary nonnegative INT.'')
     writeln()
     writeln(''" {}" :−'')
     writeln(''(form (string chars, type int) int:'')
     writeln('' (with'')
     writeln(''   var int    bits   :− 0'')
     writeln(''   var string chars  :− (past chars)'')
     writeln(''   int        "01⋯1" :− high(int)'')
     writeln(''  do (while chars↑'')
     writeln(''      do bits := (bits ← 1) ~ chars↑'')
     writeln(''         chars += 1)'')
     writeln(''     "01⋯1" & bits))''))

!  WRITE HASH FORM. Write an Orson form HASH to OUTPUT that contains values for
!  PRIMARY, SECONDARIES, and OFFSETS.

  writeHashForm :−
   (form (int primary, row var int secondaries, row var int offsets) void:
    (with
      int primaryWidth   :− width(primary)
      int secondaryWidth :− width(primary, secondaries)
     do writeln()
        writeln(''!  HASH. Return the index of KEY in the secondary table.'')
        writeln()
        writeln(''hash :−'')
        writeln('' (form (string key) int:'')
        writeln(''  (with int j :− key{int}'')
        writeln(''   do (case j mod %i'': primary)
        (for int primary in primary
         do (if primary = 0
             then write(''       of '')
             else write(''          ''))
            write(''%0*i: '': primaryWidth, primary)
            write(''j mod %*i '': secondaryWidth, secondaries[primary])
            write(''+ %i'': offsets[primary])
            writeln())
        (in primaryWidth + 6
         do write(' '))
        writeln(''none: 0)))'')))

!  WRITE HASH TABLE. Write an Orson form MAKE SECONDARY to OUTPUT that makes an
!  array of strings that are mapped to indexes by HASH. It has LENGTH elements,
!  and the strings are in the chain KEYS. Also, write what fraction of elements
!  are nonempty strings.

  writeHashTable :−
   (form (form (string) int hash, int length, ref key keys) void:
    (with
      var int        count      :− 0
      int            indexWidth :− width(length)
      var real       percent
      row var string table      :− makeArray(length, var string)

!  WRITE ASSIGNMENT. Write an assignment to an array element in MAKE SECONDARY.

      writeAssignment :−
       (form (string prefix, int index) void:
         write(prefix)
         write(''table[%0*i] := '': indexWidth, index)
         (if isEmpty(table[index])
          then write(''ϵ'')
          else write(''\'\'%s\'\''': table[index]))
         writeln())

!  This is WRITE HASH TABLE's body.

     do (for string chars in charses(keys)
         do table[hash(chars)] := chars
            count += 1)
        writeln()
        write(''!  MAKE SECONDARY. Return the secondary table. It's '')
        percent := 100.0 × (count{real} / length{real})
        write(''%i/%i = %G%%'': count, length, percent)
        writeln('' full.'')
        writeln()
        writeln(''makeSecondary :−'')
        writeln('' (form () [] string:'')
        writeln(''  (with var [%i] string table'': length)
        writeAssignment(''   do '', 0)
        (for int index in 1, length − 1
         do writeAssignment(''      '', index))
        writeln(''      table))'')))

!  WIDTH. Return how many columns are needed to write the nonnegative NUMBER in
!  base 10. Return the maximum number of columns that are needed to write every
!  number in the array NUMBERS, whose length is LENGTH.

  width :−
   (alt
    (form (int number) int:
      Width(number)),
    (form (int length, row var int numbers) int:
     (with var int maxWidth :− 0
      do (for int index in length
          do (with int temp :− width(numbers[index])
              do (if temp > maxWidth
                  then maxWidth := temp)))
         maxWidth)))

  Width :−
   (proc (int number) int:
    (with
      var int count  :− 0
      var int number :− (past number)
     do (while number ≠ 0
         do count += 1
            number /= 10)
        count))
)
