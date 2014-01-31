!
!  EG/DEBUG. Write grammar fragments for debugging.
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

!  DEBUG. If we're TRACING, then write the generator's current recursive DEPTH,
!  and THIS, the codon we're generating strings from.

  debug :−
   (form (int depth, ref codon this) void:
    (if tracing
     then write(''[%i] '': depth)
          debugging(this)
          writeln()))

!  DEBUGGING. Write THIS as a fragment of a grammar rule.

  debugging :−
   (proc (ref codon this) void:
    (with
      var ref codon this :− (past this)

!  DEBUGGING AND. Write the conjunction THIS as a sequence of one or more items
!  separated by blanks.

      debuggingAnd :−
       (form () void:
         debugging(this.car)
         this := this.cdr
         (while this ≠ nil
          do write(' ')
             debugging(this.car)
             this := this.cdr))

!  DEBUGGING NAME. Write the terminal THIS as a string of CHARS.

      debuggingName :−
       (form () void:
         write(this.chars))

!  DEBUGGING NIL. Write NIL as a symbol that represents it.

      debuggingNil :−
       (form () void:
         write(''[Nil]''))

!  DEBUGGING OR. Write the disjunction THIS as a sequence of one or more items,
!  separated by bars.

      debuggingOr :−
       (form () void:
         debugging(this.car)
         this := this.cdr
         (while this ≠ nil
          do write('' | '')
             debugging(this.car)
             this := this.cdr))

!  DEBUGGING RANGE. Write the range THIS as an item followed by MIN and MAX, in
!  parentheses.

      debuggingRange :−
       (form () void:
         debugging(this.base)
         write(''(%i, %i)'': this.min, this.max))

!  DEBUGGING STRING. Write the terminal THIS as a string of its CHARS. We write
!  zero-width characters as hexadecimal slashed characters.

      debuggingString :−
       (form () void:
         write('"')
         (for char ch in elements(this.chars)
          do (if ch = '"'
              then write(''\\"'')
              else if ch = '\\'
                   then write(''\\\\'')
                   else if width(ch) ≥ 1
                        then write(ch)
                        else write(''\\#%X'': ch)))
         write('"'))

!  DEBUGGING UNKNOWN. Write an unknown codon, including its TAG. This shouldn't
!  happen.

      debuggingUnknown :−
       (form () void:
         write(''[Unknown %i]'': this.tag))

!  This is DEBUG's body. Dispatch on the TAG slot of THIS.

     do (if this = nil
         then debuggingNil()
         else (case this.tag
               of andTag: debuggingAnd()
                 nameTag: debuggingName()
                   orTag: debuggingOr()
                rangeTag: debuggingRange()
               stringTag: debuggingString()
                    none: debuggingUnknown()))))

!  DEBUG TABLE. Visit each nonterminal from TABLE (see EG/CODON), and write its
!  DEF slot as a grammar rule. This is never called.

  debugTable :−
   (form () void:
    (for int index in length(table)
     do (with ref nameCodon name :− table[index]
         do (if name ≠ nil
             then write(name.chars)
                  write('' → '')
                  debugging(name.def)
                  writeln('' .'')))))
)
