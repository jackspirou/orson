!
!  ORSON/LIB/SUBSUP. Write integers using subscript or superscript digits.
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

(load ''lib.file'')  !  Input and output on file systems.

(prog

!  MAKE WRITE. Return a form that writes the integer K to the stream S, using a
!  procedure WRITER. The stream S defaults to OUTPUT.

  MakeWrite :−
   (form (proc (stream, int) void writer) foj:
    (alt
     (form (int k) void:
       writer(output, k)),
     (form (stream s, int k) void:
       writer(s, k))))

!  MAKE WRITER. Return a procedure that writes an integer K to a stream S using
!  digits computed by DIGIT, optionally prefixed by a MINUS sign.

  MakeWriter :−
   (form (char minus, form (int) char digit) pro:
    (proc (stream s, int k) void:
     (with

!  WRITING. Write a positive nonzero integer K to S.

       writing :−
        (proc (int k) void:
         (if k > 0
          then writing(k / 10)
               write(s, digit(k mod 10))))

!  This is MAKE WRITER's body. We dispatch on the sign of K.

      do (if k < 0
          then write(s, minus)
               writing(− k)
          else if k = 0
               then write(s, digit(0))
               else writing(k)))))

!  WRITE SUB. Write an integer using subscript characters.

  SubWriter :−
   MakeWriter('₋',
    (form (int d) char:
     (case d
      of 0: '₀'
         1: '₁'
         2: '₂'
         3: '₃'
         4: '₄'
         5: '₅'
         6: '₆'
         7: '₇'
         8: '₈'
         9: '₉'
      none: ' ')))

  writeSub :− MakeWrite(SubWriter)

!  WRITE SUP. Write an integer using superscript characters.

  SupWriter :−
   MakeWriter('⁻',
    (form (int d) char:
     (case d
      of 0: '⁰'
         1: '¹'
         2: '²'
         3: '³'
         4: '⁴'
         5: '⁵'
         6: '⁶'
         7: '⁷'
         8: '⁸'
         9: '⁹'
      none: ' ')))

  writeSup :− MakeWrite(SupWriter)
)
