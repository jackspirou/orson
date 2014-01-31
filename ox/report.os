!
!  OX/REPORT. Write names and their line numbers.
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

!  REPORT NAMES. Write names in lexicographic order. Following each name, write
!  a list of the line numbers where the name appears. If NUMBERS PER LINE is 0,
!  then do not write line numbers at all.

  reportNames :−
   (form () void:
    (for string name, ref place places in names()
     do (with var int count :− 0
         do reportName(name)
            (if numbersPerLine = 0
             then writeln()
             else (for int number in numbers(places)
                   do (if count = numbersPerLine
                       then writeln()
                            (in columnsPerName
                             do write(' '))
                            count := 0)
                      write('' %0*i'': digitsPerNumber, number)
                      count += 1)
                  (if count > 0
                   then writeln())))))

!  NAMES. Iterator. Visit each NAME string and its corresponding PLACE chain in
!  the subtrees of ROOT.

  names :−
   (form () foj:
    (form (form (string, ref place) obj body) obj:
     (with

!  NAMING. Do all the work for NAMES. We traverse ROOT in inorder.

       naming :−
        (proc (ref tree root) void:
         (with var ref tree subtree :− root
          do (while subtree ≠ nil
              do (with
                   string    name  :− subtree↑.name
                   ref place first :− subtree↑.first
                  do naming(subtree↑.left)
                     body(name, first)
                     subtree := subtree↑.right))))

!  This is NAMES's body.

      do naming(root↑.left)
         naming(root↑.right))))

!  NUMBERS. Iterator. Visit each NUMBER in the chain of PLACEs that starts with
!  FIRST.

  numbers :−
   (form (ref place first) foj:
    (form (form (int) obj body) obj:
     (with var ref place this :− first
      do (while this ≠ nil
          do (with int number :− this↑.number
              do body(number)
                 this := this↑.next)))))

!  REPORT NAME. Write NAME left justified and blank filled, in COLUMNS PER NAME
!  chars.

  reportName :−
   (form (string name) void:
    (with var int columns :− 0
     do (for breaker() break, char ch in elements(name)
         do (if asciiing
             then (if columns + 1 > columnsPerName
                   then break()
                   else columns += 1
                        (if isAscii(ch)
                         then write(ch)
                         else write(''_'')))
             else (with int temp :− width(ch)
                   do (case temp
                       of −1: (if columns + 1 > columnsPerName
                               then break()
                               else columns += 1
                                    write(''□''))
                           0: (if accenting
                               then write(ch)
                               else if columns + 1 > columnsPerName
                                    then break()
                                    else columns += 1
                                         write('' '')
                                         write(ch))
                        none: (if columns + temp > columnsPerName
                               then break()
                               else columns += temp
                                    write(ch))))))
        (if numbersPerLine > 0
         then (in columnsPerName − columns
               do write('' '')))))
)

