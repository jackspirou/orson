!
!  APPS/OLF. Write the titles of Orson library files.
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

!  OLF writes the title lines of Orson library files. This may be useful if you
!  want to write about them in comments. The OLF command looks like this.
!
!    olf φ₁ φ₂ ... φⱼ
!
!  Each subscripted φ is the name of an Orson library file, without its suffix.
!  OLF visits every such file in the Orson library and writes its title line to
!  OUTPUT. If φ is not the name of an Orson library file, then OLF just ignores
!  it without complaining.

(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.command'')  !  Process Unix command line arguments.
(load ''lib.dynamic'')  !  Dynamic memory allocation with explicit release.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.library'')  !  Get a string of library directory paths.
(load ''lib.path'')     !  Operations on Unix pathnames.

(prog
  inj  maxPathLength :− 1024                !  Max CHAR0s in a Unix path.
  list suffixes      :− (: ''op'', ''os'')  !  Orson file suffixes.

!  SEARCH. Search for files with NAME and SUFFIX in DIRECTORIES, then write the
!  title lines of the files we find. We assume files have title lines like that
!  of this file. Depending on the current directory, and on DIRECTORIES, we may
!  visit the same file more than once.

  search :−
   (proc (string directories, string name, string suffix) void:
    (for var buffer() path in coloned(directories, maxPathLength)
     do (if end(path) ≠ '/'
         then append(path, '/'))
        append(path, name)
        append(path, '.')
        append(path, suffix)
        (for bool ok, string path in canonical(path{string})
         do (if ok ∧ ¬ isVisited(path)
             then (for bool ok, stream source in opened(path, ''r'')
                   do (if ok
                       then (with var char ch :− read(source)
                             do (while ch = ' ' ∨ ch = '!' ∨ ch = eol
                                 do ch := read(source))
                                (while ch ≠ eol
                                 do write(ch)
                                    ch := read(source))
                                writeln())))))))

!  COLONED. Iterator. Visit each nonempty SUBSTRING in a colon delimited string
!  SUBSTRINGS. Each SUBSTRING is in a BUFFER of length LENGTH.

  coloned :−
   (form (string substrings, inj length) foj:
    (form (form (var buffer(length)) obj body) obj:
     (with
       var buffer(length) substring
       var string substrings :− (past substrings)
      do (while substrings↑
          do empty(substring)
             (while
              (if substrings↑ = ':'
               then substrings += 1
                    false
               else if substrings↑
                    then append(substring, substrings↑)
                         substrings += 1
                         true
                    else false))
             (if ¬ isEmpty(substring)
              then body(substring))))))

!  TREE. A node in a binary search tree that holds filename PATHs. Nodes in the
!  LEFT subtree have PATH slots less than PATH. Those in the RIGHT subtree have
!  PATH slots greater than PATH.

  tree :−
   (tuple
     string       path,
     var ref tree left,
     var ref tree right)

!  MAKE TREE. Return a binary search tree leaf node whose key is PATH.

  makeTree :−
   (form (string path) ref tree:
    (with ref var tree this :− fromHeap(var tree)
     do this↑.path  := copy(path)
        this↑.left  := nil
        this↑.right := nil
        this{ref tree}))

!  IS VISITED. Test if the pathname PATH has been visited before. We keep PATHs
!  in a binary search tree ROOT. A linear chain would have been good enough but
!  this is more fun.

  var ref tree root :− makeTree(ϵ)

  isVisited :−
   (form (string path) bool:
    (with
      var bool     found   :− false
      string       path    :− (past path)
      var ref tree subtree :− root
     do (while
         (if path < subtree↑.path
          then (if subtree↑.left = nil
                then subtree↑.left := makeTree(path)
                     false
                else subtree := subtree↑.left
                     true)
          else if path > subtree↑.path
               then (if subtree↑.right = nil
                     then subtree↑.right := makeTree(path)
                          false
                     else subtree := subtree↑.right
                          true)
               else found := true
                    false))
        found))

!  MAIN. Main program. Visit files named on the command line.

  main :−
   (for char, string file in command(ϵ, '' '')
    do (for obj suffix in elements(string, suffixes)
        do search(libraries(), file, suffix)))
)
