!
!  APPS/SIEVE. The Sieve of Eratosthenes.
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

(load ''lib.bitset'')  !  Finite sets of small integers.
(load ''lib.file'')    !  Input and output on Unix file streams.

(prog

!  PRIMES. Iterator. Call BODY on each prime number between L and H, inclusive.
!  We find prime numbers with the Sieve of Eratosthenes, using a bitset to hold
!  the primes. This shows how to write an iterator, and how bitsets work.

  primes :−
   (form (int l, int h) foj:
    (form (form (int) obj body) obj:
     (with
       l :− max(2, (past l))
       h :− (past h)
      do (if l ≤ h
          then (with
                 m :− h − l + 2
                 var set(m) s :− makeSet(m:)
                do (for int j in m
                    do s += j)
                   (for int j in l, h
                    do (if (j − l) ∊ s
                        then body(j)
                             (for int k in 2 × j, m, j
                              do s −= k − l))))))))

!  MAIN. Write each prime between 1 and 10000 on one line, separated by blanks.
!  We can pipe the output through TABU to format the primes into a table.

  main :−
   (for int p in primes(1, 10000)
    do write(''%04i '': p))
)
