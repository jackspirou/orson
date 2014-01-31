!
!  ORSON/LIB/MATCH. Test if a string is a member of a list of strings.
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

(load ''lib.cxr'')     !  Compositions of CARs and CDRs.
(load ''lib.string'')  !  Operations on strings.

(prog

!  IS MATCH.  Suppose that K is a string valued expression, and the subscripted
!  K's are distinct constant strings. Then:
!
!    isMatch(K: K₁, K₂ ..., Kⱼ)
!
!  returns true if some string Kᵢ (1 ≤ i ≤ j) equals K, and false otherwise.
!
!  If K is constant, then we find Kᵢ by transformation and we return a constant
!  Boolean value. If K is not a constant, then we transform into a balanced BST
!  made out of IF clauses. It can find Kᵢ at execution time, in O(log j) string
!  comparisons.
!
!  If you want to return a value associated with each subscripted K, then think
!  about using SELECT from LIB.SELECT instead.

  isMatch :−
   (with

!  ARE STRINGS. Test if L is a list of zero or more string constants, and issue
!  error messages for elements that are not.

     areStrings :−
      (form (list l) bool:
       (if isEmpty(l)
        then true
        else if isString(car(l))
             then areStrings(cdr(l))
             else areStrings(cdr(l))
                  error(l, "unexpected element", false)))

!  SORT. Sort a list of strings S into nondecreasing order.

     sort :−
      (form (list s) list:
       (with
         sorting :−
          (form (list s) list:
           (if isEmpty(s) ∨ isEmpty(cdr(s))
            then s
            else partitioning((:), s[length(s) / 2], (:), s)))
         partitioning :−
          (form (list l, string m, list r, list s) list:
           (if isEmpty(s)
            then conc(sorting(l), sorting(r))
            else if car(s) < m
                 then partitioning(cons(car(s), l), m, r, cdr(s))
                 else partitioning(l, m, cons(car(s), r), cdr(s))))
        do sorting(s)))

!  HAS DUPLICATES. Test if a sorted list of strings s has duplicate keys. Issue
!  error messages for any duplicate keys we find.

     hasDuplicates :−
      (form (list s) bool:
       (if isEmpty(s) ∨ isEmpty(cdr(s))
        then false
        else if car(s) = cadr(s)
             then hasDuplicates(cdr(s))
                  error(s, "unexpected element", true)
             else hasDuplicates(cdr(s))))

!  Lost? This is IS MATCH's body. If S is not syntactically correct then return
!  false.

    do (form (string k, list s) bool:
        (if ¬ areStrings(s)
         then false
         else (with s :− sort((past s))
               do (if hasDuplicates(s)
                   then false

!  Otherwise, find a matching case at transformation time, by linear search...

                   else if isString(k)
                        then (with
                               isMatching :−
                                (form (list s) bool:
                                 (if isEmpty(s)
                                  then false
                                  else (with t :− comp(k, car(s))
                                        do (if t < 0
                                            then isMatching(cdr(s))
                                            else t = 0))))
                              do isMatching(s))

!  ... or at run time, by transforming to a balanced binary search tree made of
!  IF's. This looks a lot like binary search.

                        else (with
                               var int t
                               k :− (past k)
                               isMatching :−
                                (form (inj l, inj r) bool:
                                 (if l < r
                                  then (with m :− (l + r) / 2
                                        do t := comp(k, s[m])
                                           (if t < 0
                                            then isMatching(l, m − 1)
                                            else if t > 0
                                                 then isMatching(m + 1, r)
                                                 else true))
                                  else if l > r
                                       then false
                                       else k = s[l]))
                              do isMatching(0, length(s) − 1)))))))

)
