!
!  EG/PARSE. Translate grammar rules to an internal form.
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
  ref codon emptyString :− makeString(ϵ)  !  The empty string as a CODON.
  type exe  tokenSet    :− set(maxToken)  !  A set of tokens.

!  MAKE TOKEN SET. Return a set containing the tokens in TOKENS.

  makeTokenSet :−
   (form (list tokens) tokenSet:
     makeSet(maxToken, tokens))

!  NEXT 1 GRAMMAR. Read a grammar from SOURCE, Pass One. If we encounter a name
!  that's followed by an arrow, then the name must be on the left of some rule.
!  MARK the name to indicate it will be defined by that rule during Pass Two.

  next1Grammar :−
   (form () void:
     next1Token()
     (while token ≠ endToken
      do (if token = nameToken
          then (with ref codon name :− tokenName
                do next1Token()
                   (if token = arrowToken
                    then name.mark := true
                         next1Token()))
          else next1Token())))

!  NEXT 2 GRAMMAR. Read a grammar from SOURCE, Pass Two. Compile the grammar to
!  a linked network of codons (see EG/CODON). The sets help recover from syntax
!  errors, sort of.

  importants      :− makeTokenSet(: endToken, nameToken, stringToken)
  numberFollowers :− makeTokenSet(: closeParenToken, commaToken) ∪ importants
  unitFollowers   :− makeTokenSet(: barToken, dotToken) ∪ importants

  next2Grammar :−
   (form () void:
    (with

!  NEXT RULE. Parse a rule, and set the DEF slot of the name on the left of the
!  arrow to the disjunction on the right of the arrow.

      nextRule :−
       (form () void:
        (if token = nameToken
         then (with ref codon name :− tokenName
               do (if name.def ≠ nil
                   then syntaxError(alreadyDefinedErr))
                  next2Token()
                  next2Token(arrowToken, arrowExpectedErr)
                  name.def := nextDisjunction())
         else syntaxError(nameExpectedErr)
              next2Token(arrowToken, arrowExpectedErr)
              nextDisjunction())
        next2Token(dotToken, dotExpectedErr))

!  NEXT DISJUNCTION. Parse a disjunction and return its representation.

      nextDisjunction :−
       (proc () ref codon:
        (with var ref codon first :− nextConjunction()
         do (if token = barToken
             then (with
                    var ref codon last
                    var ref codon next
                   do first := makeOr(first)
                      last := first
                      (while
                        next2Token()
                        next := makeOr(nextConjunction())
                        last.cdr := next
                        last := next
                        token = barToken)))
            first))

!  NEXT CONJUNCTION. Parse a conjunction and return its representation.

      nextConjunction :−
       (proc () ref codon:
        (with var ref codon first :− nextTerm()
         do (if token = nameToken ∨ token = stringToken
             then (with
                    var ref codon last
                    var ref codon next
                   do first := makeAnd(first)
                      last := first
                      (while
                        next := makeAnd(nextTerm())
                        last.cdr := next
                        last := next
                        token = nameToken ∨ token = stringToken)))
            first))

!  NEXT TERM. Parse a term and return its representation.

      nextTerm :−
       (proc () ref codon:
        (with var ref codon term

!  NEXT RANGE POSTFIX. Parse a range postfix and add it to TERM.

          nextRangePostfix :−
           (form () void:
            (with
              var int lower :− 0
              var int upper :− 0
             do next2Token()
                (if token = numberToken
                 then lower := tokenNumber
                      next2Token()
                 else syntaxError(numberExpectedErr)
                      (while ¬ (token ∊ numberFollowers)
                       do next2Token()))
                (if token = commaToken
                 then next2Token()
                      (if token = numberToken
                       then (if tokenNumber ≥ lower
                             then upper := tokenNumber
                             else syntaxError(rangeErr)
                                  upper := lower)
                             next2Token()
                       else syntaxError(numberExpectedErr)
                            (while ¬ (token ∊ numberFollowers)
                             do next2Token()))
                 else upper := lower)
                next2Token(closeParenToken, closeExpectedErr)
                term := makeRange(term, lower, upper)))

!  NEXT SINGLE POSTFIX. Parse a postfix, make an equivalent range, and add that
!  range to TERM.

          nextSinglePostfix :−
           (form (int lower, int upper) void:
             next2Token()
             term := makeRange(term, lower, upper))

!  This is NEXT TERM's body. We dispatch on the first token of its postfix.

         do term := nextUnit()
            (case token
             of openParenToken: nextRangePostfix()
                     plusToken: nextSinglePostfix(1, maxRepeat)
                 questionToken: nextSinglePostfix(0, 1)
                     starToken: nextSinglePostfix(0, maxRepeat))
            term))

!  NEXT UNIT. Parse a unit and return its representation. If a name has no MARK
!  then it never appears on the left of a rule, so it's not defined.

      nextUnit :−
       (form () ref codon:
        (if token = nameToken
         then (if ¬ tokenName.mark
               then syntaxError(undefinedNameErr))
              tokenName also next2Token()
         else if token = stringToken
              then makeString(tokenChars{string}) also next2Token()
              else syntaxError(termExpectedErr)
                   (while ¬ (token ∊ unitFollowers)
                    do next2Token())
                   emptyString))

!  This is NEXT 2 GRAMMAR's body. Parse a series of rules.

     do next2Token()
        (while token ≠ endToken
         do nextRule())))
)
