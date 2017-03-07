EqualityTester. A Mathematica package for testing equalities numerically
------------------------------------------------------------------------

EqualityTester compares expressions by assigning random values to all symbols in the expressions. The comparison function, number of comparisons can be controlled and the distribution and type of values can be modified on a pattern-by-pattern basis. 

__EqualityTest[lhs, rhs, [distributions,] [Options]]__  
compares lhs and rhs numerically by assigning values to all symbols as  specified by `distributions`, calculating a distance between `lhs` and `rhs`  using `DistanceMeasure` and comparing it to `Tolerance`. This is repeated `MaxIterations` times. The distributions parameter must be a list {p_1,p_2,...} where p_i is either  Automatic, a pattern  or a delayed rule `pattern:>RNG`, e.g. `x:>RandomReal[{-1,1}]`.  
It defaults to `{Automatic}`.   All components can be `Automatic`, e.g

  - `Automatic:>Automatic`
  - `Automatic:>RandomReal[{0,1}]`
  - `Subscript[y,_]:>Automatic (equivalent to Subscript[y,_] )`

The default/`Automatic` pattern is `s_Symbol/;Not@Or[ValueQ[s],NumericQ[s]]`. The default/`Automatic` RNG is `RandomReal[NormalDistribution[]]`, this can be changed with the `DefaultRNG` option."

__MakeEqualityTester[{p1,p2,...}]__  
returns an equality test function so that `MakeEqualityTester[args][lhs,rhs]` is equivalent to `EqualityTest[lhs,rhs,args___]`.

 . . .

(C) janus@insignificancegalore.net, 2010
EqualityTester is released under Creative Commons Attribution-ShareAlike 3.0 Unported License, see http://creativecommons.org/licenses/by-sa/3.0/.


