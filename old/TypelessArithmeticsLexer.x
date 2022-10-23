{
module TypelessArithmeticsLexer where
}

%wrapper "basic"

tokens :-

$white+		;
true		{ \_ -> TkTrue }
false		{ \_ -> TkFalse }
if			{ \_ -> TkIf }
then		{ \_ -> TkThen }
else		{ \_ -> TkElse }
0			{ \_ -> TkZero }
succ		{ \_ -> TkSucc }
pred		{ \_ -> TkPred }
iszero		{ \_ -> TkIsZero }
\(			{ \_ -> TkLeftPar }
\)			{ \_ -> TkRightPar }

{
data Token =
  TkTrue |
  TkFalse |
  TkIf |
  TkThen |
  TkElse |
  TkZero |
  TkSucc |
  TkPred |
  TkIsZero |
  TkLeftPar |
  TkRightPar
  deriving ( Eq, Show )
}
