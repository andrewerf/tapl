{
module TypelessArithmeticsParser ( parse ) where

import TypelessArithmeticsLexer
import TypelessArithmeticsAST
}

%name parseTypelessArithmetics
%tokentype { Token }
%error { parseError }

%token
true		{ TkTrue }
false		{ TkFalse }
if			{ TkIf }
then		{ TkThen }
else		{ TkElse }
'0'			{ TkZero }
succ		{ TkSucc }
pred		{ TkPred }
iszero		{ TkIsZero }
'('			{ TkLeftPar }
')'			{ TkRightPar }

%%

Term	: succ '(' Term ')'				{ TmSucc $3 }
		| pred '(' Term ')'				{ TmPred $3 }
		| iszero '(' Term ')'			{ TmIsZero $3 }
		| if Term then Term else Term	{ TmIf $2 $4 $6 }
		| '0'							{ TmZero }
		| true							{ TmTrue }
		| false							{ TmFalse }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parse = parseTypelessArithmetics.alexScanTokens
}
