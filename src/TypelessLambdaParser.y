{
module TypelessLambdaParser ( parse ) where

import TypelessLambdaLexer
import TypelessLambdaAST

}

%name parseTypelessLambda
%tokentype { Token }
%error { parseError }

%token
Abs			{ TkAbs }
Var			{ TkVar _ }
'.'			{ TkDot }
'('			{ TkLeftPar }
')'			{ TkRightPar }
Let			{ TkLet }
'='			{ TkEq }
In			{ TkIn }

%%

Terms	: Term					{ $1 }
		| Terms Term			{ TmsApp $1 $2 }

Term	: Var					{ TmsVar ( tkVarName $1 ) }
		| Abs Var '.' Terms		{ TmsAbs ( tkVarName $2 ) $4 }
		| Let Var '=' Terms In Terms
								{ TmsLet ( tkVarName $2 ) $4 $6 }
		| '(' Terms ')'			{ $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

parse = parseTypelessLambda.alexScanTokens
}
