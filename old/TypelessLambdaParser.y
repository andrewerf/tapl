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
'='			{ TkEq }
In			{ TkIn }
';'			{ TkSemicolon }

%%

LetExpr	: Var '=' Terms ';' LetExpr
								{ TmsLet ( tkVarName $1 ) $3 $5 }
		| Var '=' Terms In Terms
								{ TmsLet ( tkVarName $1 ) $3 $5 }
		| Terms					{ $1 }

Terms	: Term					{ $1 }
		| Terms Term			{ TmsApp $1 $2 }

Term	: Var					{ TmsVar ( tkVarName $1 ) }
		| Abs Var '.' Terms		{ TmsAbs ( tkVarName $2 ) $4 }
		| '(' Terms ')'			{ $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

parse = parseTypelessLambda.alexScanTokens
}
