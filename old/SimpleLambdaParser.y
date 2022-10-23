{
module SimpleLambdaParser ( parse ) where

import SimpleLambdaLexer
import SimpleLambdaAST

}

%name parseSimpleLambda
%tokentype { Token }
%error { parseError }

%token
abs			{ TkAbs }
var			{ TkVar _ }
'.'			{ TkDot }
'('			{ TkLeftPar }
')'			{ TkRightPar }
'='			{ TkEq }
in			{ TkIn }
';'			{ TkSemicolon }
':'			{ TkColon }
'->'		{ TkArrow }
'*'         { TkStar }
'@'         { TkP }

%%

LetExpr	: var '=' Terms ';' LetExpr
								{ TmsLet ( tkVarName $1 ) $3 $5 }
		| var '=' Terms in Terms
								{ TmsLet ( tkVarName $1 ) $3 $5 }
		| Terms					{ $1 }

Terms	: Term					{ $1 }
		| Terms Term			{ TmsApp $1 $2 }

Type	: var					{ TypeVar ( tkVarName $1 ) }
		| var '->' Type			{ TypeArrow ( TypeVar ( tkVarName $1 ) ) $3 }

Term	: var					{ TmsVar ( tkVarName $1 ) }
		| abs var ':' Type '.' Terms
								{ TmsAbs ( tkVarName $2 ) $4 $6 }
		| '(' Terms ')'			{ $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

parse = parseSimpleLambda.alexScanTokens
}
