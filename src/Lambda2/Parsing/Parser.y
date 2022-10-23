{
module Lambda2.Parsing.Parser ( parse ) where

import Lambda2.Parsing.Lexer
import Lambda2.Core.AST

}

%name parseLambda2
%tokentype { Token }
%error { parseError }

%token
abs			{ TkAbs }
var			{ TkVar _ }
type_var    { TkTypeVar _ }
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

Type	: type_var			    { TpsVar ( tkVarName $1 ) }
		| type_var '->' Type	{ TpsArrow ( TpsVar ( tkVarName $1 ) ) $3 }
        | '@' type_var '.' Type { TpsPoly ( tkVarName $2 ) $4 }
        | '(' Type ')'          { $2 }

Term	: var					{ TmsVar ( tkVarName $1 ) }
        | Type                  { TmsType $1 }
		| abs var ':' Type '.' Terms
								{ TmsAbs ( tkVarName $2 ) $4 $6 }
        | abs type_var ':' '*' '.' Terms
                                { TmsPoly ( tkVarName $2 ) $6 }
		| '(' Terms ')'			{ $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

parse = parseLambda2.alexScanTokens
}
