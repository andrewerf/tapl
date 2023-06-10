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
'&'         { TkPt }
int         { TkInt _ }

%%

LetExpr	: var '=' Terms ';' LetExpr
								{ TmsLet ( tkVarName $1 ) $3 $5 }
		| var '=' Terms in Terms
								{ TmsLet ( tkVarName $1 ) $3 $5 }
        | type_var '=' Terms ';' LetExpr
                                { TmsLetType ( tkVarName $1 ) $3 $5 }
        | type_var '=' Terms in Terms
                                { TmsLetType ( tkVarName $1 ) $3 $5 }
		| Terms					{ $1 }

Terms	: Term					{ $1 }
		| Terms Term			{ TmsApp $1 $2 }

Types	: Type					{ $1 }
		| Types Type			{ TpsApp $1 $2 }

Kind    : '*'                   { KndsStar }
        | '*' '->' Kind         { KndsArrow KndsStar $3 }
        | Kind '->' Kind        { KndsArrow $1 $3 }
        | '(' Kind ')'          { $2 }

Type	: type_var			    { TpsVar ( tkVarName $1 ) }
		| type_var '->' Type	{ TpsArrow ( TpsVar ( tkVarName $1 ) ) $3 }
		| Type '->' Type    	{ TpsArrow $1 $3 }
        | '@' type_var ':' Kind '.' Type
                                { TpsPoly ( tkVarName $2 ) $4 $6 }
        | '&' type_var ':' Kind '.' Type
                                { TpsAbs ( tkVarName $2 ) $4 $6 }
        | '(' Types ')'         { $2 }

Term	: var					{ TmsVar ( BoundVar $ tkVarName $1 ) }
        | int                   { TmsVar ( DataVar ( TdInt $ tkIntVal $1 ) ) }
        | Types                 { TmsType $1 }
		| abs var ':' Types '.' Terms
								{ TmsAbs ( tkVarName $2 ) $4 ( TailAbs $6 ) }
        | abs type_var ':' '*' '.' Terms
                                { TmsPoly ( tkVarName $2 ) KndsStar $6 }
		| '(' Terms ')'			{ $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

parse = parseLambda2.alexScanTokens
}
