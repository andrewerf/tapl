{
module TypelessLambdaLexer where
}

%wrapper "basic"

tokens :-

$white+		;
\\				{ \s -> TkAbs }
[a-zA-Z]		{ \s -> TkVar s }
\( 				{ \_ -> TkLeftPar }
\) 				{ \_ -> TkRightPar }
.				{ \_ -> TkDot }

{

data Token =
  TkAbs |
  TkVar String |
  TkLeftPar |
  TkRightPar |
  TkDot
  deriving ( Eq, Show )

tkVarName :: Token -> String
tkVarName ( TkVar name ) = name
tkVarName _ = ""

}