{
module TypelessLambdaLexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

$white+		;
\\				{ \s -> TkAbs }
=				{ \_ -> TkEq }
in				{ \_ -> TkIn }
$alpha [$alpha $digit \_ \']*
				{ \s -> TkVar s }
\( 				{ \_ -> TkLeftPar }
\) 				{ \_ -> TkRightPar }
\.				{ \_ -> TkDot }
\;				{ \_ -> TkSemicolon }

{

data Token =
  TkAbs |
  TkVar String |
  TkLeftPar |
  TkRightPar |
  TkDot |
  TkEq |
  TkIn |
  TkSemicolon
  deriving ( Eq, Show )

tkVarName :: Token -> String
tkVarName ( TkVar name ) = name
tkVarName _ = ""

}