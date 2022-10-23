{
module SimpleLambdaLexer where
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
\:				{ \_ -> TkColon }
\->				{ \_ -> TkArrow }
\*              { \_ -> TkStar }
\@              { \_ -> TkP }

{

data Token =
  TkAbs |
  TkVar String |
  TkLeftPar |
  TkRightPar |
  TkDot |
  TkEq |
  TkIn |
  TkSemicolon |
  TkColon |
  TkArrow |
  TkStar |
  TkP
  deriving ( Eq, Show )

tkVarName :: Token -> String
tkVarName ( TkVar name ) = name
tkVarName _ = ""

}