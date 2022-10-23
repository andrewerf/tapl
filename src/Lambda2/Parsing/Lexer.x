{
module Lambda2.Parsing.Lexer where
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$greek = [α-ζΑ-Ζ]
@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

$white+		;
\\				{ \s -> TkAbs }
=				{ \_ -> TkEq }
in				{ \_ -> TkIn }
$alpha [$alpha $digit \_ \']*
				{ \s -> TkVar s }
$greek [$greek $digit \_ \']*
                { \s -> TkTypeVar s }
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
  TkTypeVar String |
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
tkVarName ( TkTypeVar name ) = name
tkVarName _ = ""

}