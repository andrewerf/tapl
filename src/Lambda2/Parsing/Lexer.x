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
[A-Z]  [$alpha $digit \_ \']*
                { \s -> TkTypeVar s }
$alpha [$alpha $digit \_ \']*
				{ \s -> TkVar s }
$greek [$greek $digit \_ \']*
                { \s -> TkTypeVar s }
0               { \_ -> TkInt 0 }
[1-9][$digit]*  { \s -> TkInt ( read s :: Int ) }
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
  TkP |
  TkInt Int
  deriving ( Eq, Show )

tkVarName :: Token -> String
tkVarName ( TkVar name ) = name
tkVarName ( TkTypeVar name ) = name
tkVarName _ = ""

tkIntVal :: Token -> Int
tkIntVal ( TkInt i ) = i
tkIntVal tk = error ( "Trying to get integer value from " ++ show tk )


}