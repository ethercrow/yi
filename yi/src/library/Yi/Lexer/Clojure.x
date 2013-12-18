-- -*- haskell -*-

{
#define NO_ALEX_CONTEXTS
{-# OPTIONS -w  #-}
module Yi.Lexer.Clojure (initState, alexScanToken, Token(..)) where
import Yi.Lexer.Alex
import Yi.Style

}

@clj_cond = cond
    | "cond->"
    | "cond->>"
    | "condp"
    | "if-let"
    | "if-not"
    | "when"
    | "when-first"
    | "when-let"
    | "when-not"

@clj_special = def
    | do
    | finally
    | fn
    | if
    | let
    | loop
    | "monitor-enter"
    | "monitor-exit"
    | new
    | quote
    | recur
    | "set!"
    | throw
    | try
    | var

@clj_bool = true | false

@clj_nil = nil

@clj_destructuring = ":as"
    | ":keys"
    | "&"

@clj_def = definline
    | definterface
    | defmacro
    | defmethod
    | defmulti
    | defn
    | "defn-"
    | defonce
    | defprotocol
    | defrecord
    | defstruct
    | deftype

@clj_macros = "->"
    | "->>"
    | "\.\."
    | amap
    | and
    | areduce
    | "as->"
    | assert
    | binding
    | "bound-fn"
    | comment
    | declare
    | delay
    | dosync
    | doto
    | "extend-protocol"
    | "extend-type"
    | for
    | future
    | "gen-class"
    | "gen-interface"
    | import
    | "io!"
    | "lazy-cat"
    | "lazy-seq"
    | letfn
    | locking
    | memfn
    | ns
    | or
    | proxy
    | "proxy-super"
    | pvalues
    | "refer-clojure"
    | reify
    | "some->"
    | "some->>"
    | sync
    | time
    | "with-bindings"
    | "with-in-str"
    | "with-loading-context"
    | "with-local-vars"
    | "with-open"
    | "with-out-str"
    | "with-precision"
    | "with-redefs"

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\[\]\']

$ascdigit     = 0-9
$unidigit     = [] -- TODO
$digit        = [$ascdigit $unidigit]

$large        = [A-Z \xc0-\xd6 \xd8-\xde]
$small        = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha        = [$small $large]

$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol    = [] -- TODO
$symbol       = [$ascsymbol $unisymbol] # [$special \_]


$graphic      = [$small $large $symbol $digit $special \"]

$nonzerodigit = 1-9
$octit        = 0-7
$hexit        = [0-9 A-F a-f]
$idchar       = [$alpha $digit \- \? \* \>]
$symchar      = [$symbol]

@digits = $nonzerodigit $digit*
@octits = "0"  $octit
@hexits = "0x" $hexit

@integer     = @digits | @octits | @hexits
@longinteger = @integer
@number      = @integer | @longinteger

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
         | SUB | ESC | FS | GS | RS | US | SP | DEL
$nl        = [\n\r]

$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @number | \.)
@string = $graphic # [\"\\] | " " | @escape

@varid  = $alpha $idchar*
@clj_earmuffed = "*" @varid "*"
@clj_keyword = ":" @varid

@comment = ";".*$nl

clojure :-

<0> {
    @comment                 { c commentStyle }
    "("                      { m' (+ 1) (parenStyle . succ) }
    $white+                  { c defaultStyle }
    .                        { c defaultStyle }
}

<sexpr> {
    $white+                  { c defaultStyle }
    @comment                 { c commentStyle }
    \" @string* \"           { c stringStyle }
    \#\" @string* \"         { c regexStyle }
    "("                      { m' (+ 1)        (parenStyle . succ) }
    ")"                      { m' (subtract 1) parenStyle }
    @clj_cond                { c numberStyle }
    @clj_special             { c keywordStyle }
    @clj_bool                { c dataConstructorStyle }
    @clj_nil                 { c dataConstructorStyle }
    @clj_destructuring       { c importStyle }
    @clj_keyword             { c typeStyle }
    @clj_earmuffed           { c importStyle }
    @clj_def                 { c keywordStyle }
    @clj_macros              { c operatorStyle }
    @varid                   { c defaultStyle }
    @number
      | @number \. @number?  { c numberStyle }
    .                        { c defaultStyle }
}

{

-- parenStyle :: HlState -> Style
parenStyle level = cycle styles !! level
    where styles = [builtinStyle, numberStyle, dataConstructorStyle, operatorStyle, keywordStyle, stringStyle]

type HlState = Int
type Token = StyleName

stateToInit x | x > 0     = sexpr
              | otherwise = 0

initState :: HlState
initState = 0

#include "common.hsinc"
}
