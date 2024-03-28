{
{-# OPTIONS -Werror=missing-fields #-}

module Parser( parseProgram ) where

-- *******************
-- *                 *
-- * project imports *
-- *                 *
-- *******************
import Ast
import Lexer
import Location
import qualified Token

-- *******************
-- *                 *
-- * general imports *
-- *                 *
-- *******************
import Data.Maybe
import Data.Either
import Data.List ( map )
import Data.Map ( fromList )

}

-- ***********************
-- *                     *
-- * API function: parse *
-- *                     *
-- ***********************
%name parse

-- *************
-- * tokentype *
-- *************
%tokentype { AlexTokenTag }

-- *********
-- * monad *
-- *********
%monad { Alex }

-- *********
-- * lexer *
-- *********
%lexer { lexwrap } { AlexTokenTag TokenEOF _ }

-- ***************************************************
-- * Call this function when an error is encountered *
-- ***************************************************
%error { parseError }

%token 

-- ***************
-- *             *
-- * parentheses *

-- ***************

'('    { AlexTokenTag AlexRawToken_LPAREN _ }
')'    { AlexTokenTag AlexRawToken_RPAREN _ }
'['    { AlexTokenTag AlexRawToken_LBRACK _ }
']'    { AlexTokenTag AlexRawToken_RBRACK _ }
'{'    { AlexTokenTag AlexRawToken_LBRACE _ }
'}'    { AlexTokenTag AlexRawToken_RBRACE _ }

-- ***************
-- *             *
-- * punctuation *
-- *             *
-- ***************

':'    { AlexTokenTag AlexRawToken_COLON  _ }
','    { AlexTokenTag AlexRawToken_COMMA  _ }
'-'    { AlexTokenTag AlexRawToken_HYPHEN _ }

-- *********************
-- *                   *
-- * reserved keywords *
-- *                   *
-- *********************

'id'                    { AlexTokenTag AlexRawToken_KWID            _ }
'end'                   { AlexTokenTag AlexRawToken_END             _ }
'raw'                   { AlexTokenTag AlexRawToken_RAW             _ }
'loc'                   { AlexTokenTag AlexRawToken_LOC             _ }
'Arg'                   { AlexTokenTag AlexRawToken_ARG             _ }
'var'                   { AlexTokenTag AlexRawToken_VAR             _ }
'null'                  { AlexTokenTag AlexRawToken_NULL            _ }
'test'                  { AlexTokenTag AlexRawToken_TEST            _ }
'line'                  { AlexTokenTag AlexRawToken_LINE            _ }
'true'                  { AlexTokenTag AlexRawToken_TRUE            _ }
'args'                  { AlexTokenTag AlexRawToken_ARGS            _ }
'name'                  { AlexTokenTag AlexRawToken_NAME            _ }
'expr'                  { AlexTokenTag AlexRawToken_EXPR            _ }
'Name'                  { AlexTokenTag AlexRawToken_MAME            _ }
'type'                  { AlexTokenTag AlexRawToken_TYPE            _ }
'left'                  { AlexTokenTag AlexRawToken_LEFT            _ }
'loop'                  { AlexTokenTag AlexRawToken_LOOP            _ }
'init'                  { AlexTokenTag AlexRawToken_INIT            _ }
'cond'                  { AlexTokenTag AlexRawToken_COND            _ }
'body'                  { AlexTokenTag AlexRawToken_BODY            _ }
'update'                { AlexTokenTag AlexRawToken_UPDATE          _ }
'false'                 { AlexTokenTag AlexRawToken_FALSE           _ }
'start'                 { AlexTokenTag AlexRawToken_START           _ }
'exprs'                 { AlexTokenTag AlexRawToken_EXPRS           _ }
'value'                 { AlexTokenTag AlexRawToken_VALUE           _ }
'right'                 { AlexTokenTag AlexRawToken_RIGHT           _ }
'stmts'                 { AlexTokenTag AlexRawToken_STMTS           _ }
'array'                 { AlexTokenTag AlexRawToken_ARRAY           _ }
'Param'                 { AlexTokenTag AlexRawToken_PARAM           _ }
'object'                { AlexTokenTag AlexRawToken_OBJECT          _ }
'prefix'                { AlexTokenTag AlexRawToken_PREFIX          _ }
'params'                { AlexTokenTag AlexRawToken_PARAMS          _ }
'column'                { AlexTokenTag AlexRawToken_COLUMN          _ }
'Literal'               { AlexTokenTag AlexRawToken_LITERAL         _ }
'Program'               { AlexTokenTag AlexRawToken_PROGRAM         _ }
'property'              { AlexTokenTag AlexRawToken_PROPERTY        _ }
'computed'              { AlexTokenTag AlexRawToken_COMPUTED        _ }
'operator'              { AlexTokenTag AlexRawToken_OPERATOR        _ }
'alternate'             { AlexTokenTag AlexRawToken_ALTERNATE       _ }
'consequent'            { AlexTokenTag AlexRawToken_CONSEQUENT      _ }
'argument'              { AlexTokenTag AlexRawToken_ARGUMENT        _ }
'arguments'             { AlexTokenTag AlexRawToken_ARGUMENTS       _ }
'generator'             { AlexTokenTag AlexRawToken_GENERATOR       _ }
'expression'            { AlexTokenTag AlexRawToken_EXPRESSION      _ }
'async'                 { AlexTokenTag AlexRawToken_ASYNC           _ }
'callee'                { AlexTokenTag AlexRawToken_CALLEE          _ }
'sourceType'            { AlexTokenTag AlexRawToken_SRC_TYPE        _ }
'Stmt_Echo'             { AlexTokenTag AlexRawToken_STMT_ECHO       _ }
'Expr_Variable'         { AlexTokenTag AlexRawToken_EXPR_VAR        _ }
'Stmt_Expr'             { AlexTokenTag AlexRawToken_STMT_EXPR       _ }
'Scalar_Int'            { AlexTokenTag AlexRawToken_SCALAR_INT      _ }
'Identifier'            { AlexTokenTag AlexRawToken_IDENTIFIER      _ }
'returnType'            { AlexTokenTag AlexRawToken_RETURN_TYPE     _ }
'Stmt_Function'         { AlexTokenTag AlexRawToken_STMT_FUNCTION   _ }
'FunctionDeclaration'   { AlexTokenTag AlexRawToken_FUNCTION_DEC    _ }
'Expr_ConstFetch'       { AlexTokenTag AlexRawToken_EXPR_CONST_GET  _ }
'Expr_BinaryOp_Plus'    { AlexTokenTag AlexRawToken_EXPR_BINOP_PLUS _ }
'Expr_BinaryOp_Smaller' { AlexTokenTag AlexRawToken_EXPR_BINOP_LT   _ }

-- *********
-- *       *
-- * other *
-- *       *
-- *********

QUOTED_INT  { AlexTokenTag AlexRawToken_QUOTED_INT  _ }
QUOTED_BOOL { AlexTokenTag AlexRawToken_QUOTED_BOOL _ }

-- ***************
-- *             *
-- * expressions *
-- *             *
-- ***************

'CallExpression'   { AlexTokenTag AlexRawToken_EXPR_CALL   _ }
'MemberExpression' { AlexTokenTag AlexRawToken_EXPR_MEMBER _ }
'BinaryExpression' { AlexTokenTag AlexRawToken_EXPR_BINOP  _ }
'UpdateExpression' { AlexTokenTag AlexRawToken_EXPR_UPDATE _ }
'AssignExpression' { AlexTokenTag AlexRawToken_EXPR_ASSIGN _ }

-- **************
-- *            *
-- * statements *
-- *            *
-- **************

'IfStatement'         { AlexTokenTag AlexRawToken_STMT_IF     _ }
'ForStatement'        { AlexTokenTag AlexRawToken_STMT_FOR    _ }
'BlockStatement'      { AlexTokenTag AlexRawToken_STMT_BLOCK  _ }
'ReturnStatement'     { AlexTokenTag AlexRawToken_STMT_RETURN _ }
'ExpressionStatement' { AlexTokenTag AlexRawToken_STMT_EXP    _ }

-- *************
-- *           *
-- * operators *
-- *           *
-- *************

'<'  { AlexTokenTag AlexRawToken_OP_LT       _ }
'==' { AlexTokenTag AlexRawToken_OP_EQ       _ }
'='  { AlexTokenTag AlexRawToken_OP_ASSIGN   _ }
'*'  { AlexTokenTag AlexRawToken_OP_TIMES    _ }
'++' { AlexTokenTag AlexRawToken_OP_PLUSPLUS _ }

-- ****************************
-- *                          *
-- * integers and identifiers *
-- *                          *
-- ****************************

INT    { AlexTokenTag (AlexRawToken_INT  i) _ }
ID     { AlexTokenTag (AlexRawToken_ID  id) _ }

-- *************************
-- *                       *
-- * grammar specification *
-- *                       *
-- *************************
%%

-- **********************
-- *                    *
-- * parametrized lists *
-- *                    *
-- **********************
listof(a):      a { [$1] } | a          listof(a) { $1:$2 }
commalistof(a): a { [$1] } | a ',' commalistof(a) { $1:$3 }

-- *********************
-- *                   *
-- * Ast root: program *
-- *                   *
-- *********************
program:
'{'
    'type' ':' 'Program' ','
    'loc'  ':' location ','
    'stmts' ':' decs ','
'}'
{
    Ast.Root
    {
        Ast.filename = "DDD",
        actualAst = []
    }
}

-- ********
-- *      *
-- * decs *
-- *      *
-- ********
decs:
'{'
    'type' ':' 'stmts' ','
    'loc' ':' location ','
    'body' ':' '[' commalistof(dec) ']'
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * dec or stmt *
-- *             *
-- ***************
dec_or_stmt:
dec  { Left  $1 } |
stmt { Right $1 }

-- *******
-- *     *
-- * dec *
-- *     *
-- *******
dec:
dec_function { $1 }

-- ************
-- *          *
-- * location *
-- *          *
-- ************
location: '[' INT ',' INT ',' INT ',' INT ']'
{
    Location
    {
        Location.filename = "DDD",
        lineStart = tokIntValue $2,
        colStart = tokIntValue $4,
        lineEnd = tokIntValue $6,
        colEnd = tokIntValue $8
    }
}

-- ************
-- *          *
-- * token ID *
-- *          *
-- ************
tokenID:
ID      { Nothing } |
'end'   { Nothing } |
'start' { Nothing }

-- **************
-- *            *
-- * identifier *
-- *            *
-- **************
identifier:
'{'
    'type' ':' 'Identifier' ','
    'name' ':' tokenID ','
    'loc' ':' location
'}'
{
    Nothing
}

-- *********
-- *       *
-- * param *
-- *       *
-- *********
param:
'{'
    'type' ':' 'Identifier' ','
    'name' ':' ID ','
    'loc' ':' location
'}'
{
    Token.Named
    {
        Token.content = tokIDValue $8,
        Token.location = $12
    } 
}

-- ***********
-- *         *
-- * exp_var *
-- *         *
-- ***********
exp_var: var { $1 }

-- *************
-- *           *
-- * exp_binop *
-- *           *
-- *************
exp_binop:
'{'
    'type' ':' 'BinaryExpression' ','
    'operator' ':' operator ','
    'left' ':' exp ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ******************
-- *                *
-- * field variable *
-- *                *
-- ******************
var_field:
'{'
    'type' ':' 'MemberExpression' ','
    'computed' ':' bool ','
    'object' ':' exp_var ','
    'property' ':' identifier ','
    'loc' ':' location
'}'
{
    Nothing
}

-- *******************
-- *                 *
-- * simple variable *
-- *                 *
-- *******************
var_simple: identifier { $1 }

-- ************
-- *          *
-- * variable *
-- *          *
-- ************
var:
var_simple { $1 } |
var_field  { $1 }

-- **************
-- *            *
-- * exp_assign *
-- *            *
-- **************
exp_assign:
'{'
    'type' ':' 'AssignExpression' ','
    'operator' ':' operator ','
    'left' ':' var ','
    'right' ':' exp ','
    'loc' ':' location
'}'
{
    Nothing
}

-- **************
-- *            *
-- * exp_assign *
-- *            *
-- **************
exp_assign_tag:
'{'
    'type' ':' 'ExpressionStatement' ','
    'expression' ':' exp_assign ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ***********
-- *         *
-- * exp_int *
-- *         *
-- ***********
exp_int:
'{'
    'type' ':' 'Literal' ','
    'value' ':' INT ','
    'raw' ':' QUOTED_INT ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ************
-- *          *
-- * exp_bool *
-- *          *
-- ************
exp_bool:
'{'
    'type' ':' 'Literal' ','
    'value' ':' bool ','
    'raw' ':' QUOTED_BOOL ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ************
-- *          *
-- * exp_null *
-- *          *
-- ************
exp_null: 'null' { Nothing }

-- ************
-- *          *
-- * exp_call *
-- *          *
-- ************
exp_call:
'{'
    'type' ':' 'CallExpression' ','
    'callee' ':' exp ','
    'arguments' ':' '[' commalistof(exp) ']' ','
    'loc' ':' location
'}'
{
    Nothing
}

-- *******
-- *     *
-- * exp *
-- *     *
-- *******
exp:
exp_int    { $1 } |
exp_var    { $1 } |
exp_bool   { $1 } |
exp_call   { $1 } |
exp_binop  { $1 } |
exp_assign { $1 }

-- ************
-- *          *
-- * stmt_for *
-- *          *
-- ************
stmt_for:
'{'
    'type' ':' 'ForStatement' ','
    'init' ':' exp ','
    'test' ':' exp ','
    'update' ':' stmt ','
    'body' ':' stmts ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ************
-- *          *
-- * operator *
-- *          *
-- ************
operator:
'++' { Nothing } |
'==' { Nothing } |
'*'  { Nothing } |
'<'  { Nothing } |
'='  { Nothing }

-- ********
-- *      *
-- * bool *
-- *      *
-- ********
bool:
'true'  { True  } |
'false' { False }

-- ***************
-- *             *
-- * stmt_return *
-- *             *
-- ***************
stmt_return:
'{'
    'type' ':' 'ReturnStatement' ','
    'argument' ':' exp ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_update *
-- *             *
-- ***************
stmt_update:
'{'
    'type' ':' 'UpdateExpression' ','
    'operator' ':' operator ','
    'argument' ':' identifier ','
    'prefix' ':' bool ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ***************
-- *             *
-- * stmt_assign *
-- *             *
-- ***************
stmt_assign:
stmt_update    { $1 } |
exp_assign     { $1 } |
exp_assign_tag { $1 }

-- ***********
-- *         *
-- * stmt_if *
-- *         *
-- ***********
stmt_if:
'{'
    'type' ':' 'IfStatement' ','
    'test' ':' exp ','
    'consequent' ':' stmts ','
    'alternate' ':' stmts ','
    'loc' ':' location
'}'
{
    Nothing
}

-- *************
-- *           *
-- * stmt_call *
-- *           *
-- *************
stmt_call:
'{'
    'type' ':' 'ExpressionStatement' ','
    'expression' ':' exp_call ','
    'loc' ':' location
'}'
{
    Nothing
}

-- ********
-- *      *
-- * stmt *
-- *      *
-- ********
stmt:
stmt_if     { $1 } |
stmt_for    { $1 } |
stmt_call   { $1 } |
stmt_assign { $1 } |
stmt_return { $1 }

-- *********
-- *       *
-- * stmts *
-- *       *
-- *********
stmts:
'{'
    'type' ':' 'stmts' ','
    'loc' ':' location
'}'
{
    []
} |
'null' { [] }

-- ****************
-- *              *
-- * dec_function *
-- *              *
-- ****************
dec_function: '{'
    'type' ':' 'FunctionDeclaration' ','
    'loc' ':' location ','
    'body' ':' '[' commalistof(dec) ']' ','
    'generator' ':' bool ','
    'params' ':' '[' commalistof(param) ']' ','
    'id' ':' identifier ','
    'expression' ':' bool ','
    'async' ':' bool ','
'}'
{
    Left "MMM"
}

{

extractParamSingleName' :: [ Token.ParamName ] -> Maybe Token.ParamName
extractParamSingleName' ps = case ps of { [p] -> Just p; _ -> Nothing }
 
extractParamSingleName :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.ParamName
extractParamSingleName = extractParamSingleName' . lefts  

extractParamNominalType' :: [ Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType' ts = case ts of { [t] -> Just t; _ -> Nothing }
 
extractParamNominalType :: [ Either Token.ParamName Token.NominalTy ] -> Maybe Token.NominalTy
extractParamNominalType = extractParamNominalType' . rights 

paramify :: [ Either Token.ParamName Token.NominalTy ] -> Location -> Maybe Ast.Param
paramify attrs l = let
    name = extractParamSingleName attrs
    nominalType = extractParamNominalType attrs
    in case (name, nominalType) of { (Just n, Just t) -> Just $ Ast.Param n t 0; _ -> Nothing }

getFuncNameAttr :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.FuncName
getFuncNameAttr = undefined

getFuncReturnType :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe Token.NominalTy
getFuncReturnType = undefined

getFuncBody :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Stmt ]
getFuncBody = undefined

getFuncParams :: [ Either (Either Token.FuncName [ Ast.Param ] ) (Either Token.NominalTy [ Ast.Stmt ] ) ] -> Maybe [ Ast.Param ]
getFuncParams = undefined

-- getFilename :: (Either Location Ast.Dec) -> FilePath
-- getFilename x = case x of { Left l -> Location.filename l; Right dec -> Location.filename $ Ast.locationDec dec }

-- add the /real/ serial index of the param
-- the parser just puts an arbitrary value
-- there because it lacks context
enumerateParams :: (Word,[Param]) -> [Param]
enumerateParams (_,[    ]) = []
enumerateParams (i,(p:ps)) =
    let
        n = (paramName        p)
        t = (paramNominalType p)
        head = Param { paramName = n, paramNominalType = t, paramSerialIdx = i }
        tail = (enumerateParams (i+1,ps))
    in
        head:tail

-- ***********
-- *         *
-- * lexwrap *
-- *         *
-- ***********
lexwrap :: (AlexTokenTag -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

-- **************
-- *            *
-- * parseError *
-- *            *
-- **************
parseError :: AlexTokenTag -> Alex a
parseError t = alexError' (tokenLoc t)

-- ****************
-- *              *
-- * parseProgram *
-- *              *
-- ****************
parseProgram :: FilePath -> String -> Either String Ast.Root
parseProgram = runAlex' parse
}
