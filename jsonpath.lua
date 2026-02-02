--[[

    This file is part of lua-jsonpath.

    Copyright (c) 2016 Frank Edelhaeuser

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


    Lua JsonPath
    ============

    Lua implementation of Stefan Goessner's "JSONPath - XPath for JSON"

    Query Lua objects with JsonPath expressions. Robust and safe JsonPath engine.


    Website:

        https://github.com/tarantool/lua-jsonpath

    See:

      - Original JsonPath specification and Javascript + PHP implementation
        http://goessner.net/articles/JsonPath/

      - David Chester's Javascript implementation and extended specification
        https://github.com/dchester/jsonpath

    This implementation of JsonPath supports the API and extensions defined
    in David Chester's Javascript implementation. JsonPath is tested against
    David's test suite.


    Installation:

        tarantoolctl rocks install jsonpath

    Dependencies:

        - lulpeg

    Usage:

        local jp = require('jsonpath')

        local data = {
            store = {
                book = {
                    {
                        category = 'reference',
                        author = 'Nigel Rees',
                        title = 'Sayings of the Century',
                        price = 8.95
                    }, {
                        category = 'fiction',
                        author = 'Evelyn Waugh',
                        title = 'Sword of Honour',
                        price = 12.99
                    }, {
                        category = 'fiction',
                        author = 'Herman Melville',
                        title = 'Moby Dick',
                        isbn = '0-553-21311-3',
                        price = 8.99
                    }, {
                        category = 'fiction',
                        author = 'J. R. R. Tolkien',
                        title = 'The Lord of the Rings',
                        isbn = '0-395-19395-8',
                        price = 22.99
                    }
                },
                bicycle = {
                    color = 'red',
                    price = 19.95
                }
            }
        }

        local match = jp.query(data, '$..author')
        -- { 'Nigel Rees', 'Evelyn Waugh', 'Herman Melville', 'J. R. R. Tolkien' }

]]--
local M = {}

local codes = {
    BAD_REQUEST = 400,
    NOT_FOUND = 404,
    INTERNAL_ERR = 500,
}

local errors = require('errors')

local JsonPathError = errors.new_class("JsonPathError")
local JsonPathNotFoundError = errors.new_class("JsonPathNotFoundError")

local ffi = require('ffi')

-- Use Roberto Ierusalimschy's fabulous LulPeg pattern-matching library
local lulpeg = require('lulpeg')
local S, R, P, V = lulpeg.S, lulpeg.R, lulpeg.P, lulpeg.V
local C, Cc, Cg, Ct, Cp = lulpeg.C, lulpeg.Cc, lulpeg.Cg, lulpeg.Ct, lulpeg.Cp -- luacheck: ignore


-- Return values for match_path()
local MISMATCH = 0
local MATCH_ONE = 1
local MATCH_DESCENDANTS = 2
local MATCH_PARTIAL = 3

local NULL = box.NULL or ffi.cast("void*", ffi.new('uintptr_t', 0))
M.NULL = NULL

local function is_nil(val)
    return not val and val == nil
end

local function is_null(val)
    return val and val == NULL or false
end

-- Generate JsonPath grammer
local jsonpath_grammer = (function()

    local function anycase(x)
        local y = P ''
        for i = 1, #x do
            local c = x:sub(i, i)
            y = y * S(c:lower() .. c:upper())
        end
        return y
    end

    local function toboolean(s)
        return s:lower() == 'true' and true or false
    end

    local function tonull(_)
        return NULL
    end

    local function reduce_expr(expr)
        for i = 2, #expr, 2 do
            if type(expr[i]) == 'table' then
                expr[i] = reduce_expr(expr[i])
            end
        end
        if #expr == 2 and expr[1] == 'expr' and type(expr[2]) == 'table' then
            expr = expr[2]
        end
        return expr
    end

    local space = S ' \t\n'
    local S_0 = space ^ 0

    local lparen = '(' * S_0
    local rparen = ')' * S_0
    local comma = ',' * S_0
    local colon = ':' * S_0
    local quote = P '"'
    local apos = P "'"
    local root = P '$'
    local star = P '*'
    local dot = P '.'
    local at = P '@'

    local sign = S '+-'
    local digit = R('09')
    local letter = R('AZ', 'az') -- luacheck: ignore
    local alpha0 = R('AZ', 'az') + '_'
    local alphaN = R('AZ', 'az', '09') + '_'
    local hexdigit = R('09', 'AF', 'af')

    local number_hex = P '0' * S 'xX' * hexdigit ^ 1
    local number_rat = sign ^ -1 *
            (digit ^ 1 * P '.' * digit ^ 0 + P '.' ^ -1 * digit ^ 1) * (S 'eE' * sign ^ -1 * digit ^ 1) ^ -1

    local string_q = apos * C((P '\\' * P(1) + (1 - S "\\'")) ^ 0) / function(s)
        local u = s:gsub("(\\')", "'");
        return u;
    end * apos
    local string_qq = quote * C((P '\\' * P(1) + (1 - S '\\"')) ^ 0) / function(s)
        local u = s:gsub('(\\")', '"');
        return u;
    end * quote

    local string_literal = (string_q + string_qq) * S_0
    local number_literal = C(number_hex + number_rat) / tonumber * S_0
    local boolean_literal = C(anycase('true') + anycase('false')) / toboolean * S_0
    local null_literal = C(anycase('null')) / tonull * S_0

    local literal = string_literal + number_literal + boolean_literal + null_literal
    local index_literal = string_literal + number_literal

    local name = C(alpha0 * alphaN ^ 0) * S_0
    local var_length = at * dot * P 'length' * S_0

    local child_index = C(sign ^ -1 * digit ^ 1) * S_0


    -- operators (6 = highest precedence), subset of https://sqlite.org/lang_expr.html
    local op_prec_6 = C(S '*/%') * S_0
    local op_prec_5 = C(S '+-') * S_0
    --- JsonPath specification equals precedence of all comparison operations
    local comparison_op = C(P '==' + P '=' + P '!=' + P '<>' + P '<=' + P '>=' + S '<>') * S_0
    local op_prec_2 = C(anycase('AND') + '&&') * S_0
    local op_prec_1 = C(anycase('OR') + '||') * S_0

    local jsonpath = S_0 * P {
        'JSONPATH',

        JSONPATH = Ct(V 'CHILD_1' * V 'CHILD_N' ^ 0),

        CHILD_1 = C(root) * (C('..') + '.') * V 'CHILD_NAME' +
                C(root) * V 'CHILD_SUBSCRIPT' +
                C(root) +
                Cc('$') * (V 'CHILD_SUBSCRIPT' + V 'CHILD_NAME'),

        CHILD_N = (C('..') + '.') * V 'CHILD_NAME' +
                C('..') * V 'CHILD_SUBSCRIPT' +
                V 'CHILD_SUBSCRIPT',

        CHILD_NAME = child_index +
                name +
                C(star),

        CHILD_SUBSCRIPT = '[' * Ct(V 'CHILD_SUBSCRIPT_EXPR') * ']',

        CHILD_SUBSCRIPT_EXPR = V 'WILDCARD_SELECTOR' + -- *
                V 'CHILD_UNION' + -- n1[,n2[,...]] or (script)
                V 'SLICE_SELECTOR' + -- [start]:[end][:step]
                V 'FILTER_SELECTOR', -- ?(x > 10)

        CHILD_UNION = Cc('union') * V 'CHILD_UNION_EXPR' * (comma * V 'CHILD_UNION_EXPR') ^ 0 +
                Cc('union') * '(' * S_0 * Ct(Cc('expr') * V 'LOGICAL_EXPR') * ')',

        SLICE_SELECTOR = Cc('slice') * (child_index + Cc '0') * colon
                * (child_index + Cc 'nil') * ((colon * child_index) + Cc '1'),

        CHILD_UNION_EXPR = Ct(V 'SLICE_SELECTOR') +
                string_literal +
                child_index,

        SELECTOR = V 'WILDCARD_SELECTOR' + V 'SLICE_SELECTOR' + V 'FILTER_SELECTOR' + V 'INDEX_SELECTOR',

        INDEX_SELECTOR = Ct(Cc('expr') * number_literal),
        FILTER_SELECTOR = Cc('filter') * '?' * S_0 * Ct(Cc('expr') * V 'LOGICAL_EXPR'),
        WILDCARD_SELECTOR = Cc('*') * '*',

        LOGICAL_EXPR = Ct(Cc('expr') * V 'LOGICAL_OR_EXPR') / reduce_expr * S_0 ,
        LOGICAL_OR_EXPR = Ct(Cc('expr') * V 'LOGICAL_AND_EXPR' * (op_prec_1 * V 'LOGICAL_AND_EXPR') ^ 0) * S_0,
        LOGICAL_AND_EXPR = Ct(Cc('expr') * V 'COMPARISON_EXPR' * (op_prec_2 * V 'COMPARISON_EXPR') ^ 0) * S_0,
        COMPARISON_EXPR = Ct(Cc('expr') * V 'SUM_EXPR' * (comparison_op * V 'SUM_EXPR') ^ 0) * S_0,
        SUM_EXPR = Ct(Cc('expr') * V 'MULT_EXPR' * (op_prec_5 * V 'MULT_EXPR') ^ 0) * S_0,
        MULT_EXPR = Ct(Cc('expr') * V 'BASIC_EXPR' * (op_prec_6 * V 'BASIC_EXPR') ^ 0 ) * S_0,

        BASIC_EXPR = V 'LITERAL_EXPR' + V 'PAREN_EXPR' + V 'SINGULAR_QUERY',

        LITERAL_EXPR = Ct(Cc('expr') * literal),
        PAREN_EXPR = lparen * V 'LOGICAL_EXPR' * rparen,
        SINGULAR_QUERY = V 'REL_SINGULAR_QUERY' * S_0,
        REL_SINGULAR_QUERY = Ct(Cc('var.length') * var_length) + Ct(Cc('var') * at * V 'SINGULAR_QUERY_SEGMENTS') ,
        SINGULAR_QUERY_SEGMENTS = (V 'NAME_SEGMENT' + V 'INDEX_SEGMENT') ^ 0,
        NAME_SEGMENT = dot * V 'CHILD_NAME',
        INDEX_SEGMENT = '[' * S_0 * index_literal * S_0 * ']',

        FILTER_QUERY = V 'REL_QUERY',
        REL_QUERY = at * V 'SEGMENTS',

        SEGMENTS = (S_0 * V 'SEGMENT') ^ 0,
        SEGMENT = V 'CHILD_SEGMENT' + V 'DESCENDANT_SEGMENT',
        CHILD_SEGMENT = V 'BRACKETED_SELECTION' + (dot * (V 'WILDCARD_SELECTOR' + V 'MEMBER_NAME')),
        DESCENDANT_SEGMENT = Cc('..') * '..' + (V 'BRACKETED_SELECTION' + V 'WILDCARD_SELECTOR' + V 'MEMBER_NAME'),

        BRACKETED_SELECTION = '[' * S_0 * V 'SELECTOR' * ( S_0 * ',' * S_0 * V 'SELECTOR') ^ 0 * S_0 * ']',
        MEMBER_NAME = Ct(Cc('var') * V 'CHILD_NAME'),
    } * S_0

    return jsonpath
end)()

local function bad_request_error(err)
    local err = JsonPathError:new(err)
    err.rc = codes.BAD_REQUEST
    return err
end

local function not_found_error(err)
    local err = JsonPathNotFoundError:new(err)
    err.rc = codes.NOT_FOUND
    return err
end

-- Helper: evaluate abstract syntax tree. Called recursively.
local function eval_ast(ast, obj)

    -- Helper helper: match type of second operand to type of first operand
    local function match_cmp_type(op1, op2, compare)
        if type(op1) == 'boolean' then
            if is_null(op2) then
                return not op1, nil
            else
                if type(op2) == 'string' then
                    return nil, bad_request_error("cannot compare boolean with string")
                end
                if type(op2) == 'number' then
                    if compare then
                        return nil, bad_request_error("cannot compare boolean with number")
                    end
                    return op2 ~= 0, nil
                end
                if type(op2) == 'boolean' then
                    if compare then
                        return nil,  bad_request_error("cannot compare boolean with boolean")
                    end
                    return op2, nil
                end
                return (op2 and true or false), nil
            end
        elseif type(op1) == 'number' then
            if type(op2) == 'boolean' then
                return op2 and 1 or 0, nil
            end
            if type(op2) == 'string' then
                local num = tonumber(op2)
                if num == nil then
                    return nil, bad_request_error("cannot compare number with non-numeric string")
                end
                return num, nil
            end
            return tonumber(op2), nil
        elseif type(op1) == 'cdata' and tostring(ffi.typeof(op1)) == 'ctype<int64_t>' then
            return tonumber(op2), nil
        elseif is_null(op1) then
            if compare then
                return nil, bad_request_error("cannot compare null with other values")
            end
            return op2, nil
        end
        return tostring(op2 or ''), nil
    end

    -- Helper helper: convert operand to boolean
    local function notempty(op1)
        return op1 and true or false
    end

    local function is_str_or_int(val)
        return type(val) == 'string' or
        type(val) == 'number' or
        (type(val) == 'cdata' and tostring(ffi.typeof(val)) == 'ctype<int64_t>')
    end

    -- Helper helper: evaluate variable expression inside abstract syntax tree
    local function eval_var(expr, obj)
        if obj == nil then
            return nil, bad_request_error('object is not set')
        end
        if type(obj) ~= "table" then
            return nil, not_found_error('object is primitive')
        end
        for i = 2, #expr do
            -- [1] is "var"
            local member, err = eval_ast(expr[i], obj)
            if member == nil then
                return nil, err
            end
            member = type(member) == 'number' and member + 1 or member
            obj = obj[member]
            if is_nil(obj) then
                return nil, not_found_error('object doesn\'t contain an object or attribute "'.. member ..'"')
            end
        end
        return obj
    end
    -- Helper helper: calculate number of members in object
    local function eval_var_length(obj)
        local count = 0
        for i, _ in pairs(obj) do
            count = count + 1
        end
        return count
    end
    -- Helper helper: evaluate 'union' expression inside abstract syntax tree
    local function eval_union(expr, obj)
        local matches = {}  -- [1] is "union"
        for i = 2, #expr do
            local result, err = eval_ast(expr[i], obj)
            if err then
                return nil, err
            end
            if type(result) == 'table' then
                for _, j in ipairs(result) do
                    table.insert(matches, j)
                end
            else
                table.insert(matches, result)
            end
        end
        return matches
    end

    -- Helper helper: evaluate 'filter' expression inside abstract syntax tree
    local function eval_filter(expr, obj)
        local result, err = eval_ast(expr[2], obj)
        if err then
            if err.rc == codes.NOT_FOUND then
                return false
            end
        return nil, err
    end
        return result and true or false
    end

    -- Helper helper: evaluate 'slice' expression inside abstract syntax tree
    local function eval_slice(expr, obj)
        local matches = {}  -- [1] is "slice"
        if #expr == 4 then
            local from_result, err = eval_ast(expr[2], obj)
            if err then return nil, err end
            local to_result, err = eval_ast(expr[3], obj)
            if err then return nil, err end
            local step_result, err = eval_ast(expr[4], obj)
            if err then return nil, err end

            local from = tonumber(from_result)
            local to = tonumber(to_result)
            local step = tonumber(step_result)

            if (from == nil) or (from < 0) or (to == nil) or (to < 0) then
                local len = eval_var_length(obj)
                if from == nil then
                    from = 0
                elseif from < 0 then
                    from = len + from
                end
                if to == nil then
                    to = len + 1
                elseif to < 0 then
                    to = len + to
                end
            end
            for i = from, to - 1, step do
                table.insert(matches, i)
            end
        end
        return matches
    end

    -- Helper helper: evaluate expression inside abstract syntax tree
    local function eval_expr(expr, obj)
        local op1, err = eval_ast(expr[2], obj) -- [1] is "expr"
        if is_nil(op1) then
            return nil, err
        end
        for i = 3, #expr, 2 do
            local operator = expr[i]
            if operator == nil then
                local err = bad_request_error('missing expression operator')
                return nil, err
            end
            local op2, err = eval_ast(expr[i + 1], obj)
            if is_nil(op2) then
                return nil, err
            end
            if operator == '+' then
                if is_str_or_int(op1) and is_str_or_int(op2) then
                    local num1, num2 = tonumber(op1), tonumber(op2)
                    if not (num1 and num2) then
                        return nil, "Cannot perform arithmetic on non-numeric strings"
                    end
                    op1 = num1 + num2
                else
                    return nil, bad_request_error("Only operations on strings and numbers are allowed.")
                end
            elseif operator == '-' then
                if is_str_or_int(op1) and is_str_or_int(op2) then
                    local num1, num2 = tonumber(op1), tonumber(op2)
                    if not (num1 and num2) then
                        return nil, "Cannot perform arithmetic on non-numeric strings"
                    end
                    op1 = num1 - num2
                else
                    return nil, bad_request_error("Only operations on strings and numbers are allowed.")
                end
            elseif operator == '*' then
                if is_str_or_int(op1) and is_str_or_int(op2) then
                    local num1, num2 = tonumber(op1), tonumber(op2)
                    if not (num1 and num2) then
                        return nil, "Cannot perform arithmetic on non-numeric strings"
                    end
                    op1 = num1 * num2
                else
                    return nil, bad_request_error("Only operations on strings and numbers are allowed.")
                end
            elseif operator == '/' then
                if is_str_or_int(op1) and is_str_or_int(op2) then
                    local num1, num2 = tonumber(op1), tonumber(op2)
                    if not (num1 and num2) then
                        return nil, "Cannot perform arithmetic on non-numeric strings"
                    end
                    op1 = num1 / num2
                else
                    return nil, bad_request_error("Only operations on strings and numbers are allowed.")
                end
            elseif operator == '%' then
                if is_str_or_int(op1) and is_str_or_int(op2) then
                    local num1, num2 = tonumber(op1), tonumber(op2)
                    if not (num1 and num2) then
                        return nil, "Cannot perform arithmetic on non-numeric strings"
                    end
                    op1 = num1 % num2
                else
                    return nil, bad_request_error("Only operations on strings and numbers are allowed.")
                end
            elseif operator:upper() == 'AND' or operator == '&&' then
                op1 = notempty(op1) and notempty(op2)
            elseif operator:upper() == 'OR' or operator == '||' then
                op1 = notempty(op1) or notempty(op2)
            elseif operator == '=' or operator == '==' then
                local op2, err = match_cmp_type(op1, op2, false)
                if err then
                    return nil, err
                end
                op1 = op1 == op2
            elseif operator == '<>' or operator == '!=' then
                local op2, err = match_cmp_type(op1, op2, false)
                if err then
                    return nil, err
                end
                op1 = op1 ~= op2
            elseif operator == '>' then
                local op2, err = match_cmp_type(op1, op2, true)
                if err then
                    return nil, err
                end
                op1 = op1 > op2
            elseif operator == '>=' then
                local op2, err = match_cmp_type(op1, op2, true)
                if err then
                    return nil, err
                end
                op1 = op1 >= op2
            elseif operator == '<' then
                local op2, err = match_cmp_type(op1, op2, true)
                if err then
                    return nil, err
                end
                op1 = op1 < op2
            elseif operator == '<=' then
                local op2, err = match_cmp_type(op1, op2, true)
                if err then
                    return nil, err
                end
                op1 = op1 <= op2
            else
                return nil, bad_request_error('unknown expression operator "' .. operator .. '"')
            end
        end
        return op1
    end

    -- Evaluate abstract syntax tree
    if type(ast) == 'number' or type(ast) == 'string' or type(ast) == 'boolean' or is_null(ast) then
        return ast
    elseif ast[1] == 'expr' then
        return eval_expr(ast, obj)
    elseif ast[1] == 'var' then
        return eval_var(ast, obj)
    elseif ast[1] == 'var.length' then
        return eval_var_length(obj)
    elseif ast[1] == 'union' then
        return eval_union(ast, obj)
    elseif ast[1] == 'filter' then
        return eval_filter(ast, obj)
    elseif ast[1] == 'slice' then
        return eval_slice(ast, obj)
    end

    return 0
end

local function match_path(ast, path, parent, obj)
    local descendants = false
    local ast_iter = ipairs(ast)
    local ast_key, ast_spec = ast_iter(ast, 0)
    local match = MATCH_PARTIAL

    for path_index, component in ipairs(path) do
        local match_component = true
        if type(ast_spec) ~= 'table' and ast_spec == '..' then
            -- handle descendant switch upfront so descendant
            -- gets applied in this round already
            descendants = true

            -- move AST to item after descendant switch
            ast_key, ast_spec = ast_iter(ast, ast_key)
        end

        if type(ast_spec) == 'table' then
            if ast_spec[1] == '*' then
                -- wildcard match: same as non-table '*'
                if descendants then
                    match = MATCH_DESCENDANTS
                    break
                end
            elseif ast_spec[1] == 'union' or ast_spec[1] == 'slice' then
                -- match union or slice expression (on parent object)
                local matches, err = eval_ast(ast_spec, parent)
                if err then
                    return nil, err
                end
                --- @cast matches table[]
                for _, i in pairs(matches) do
                    match_component = tostring(i) == tostring(component)
                    if match_component then
                        break
                    end
                end
            elseif ast_spec[1] == 'filter' then
                -- match filter expression
                local filter_result, err = eval_ast(ast_spec, obj)
                if err then
                    if err.rc == codes.NOT_FOUND then
                        match_component = false
                    else
                        return nil, err
                    end
                else
                    match_component = filter_result and true or false
                end
            end
        else
            if ast_spec == '*' then
                -- match all children (any level)
                if descendants then
                    match = MATCH_DESCENDANTS
                    break
                end
            else
                -- 'normal' component name
                match_component = tostring(component) == tostring(ast_spec)
            end
        end

        -- apply filter upfront if it belongs to the currently observing component.
        if path_index == #path and ast_spec ~= "array" and match_component then
            local _, next_ast_spec = next(ast, ast_key)
            if next_ast_spec ~= nil and next_ast_spec[1] == 'filter' then
                local filter_result, err = eval_ast(next_ast_spec, obj)
                if err then
                    if err.rc == codes.NOT_FOUND then
                        match_component = false
                    else
                        return nil, err
                    end
                else
                    match_component = filter_result and true or false
                end
                ast_key, ast_spec = ast_iter(ast, ast_key)
            end
        end

        -- Clear descendants mode when a match is found
        if match_component then
            descendants = false
        else
            -- if there are no matches and the object is an array,
            -- then you need to iterate through its elements
            if type(obj) == "table" and not is_nil(obj[1]) then
                descendants = true
            end

            if not descendants then
                match = MISMATCH
                break
            end
        end

        -- next item in AST
        if not descendants then
            ast_key, ast_spec = ast_iter(ast, ast_key)
        end
    end

    if match == MATCH_PARTIAL and ast_spec == nil and not descendants then
        match = MATCH_ONE
    end
    return match
end

local function match_tree(nodes, ast, path, parent, obj, count)
    -- Try to match every node against AST
    local match, err = match_path(ast, path, parent, obj)
    if err then
        return err
    end
    if match == MATCH_ONE or match == MATCH_DESCENDANTS then
        -- This node matches. Add path and value to result
        -- (if max result count not yet reached)
        if count ~= nil then
            local n = 0
            for _ in pairs(nodes) do
                n = n + 1
                if n == count then
                    return
                end
            end
        end
        nodes[path] = obj
    end
    -- Recursively traverse children, if any
    if type(obj) == 'table' and (match == MATCH_PARTIAL or match == MATCH_DESCENDANTS) then
        for key, child in pairs(obj) do
            local path1 = {}
            for _, p in ipairs(path) do
                table.insert(path1, p)
            end
            table.insert(path1, type(key) == 'string' and key or (key - 1))
            local err = match_tree(nodes, ast, path1, obj, child, count)
            if err then
                return err
            end
        end
    end
end



-- ***********************************************************************************************
--
--  Parse the provided JSONPath expression into path components and their associated operations.
--
--      local path = jp.parse('$..author')
--      -- {
--      --     { expression: { type: 'root', value: '$' } },
--      --     { expression: { type: 'identifier', value: 'author' }, operation: 'member', scope: 'descendant' }
--      -- }
--
--  @param      expr (string)       JSONPath expression string
--
--  @return     result (mixed)      On success, a table holding path expressions matching the
--                                  input path expression. In case of error, nil
--
--  @return     err     (string)    Optional error message (only if <result> is nil)
--
function M.parse(expr)
    if expr == nil or type(expr) ~= 'string' then
        return nil, bad_request_error("missing or invalid 'expr' argument")
    end

    local ast = Ct(jsonpath_grammer * Cp()):match(expr)
    if ast == nil or #ast ~= 2 then
        return nil, bad_request_error('invalid expression "' .. expr .. '"')
    end
    if ast[2] ~= #expr + 1 then
        return nil, bad_request_error('invalid expression "' .. expr .. '" near "' .. expr:sub(ast[2]) .. '"')
    end
    return ast[1]
end



-- ***********************************************************************************************
--
--  Find elements and their corresponding paths in obj matching expr. Returns an array
--  of node objects where each node has a path containing an array of keys representing the
--  location within obj, and a value pointing to the matched element. Returns only first count
--  nodes if specified.
--
--      local nodes = jp.nodes(data, '$..author')
--      -- {
--      --   { path: {'$', 'store', 'book', 0, 'author'}, value: 'Nigel Rees' },
--      --   { path: {'$', 'store', 'book', 1, 'author'}, value: 'Evelyn Waugh' },
--      --   { path: {'$', 'store', 'book', 2, 'author'}, value: 'Herman Melville' },
--      --   { path: {'$', 'store', 'book', 3, 'author'}, value: 'J. R. R. Tolkien' }
--      -- }
--
function M.nodes(obj, expr, count)
    if obj == nil or type(obj) ~= 'table' then
        local err = bad_request_error("missing or invalid 'obj' argument")
        return nil, err
    end
    if expr == nil or (type(expr) ~= 'string' and type(expr) ~= 'table') then
        local err = bad_request_error("missing or invalid 'expr' argument")
        return nil, err
    end
    if count ~= nil and type(count) ~= 'number' then
        local err = bad_request_error("invalid 'count' argument")
        return nil, err
    end

    local ast, err
    if type(expr) == 'string' then
        -- parse JsonPath expression
        ast, err = M.parse(expr)
    elseif type(expr) == 'table' then
        -- parse abstract syntax tree (when used with jp.grammer())
        ast = expr
    end
    if ast == nil then
        if not err then
            local err = JsonPathError:new("internal error")
            err.rc = codes.INTERNAL_ERR
        end
        return nil, err
    end

    if count ~= nil and count == 0 then
        return {}
    end

    -- insert root element, if not yet present
    if #ast == 0 or ast[1] ~= '$' then
        table.insert(ast, 1, '$')
    end

    if #ast == 1 then
        return { { path = { '$' }, value = obj } }
    end

    local matches = {}
    local err = match_tree(matches, ast, { '$' }, {}, obj, count)
    if err then
        return nil, err
    end
    -- Sort results by path
    local sorted = {}
    for p, v in pairs(matches) do
        table.insert(sorted, { path = p, value = v })
    end
    table.sort(sorted, function(a, b)
        return table.concat(a.path, '.') < table.concat(b.path, '.')
    end)
    return sorted
end



-- ***********************************************************************************************
--
--  Find elements in obj matching expr. Returns an array of elements that satisfy the
--  provided JSONPath expression, or an empty array if none were matched. Returns only first
--  count elements if specified.
--
--      local authors = jp.query(data, '$..author')
--      -- { 'Nigel Rees', 'Evelyn Waugh', 'Herman Melville', 'J. R. R. Tolkien' }
--
function M.query(obj, expr, count)
    local nodes, err = M.nodes(obj, expr, count)
    if nodes == nil then
        return nil, err
    end
    local results = {}
    for _, n in ipairs(nodes) do
        table.insert(results, n.value)
    end
    return results
end



-- ***********************************************************************************************
--
--  Returns the value of the first element matching expr.
--
--      local author = jp.value(data, '$..author')
--      -- 'Nigel Rees'
--
function M.value(obj, expr, count)
    local nodes, err = M.nodes(obj, expr, count)
    if nodes == nil then
        return nil, err
    elseif nodes[1] then
        return nodes[1].value
    end

    local err = bad_request_error('no element matching expression')
    return nil, err
end



-- ***********************************************************************************************
--
--  Find paths to elements in obj matching expr. Returns an array of element paths
--  that satisfy the provided JSONPath expression. Each path is itself an array of keys
--  representing the location within obj of the matching element. Returns only first count
--  paths if specified.
--
--      local paths = jp.paths(data, '$..author')
--      -- {
--      --   {'$', 'store', 'book', 0, 'author'},
--      --   {'$', 'store', 'book', 1, 'author'},
--      --   {'$', 'store', 'book', 2, 'author'},
--      --   {'$', 'store', 'book', 3, 'author'}
--      -- }
--
function M.paths(obj, expr, count)
    local nodes, err = M.nodes(obj, expr, count)
    if nodes == nil then
        return nil, err
    end
    local results = {}
    for _, n in ipairs(nodes) do
        table.insert(results, n.path)
    end
    return results
end



-- ***********************************************************************************************
--
--  Provides the lua-jsonpath LPEG grammer for embedding in higher level LPEG grammers.
--
--  The abstract syntax tree matched for JsonPath elementes in a higher level LPEG grammer can
--  then be supplied to jp.nodes(), jp.paths() or jp.query() instead of the string expression.
--
--      local assignment = C(R'az') * P'=' * P'"' * jp.grammer() * P'"'
--      local var, ast = assignment:match('x="$..author"')
--      -- var = 'x'
--      local results = jp.query(data, ast)
--      -- { 'Nigel Rees', 'Evelyn Waugh', 'Herman Melville', 'J. R. R. Tolkien' }
--
function M.grammer()
    return jsonpath_grammer
end

return M
