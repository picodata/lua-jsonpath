#!/usr/bin/env tarantool

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


    Lua JsonPath test suite
    =======================

    Applicable test cases ported over from https://github.com/dchester/jsonpath
    Additional test cases added for lua-jsonpath

    Install dependencies:

        tarantoolctl rocks install jsonpath

        git clone https://github.com/bluebird75/luaunit
        cd luaunit
        sudo python doit.py install

    Usage:

        make test

]]--
local lu = require('luaunit')


-- For grammer tests
local lulpeg = require('lulpeg')


-- Module under test
package.path = '../?.lua;'..package.path
local jp = require('jsonpath')


-- Test data
local data = require('test.data')



-- Helper: sort node table by path
local function sortByPath(nodes)
    local sorted = {}
    for _,v in pairs(nodes) do
        table.insert(sorted, v)
    end
    table.sort(sorted, function(a, b)
        return table.concat(a.path, '.') < table.concat(b.path, '.')
    end)
    return sorted
end



testParse = {

    testShouldParseRootOnly = function()
        local path, err = jp.parse('$')
        lu.assertNil(err)
        lu.assertEquals(path, { '$' })
    end,

    testParsePathForStore = function()
        local path, err = jp.parse('$.store')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store' })
    end,

    testParsePathForTheAuthorsOfAllBooksInTheStore = function()
        local path, err = jp.parse('$.store.book[*].author')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', 'book', {'*'}, 'author' })
    end,

    testParsePathForAllAuthors = function()
        local path, err = jp.parse('$..author')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', '..', 'author' })
    end,

    testParsePathForAllThingsInStore = function()
        local path, err = jp.parse('$.store.*')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', '*' })
    end,

    testParsePathForPriceOfEverythingInTheStore = function()
        local path, err = jp.parse('$.store..price')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', '..', 'price' })
    end,

    testParsePathForTheLastBookInOrderViaExpression = function()
        local path, err = jp.parse('$..book[(@.length-1)]')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', '..', 'book', {'union', {'expr', {'expr', {'var.length'}, '-', {'expr', 1}}}}})
    end,

    testParsePathForTheFirstTwoBooksViaUnion = function()
        local path, err = jp.parse('$..book[0,1]')
        lu.assertNil(err)
        lu.assertNotNil(path)
        lu.assertEquals(path, { '$', '..', 'book', {'union', '0', '1'}})
    end,

    testParsePathForTheFirstTwoBooksViaSlice = function()
        local path, err = jp.parse('$..book[0:2]')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', '..', 'book', {'union', {'slice', '0', '2', '1'}}})
    end,

    testParsePathToFilterAllBooksWithIsbnNumber = function()
        local path, err = jp.parse('$..book[?(@.isbn)]')
        lu.assertNil(err)
        lu.assertEquals(path, {'$', '..', 'book', {'filter', {'expr', {'var', 'isbn'}}}})
    end,

    testParsePathToFilterAllBooksWithAPriceLessThan10 = function()
        local path, err = jp.parse('$..book[?(@.price<10)]')
        lu.assertNil(err)
        lu.assertEquals(path, {
            '$',
            '..',
            'book',
            {'filter', {'expr', {'expr', {'var', 'price'}, '<', {'expr', 10}}}}
        })
    end,

    testParseLongSingularQuery = function()
        local path, err = jp.parse('$..book[?(@.arr[1].arr["a"].field)]')
        local expr_ast = {'expr', {'var', 'arr', 1, 'arr', 'a', 'field'}}
        lu.assertNil(err)
        lu.assertEquals(path, { '$', '..', 'book', { 'filter', expr_ast }})
    end,

    testParsePathToMatchAllElements = function()
        local path, err = jp.parse('$..*')
        lu.assertNil(err)
        lu.assertEquals(path, {'$', '..', '*'})
    end,

    testParsePathWithLeadingMember = function()
        local path, err = jp.parse('store')
        lu.assertNil(err)
        lu.assertEquals(path, {'$', 'store'})
    end,

    testParsePathWithLeadingMemberAndFollowers = function()
        local path, err = jp.parse('Request.prototype.end')
        lu.assertNil(err)
        lu.assertEquals(path, {'$', 'Request', 'prototype', 'end'})
    end,

    testParserAstIsReinitializedAfterParseThrows = function()
        lu.assertNil(jp.parse('store.book...'))
        local path, err = jp.parse('$..price')
        lu.assertNil(err)
        lu.assertEquals(path, {'$', '..', 'price'})
    end,
}


testParseLuaPort = {
    testParseAllElementsViaSubscriptWildcard = function()
        local path, err = jp.parse('$..[*]')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', '..', {'*'} })
    end,

    testParseObjectSubscriptWildcard = function()
        local path, err = jp.parse('$.store[*]')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', {'*'} })
    end,

    testParseMemberNumericLiteralGetsFirstElement = function()
        local path, err = jp.parse('$.store.book.0')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', 'book', '0' })
    end,

    testParseDescendantNumericLiteralGetsFirstElement = function()
        local path, err = jp.parse('$.store.book..0')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', 'book', '..', '0' })
    end,

    test_SubscriptDoubleQuotedString = function()
        local path, err = jp.parse('$["store"]')
        lu.assertNil(err)
        lu.assertEquals(path, { '$', { 'union', 'store' }})
    end,

    testSubscriptSingleQuotedString = function()
        local path, err = jp.parse("$['store']")
        lu.assertNil(err)
        lu.assertEquals(path, { '$', { 'union', 'store' }})
    end,

    testParseUnionOfThreeArraySlices = function()
        local path, err = jp.parse('$.store.book[0:1,1:2,2:3]')
        lu.assertNil(err)
        lu.assertEquals(path, {
            '$',
            'store',
            'book',
            {
                'union',
                {'slice', '0', '1', '1'},
                {'slice', '1', '2', '1'},
                {'slice', '2', '3', '1'}
            }
        })
    end,

    testParseSliceWithStepGreaterThan1 = function()
        local path, err = jp.parse("$.store.book[0:4:2]")
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', 'book', { 'union', { 'slice', '0', '4', '2' }}})
    end,

    testParseUnionOfSubscriptStringLiteralKeys = function()
        local path, err = jp.parse("$.store['book','bicycle']")
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', { 'union', 'book', 'bicycle'}})
    end,

    testParseUnionOfSubscriptStringLiteralThreeKeys = function()
        local path, err = jp.parse("$.store.book[1]['title','author','price']")
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', 'book', { 'union', '1' }, { 'union', 'title', 'author', 'price'}})
    end,

    testParseUnionOfSubscriptIntegerThreeKeysFollowedByMemberChildIdentifier = function()
        local path, err = jp.parse("$.store.book[1,2,3]['title']")
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', 'book', { 'union', '1', '2', '3' }, { 'union', 'title'}})
    end,

    testParseUnionOfSubscriptIntegerThreeKeysFollowedByUnionOfSubscriptStringLiteralThreeKeys = function()
        local path, err = jp.parse("$.store.book[0,1,2,3]['title','author','price']")
        lu.assertNil(err)
        lu.assertEquals(path, { '$', 'store', 'book', { 'union', '0', '1', '2', '3' }, { 'union', 'title', 'author', 'price'}})
    end,

    testParseUnionOfSubscript4ArraySlicesFollowedByUnionOfSubscriptStringLiteralThreeKeys = function()
        local path, err = jp.parse("$.store.book[0:1,1:2,2:3,3:4]['title','author','price']")
        lu.assertNil(err)
        lu.assertEquals(path, {
            '$',
            'store',
            'book',
            {
                'union',
                {'slice', '0', '1', '1'},
                {'slice', '1', '2', '1'},
                {'slice', '2', '3', '1'},
                {'slice', '3', '4', '1'}
            },
            {'union', 'title', 'author', 'price'}
        })
    end,

    testParseNestedParenthesesEval = function()
        local path, err = jp.parse('$..book[?( @.length && (@.price.max - @.price.min + 20 > 50 || false) )]')
        lu.assertNil(err)
        lu.assertEquals(path, {
            '$',
            '..',
            'book',
            {
                'filter',
                {
                    'expr',
                    {
                        'expr',
                        {'var.length'},
                        '&&',
                        {
                            'expr',
                            {
                                'expr',
                                {
                                    'expr',
                                    {'var', 'price', 'max'},
                                    '-',
                                    {'var', 'price', 'min'},
                                    '+',
                                    {'expr', 20}
                                },
                                '>',
                                {'expr', 50}
                            },
                            '||',
                            {'expr', false}
                        }
                    }
                }
            }
        })
    end,
}


testParseNegative = {

    testParsePathWithLeadingMemberComponentThrows = function()
        local ast, err = jp.parse('.store')
        lu.assertNil(ast)
        lu.assertNotNil(err)
    end,

    testParsePathWithLeadingDescendantMemberThrows = function()
        local ast, err = jp.parse('..store')
        lu.assertNil(ast)
        lu.assertNotNil(err)
    end,

    testLeadingScriptThrows = function()
        local ast, err = jp.parse('()')
        lu.assertNil(ast)
        lu.assertNotNil(err)
    end,
}


testQuery = {

    testFirstLevelMember = function()
        local results, err = jp.nodes(data, '$.store')
        lu.assertNil(err)
        lu.assertItemsEquals(results, { { path = {'$', 'store'}, value = data.store } })
    end,

    testAuthorsOfAllBooksInTheStore = function()
        local results, err = jp.nodes(data, '$.store.book[*].author')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0, 'author'}, value = 'Nigel Rees' },
            { path = {'$', 'store', 'book', 1, 'author'}, value = 'Evelyn Waugh' },
            { path = {'$', 'store', 'book', 2, 'author'}, value = 'Herman Melville' },
            { path = {'$', 'store', 'book', 3, 'author'}, value = 'J. R. R. Tolkien' },
            { path = {'$', 'store', 'book', 4, 'author'}, value = 'Douglas Noël Adams' },
        }))
    end,

    testAllAuthors = function()
        local results, err = jp.nodes(data, '$..author')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0, 'author'}, value = 'Nigel Rees' },
            { path = {'$', 'store', 'book', 1, 'author'}, value = 'Evelyn Waugh' },
            { path = {'$', 'store', 'book', 2, 'author'}, value = 'Herman Melville' },
            { path = {'$', 'store', 'book', 3, 'author'}, value = 'J. R. R. Tolkien' },
            { path = {'$', 'store', 'book', 4, 'author'}, value = 'Douglas Noël Adams' },
        }))
    end,

    testAllThingsInStore = function()
        local results, err = jp.nodes(data, '$.store.*')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book'}, value = data.store.book },
            { path = {'$', 'store', 'bicycle'}, value = data.store.bicycle }
        }))
    end,

    testPriceOfEverythingInTheStore = function()
        local results, err = jp.nodes(data, '$.store..price')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0, 'price'}, value = 8.95 },
            { path = {'$', 'store', 'book', 1, 'price'}, value = 12.99 },
            { path = {'$', 'store', 'book', 2, 'price'}, value = 8.99 },
            { path = {'$', 'store', 'book', 3, 'price'}, value = 22.99 },
            { path = {'$', 'store', 'book', 4, 'price'}, value = 42LL },
            { path = {'$', 'store', 'bicycle', 'price'}, value = 19.95 }
        }))
    end,

    testLastBookInOrderViaExpression = function()
        local results, err = jp.nodes(data, '$..book[(@.length-1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store', 'book', 4}, value = data.store.book[5] }
        })
    end,

    testFirstTwoBooksViaUnion = function()
        local results, err = jp.nodes(data, '$..book[0,1]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0}, value = data.store.book[1] },
            { path = {'$', 'store', 'book', 1}, value = data.store.book[2] }
        }))
    end,

    testFirstTwoBooksViaSlice = function()
        local results, err = jp.nodes(data, '$..book[0:2]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0}, value = data.store.book[1] },
            { path = {'$', 'store', 'book', 1}, value = data.store.book[2] }
        }))
    end,

    testFilterAllBooksWithIsbnNumber = function()
        local results, err = jp.nodes(data, '$..book[?(@.isbn)]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 2}, value = data.store.book[3] },
            { path = {'$', 'store', 'book', 3}, value = data.store.book[4] }
        }))
    end,

    testFilterAllBooksWithAPriceLessThan10 = function()
        local results, err = jp.nodes(data, '$..book[?(@.price<10)]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store', 'book', 0}, value = data.store.book[1] },
            { path = {'$', 'store', 'book', 2}, value = data.store.book[3] }
        })
    end,

    testFirstTenOfAllElements = function()
        local results, err = jp.nodes(data, '$..*', 10)
        lu.assertNil(err)
        lu.assertIsTable(results)
        lu.assertEquals(#results, 10)
    end,

    testAllElements = function()
        local results, err = jp.nodes(data, '$..*')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = { '$', 'store' }, value = data.store },
            { path = { '$', 'store', 'book' }, value = data.store.book },
            { path = { '$', 'store', 'bicycle' }, value = data.store.bicycle },
            { path = { '$', 'store', 'book', 0 }, value = data.store.book[1] },
            { path = { '$', 'store', 'book', 1 }, value = data.store.book[2] },
            { path = { '$', 'store', 'book', 2 }, value = data.store.book[3] },
            { path = { '$', 'store', 'book', 3 }, value = data.store.book[4] },
            { path = { '$', 'store', 'book', 4 }, value = data.store.book[5] },
            { path = { '$', 'store', 'book', 0, 'category' }, value = 'reference' },
            { path = { '$', 'store', 'book', 0, 'author' }, value = 'Nigel Rees' },
            { path = { '$', 'store', 'book', 0, 'title' }, value = 'Sayings of the Century' },
            { path = { '$', 'store', 'book', 0, 'price' }, value = 8.95 },
            { path = { '$', 'store', 'book', 1, 'category' }, value = 'fiction' },
            { path = { '$', 'store', 'book', 1, 'author' }, value = 'Evelyn Waugh' },
            { path = { '$', 'store', 'book', 1, 'title' }, value = 'Sword of Honour' },
            { path = { '$', 'store', 'book', 1, 'price' }, value = 12.99 },
            { path = { '$', 'store', 'book', 2, 'category' }, value = 'fiction' },
            { path = { '$', 'store', 'book', 2, 'author' }, value = 'Herman Melville' },
            { path = { '$', 'store', 'book', 2, 'title' }, value = 'Moby Dick' },
            { path = { '$', 'store', 'book', 2, 'isbn' }, value = '0-553-21311-3' },
            { path = { '$', 'store', 'book', 2, 'price' }, value = 8.99 },
            { path = { '$', 'store', 'book', 3, 'category' }, value = 'fiction' },
            { path = { '$', 'store', 'book', 3, 'author' }, value = 'J. R. R. Tolkien' },
            { path = { '$', 'store', 'book', 3, 'title' }, value = 'The Lord of the Rings' },
            { path = { '$', 'store', 'book', 3, 'isbn' }, value = '0-395-19395-8' },
            { path = { '$', 'store', 'book', 3, 'price' }, value = 22.99 },
            { path = { '$', 'store', 'book', 4, 'category' }, value = 'guides' },
            { path = { '$', 'store', 'book', 4, 'author' }, value = 'Douglas Noël Adams' },
            { path = { '$', 'store', 'book', 4, 'title' }, value = 'The Hitchhiker’s Guide to the Galaxy' },
            { path = { '$', 'store', 'book', 4, 'price' }, value = 42LL },
            { path = { '$', 'store', 'bicycle', 'color' }, value = 'red' },
            { path = { '$', 'store', 'bicycle', 'price' }, value = 19.95 }
        }))
    end,

    testAllElementsViaSubscriptWildcard = function()
        local results, err = jp.nodes(data, '$..[*]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, jp.nodes(data, '$..*'))
    end,

    testObjectSubscriptWildcard = function()
        local results, err = jp.query(data, '$.store[*]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, { data.store.book, data.store.bicycle })
    end,

    testNoMatchReturnsEmptyArray = function()
        local results, err = jp.nodes(data, '$..bookz')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {})
    end,

    testMemberNumericLiteralGetsFirstElement = function()
        local results, err = jp.nodes(data, '$.store.book.0')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = { '$', 'store', 'book', 0 }, value = data.store.book[1] }
        })
    end,

    testDescendantNumericLiteralGetsFirstElement = function()
        local results, err = jp.nodes(data, '$.store.book..0')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = { '$', 'store', 'book', 0 }, value = data.store.book[1] }
        })
    end,

    testRootElementGetsUsOriginalObj = function()
        local results, err = jp.nodes(data, '$')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$'}, value = data }
        })
    end,

    test_SubscriptDoubleQuotedString = function()
        local results, err = jp.nodes(data, '$["store"]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store'}, value = data.store}
        })
    end,

    testSubscriptSingleQuotedString = function()
        local results, err = jp.nodes(data, "$['store']")
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store'}, value = data.store}
        })
    end,

    testLeadingMemberComponent = function()
        local results, err = jp.nodes(data, 'store')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store'}, value = data.store}
        })
    end,

    testUnionOfThreeArraySlices = function()
        local results, err = jp.query(data, "$.store.book[0:1,1:2,2:3]")
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            data.store.book[1],
            data.store.book[2],
            data.store.book[3]
        })
    end,

    testSliceWithStepGreaterThan1 = function()
        local results, err = jp.query(data, "$.store.book[0:4:2]")
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            data.store.book[1],
            data.store.book[3]
        })
    end,

    testSliceLastSlice = function()
        local results, err = jp.query(data, "$.store.book[-1:]")
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            data.store.book[5]
        })
    end,

    testSliceLastTwoSlices = function()
        local results, err = jp.query(data, "$.store.book[-2:]")
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            data.store.book[4],
            data.store.book[5]
        })
    end,

    testSliceSecondToLastSlice = function()
        local results, err = jp.query(data, "$.store.book[-2:-1]")
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            data.store.book[4]
        })
    end,

    testUnionOfSubscriptStringLiteralKeys = function()
        local results, err = jp.nodes(data, "$.store['book','bicycle']")
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book'}, value = data.store.book },
            { path = {'$', 'store', 'bicycle'}, value = data.store.bicycle },
        }))
    end,

    testUnionOfSubscriptStringLiteralThreeKeys = function()
        local results, err = jp.nodes(data, "$.store.book[0]['title','author','price']")
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0, 'title'}, value = data.store.book[1].title },
            { path = {'$', 'store', 'book', 0, 'author'}, value = data.store.book[1].author },
            { path = {'$', 'store', 'book', 0, 'price'}, value = data.store.book[1].price }
        }))
    end,

    testUnionOfSubscriptIntegerThreeKeysFollowedByMemberChildIdentifier = function()
        local results, err = jp.nodes(data, "$.store.book[1,2,3]['title']")
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 1, 'title'}, value = data.store.book[2].title },
            { path = {'$', 'store', 'book', 2, 'title'}, value = data.store.book[3].title },
            { path = {'$', 'store', 'book', 3, 'title'}, value = data.store.book[4].title }
        }))
    end,

    testUnionOfSubscriptIntegerThreeKeysFollowedByUnionOfSubscriptStringLiteralThreeKeys = function()
        local results, err = jp.nodes(data, "$.store.book[0,1,2,3]['title','author','price']")
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0, 'title'}, value = data.store.book[1].title },
            { path = {'$', 'store', 'book', 0, 'author'}, value = data.store.book[1].author },
            { path = {'$', 'store', 'book', 0, 'price'}, value = data.store.book[1].price },
            { path = {'$', 'store', 'book', 1, 'title'}, value = data.store.book[2].title },
            { path = {'$', 'store', 'book', 1, 'author'}, value = data.store.book[2].author },
            { path = {'$', 'store', 'book', 1, 'price'}, value = data.store.book[2].price },
            { path = {'$', 'store', 'book', 2, 'title'}, value = data.store.book[3].title },
            { path = {'$', 'store', 'book', 2, 'author'}, value = data.store.book[3].author },
            { path = {'$', 'store', 'book', 2, 'price'}, value = data.store.book[3].price },
            { path = {'$', 'store', 'book', 3, 'title'}, value = data.store.book[4].title },
            { path = {'$', 'store', 'book', 3, 'author'}, value = data.store.book[4].author },
            { path = {'$', 'store', 'book', 3, 'price'}, value = data.store.book[4].price }
        }))
    end,

    testUnionOfSubscript4ArraySlicesFollowedByUnionOfSubscriptStringLiteralThreeKeys = function()
        local results, err = jp.nodes(data, "$.store.book[0:1,1:2,2:3,3:4]['title','author','price']")
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0, 'title'}, value = data.store.book[1].title },
            { path = {'$', 'store', 'book', 0, 'author'}, value = data.store.book[1].author },
            { path = {'$', 'store', 'book', 0, 'price'}, value = data.store.book[1].price },
            { path = {'$', 'store', 'book', 1, 'title'}, value = data.store.book[2].title },
            { path = {'$', 'store', 'book', 1, 'author'}, value = data.store.book[2].author },
            { path = {'$', 'store', 'book', 1, 'price'}, value = data.store.book[2].price },
            { path = {'$', 'store', 'book', 2, 'title'}, value = data.store.book[3].title },
            { path = {'$', 'store', 'book', 2, 'author'}, value = data.store.book[3].author },
            { path = {'$', 'store', 'book', 2, 'price'}, value = data.store.book[3].price },
            { path = {'$', 'store', 'book', 3, 'title'}, value = data.store.book[4].title },
            { path = {'$', 'store', 'book', 3, 'author'}, value = data.store.book[4].author },
            { path = {'$', 'store', 'book', 3, 'price'}, value = data.store.book[4].price }
        }))
    end,


    testNestedParenthesesEval = function()
        local pathExpression = '$..book[?( @.price && (@.price + 20 || false) )]'
        local results, err = jp.query(data, pathExpression)
        lu.assertNil(err)
        lu.assertItemsEquals(results, data.store.book)
    end,

    testArrayIndicesFrom0To100 = function()
        local data = {}
        for i = 1,100 do
            data[i] = math.random()
        end
        for i = 1,100 do
            local results, err = jp.query(data, '$['..tostring(i - 1)..']')
            lu.assertNil(err)
            lu.assertItemsEquals(results, {data[i]})
        end
    end,

    testDescendantSubscriptNumericLiteral1 = function()
        local data = { 0, { 1, 2, 3 }, { 4, 5, 6 } }
        local results, err = jp.query(data, '$..[0]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, { 0, 1, 4 })
    end,

    testDescendantSubscriptNumericLiteral2 = function()
        local data = { 0, 1, { 2, 3, 4 }, { 5, 6, 7, { 8, 9 , 10 } } }
        local results, err = jp.query(data, '$..[0,1]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, { 0, 1, 2, 3, 5, 6, 8, 9 })
    end,

    testThrowsForNoInput = function()
        local result, err = jp.query()
        lu.assertNil(result)
        lu.assertNotNil(err)
    end,

    testThrowsForBadInput1 = function()
        local result, err = jp.query('string', 'string')
        lu.assertNil(result)
        lu.assertNotNil(err)
    end,

    testThrowsForBadInput2 = function()
        local result, err = jp.query({}, nil)
        lu.assertNil(result)
        lu.assertNotNil(err)
    end,

    testThrowsForBadInput3 = function()
        local result, err = jp.query({}, 42)
        lu.assertNil(result)
        lu.assertNotNil(err)
    end,

    testFilterFirstLevel = function ()
        local data = {
            numeric = 0,
            string = "0"
        }
        local result, err = jp.query(data, "$[?(@.numeric == 0)]")
        lu.assertEquals(result, {data})
        lu.assertNil(err)

        local result, err = jp.query(data, '$[?(@.string == "0")]')
        lu.assertEquals(result, {data})
        lu.assertNil(err)
    end,

    testFilterWithDeepPropertyAccessor = function ()
        local data = {
            outer = {
                inner = {
                    key = 5
                }
            }
        }

        local result, err = jp.query(data, "$[?(@.outer.inner.key == 5)]")
        lu.assertEquals(result, {data})
        lu.assertNil(err)
    end,

  testDirectFilter1 = function ()
        local data = {
            some_unused_key = "",
            inner = {
                array = {
                    {key = 1},
                    {key = 2},
                    {key = 3}
                }
            }
        }

        local result, err = jp.query(data, "$.inner.array[?(@.key == 1 || @.key == 2)]")
        lu.assertItemsEquals(result, {data.inner.array[1], data.inner.array[2]})
        lu.assertNil(err)
    end,

    testDirectFilter2 = function ()
        local data = {
            some_unused_key = "",
            array = {
                {key = 1},
                {key = 2},
                {key = 3}
            }
        }

        local result, err = jp.query(data, "array[?(@.key == 1 || @.key == 2)]")
        lu.assertItemsEquals(result, {data.array[1], data.array[2]})
        lu.assertNil(err)
    end,

    testDirectFilter3 = function ()
        local data = {
            some_unused_key = "",
            array = {
                {key = 1},
                {key = 2},
                {key = 3}
            }
        }

        local result, err = jp.query(data, "array[?(@.key < 2)]")
        lu.assertItemsEquals(result, {data.array[1]})
        lu.assertNil(err)
    end,

    testRecursiveOperatorFilter = function ()
        local data = {
            some_unused_key = "",
            inner = {
                array = {
                    {key = 1},
                    {key = 2},
                    {key = 3}
                }
            }
        }

        local result, err = jp.query(data, "$..array[?(@.key == 1 || @.key == 2)]")
        lu.assertItemsEquals(result, {data.inner.array[1], data.inner.array[2]})
        lu.assertNil(err)
    end,
    
    testRecursiveOperatorFilterGT = function()
        local data = {
            photo = {
                size = 400,
            },
        }

        local result, err = jp.query(data, "$..photo[?(@.size>'400')]")
        lu.assertItemsEquals(result, {})
        lu.assertNil(err)
    end,

    testFilterNull = function()
        local array = {
            { item = 1, existing_field = nil },
            { item = 2, existing_field = jp.NULL },
            { item = 3, existing_field = 3 },
        }
        local result, err = jp.query(array, "$[?(@.existing_field==null)]")
        lu.assertItemsEquals(result, { array[2] })
        lu.assertNil(err)
    end,

    testNullBoolEquality = function()
        lu.assertIsFalse(jp.NULL == true)
        lu.assertIsTrue(jp.NULL ~= true)
        lu.assertIsTrue(true ~= jp.NULL)
        lu.assertIsFalse(jp.NULL == false)
        lu.assertIsTrue(jp.NULL ~= false)
        lu.assertIsTrue(false ~= jp.NULL)
    end,

    testFilterNullBool = function()
        local array = {
            { id = 1, is_end = nil },
            { id = 2, is_end = jp.NULL },
            { id = 3, is_end = true },
            { id = 4, is_end = false },
        }
        local result, err = jp.query(array, "$[?(@.is_end==null)]")
        lu.assertItemsEquals(result, { array[2] })
        lu.assertNil(err)

        result, err = jp.query(array, "$[?(@.is_end!=null)]")
        lu.assertItemsEquals(result, { array[3], array[4] })
        lu.assertNil(err)

        result, err = jp.query(array, "$[?(null==@.is_end)]")
        lu.assertItemsEquals(result, { array[2] })
        lu.assertNil(err)
    end,

    testFilterInnerSubscript = function()
        local array = {
            { array_field = { "prod", "no" } },
            { array_field = { "test", "yes" } },
        }
        local result, err = jp.query(array, '$[?(@.array_field[0]=="test")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2] })
    end,

    testFilterSubscriptEqualsNull = function()
        local array = {
            { array_field = { { value = "jp.NULL" }, "fake null" } },
            { array_field = { { value = jp.NULL}, "real null" } },
            { array_field = { { value = nil }, "lua null" } },
        }
        local result, err = jp.query(array, '$[?(@.array_field[0].value==null)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2] })
    end,

    testFilterSubscriptInLogicalAnd = function()
        local array = {
            { array_field = { { value = "jp.NULL" }, "fake null" }, name = "A" },
            { array_field = { { value = jp.NULL }, "real null" }, name = "B" },
            { array_field = { { value = "something" }, "real null 2" } , name = "C" },
            { array_field = { { value = nil }, "lua null" } , name = "D" },
        }
        local result, err = jp.query(array, '$[?(@.name=="B" && @.array_field[0].value==null)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2] })
        result, err = jp.query(array, '$[?(@.name=="C" && @.array_field[0].value=="something")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[3] })
    end,

    testFilterUnionValue = function()
        local has_no_isbn_query = '$.store.book[1,3,5][?(@.isbn!="0-553-21311-3")]'
        local result, err = jp.query(data, has_no_isbn_query)
        lu.assertNil(err)
        lu.assertEquals(result, { data.store.book[4] })
    end,

    test64Int = function ()
        local result, err = jp.query(data, "$..book[?( @.price && (@.price == '42') )]")
        lu.assertItemsEquals(result, { {
            author="Douglas Noël Adams",
            category="guides",
            price=42LL,
            title="The Hitchhiker’s Guide to the Galaxy"
        } })
        lu.assertNil(err)
    end,

    testFilterNullComparison = function()
        local array = {
            { id = 1, value = nil },
            { id = 2, value = 20 },
            { id = 3, value = 30 },
        }
        local result, err = jp.query(array, '$[?(@.value>"20")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[3] })

        local result, err = jp.query(array, '$[?(@.value>="20")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2], array[3] })

        local result, err = jp.query(array, '$[?(@.value<"30")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2] })

        local result, err = jp.query(array, '$[?(@.value<="30")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2], array[3] })
    end,

    testFilterIntBoolComparison = function ()
        local array = {
            { id = 1, value = 0 },
            { id = 2, value = 1 },
            { id = 3, value = 2 },
        }
        local result, err = jp.query(array, '$[?(@.value==true)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2] })

        local result, err = jp.query(array, '$[?(@.value>true)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[3] })

        local result, err = jp.query(array, '$[?(@.value>=true)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[2], array[3] })

        local result, err = jp.query(array, '$[?(@.value<true)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[1] })

        local result, err = jp.query(array, '$[?(@.value<=true)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[1], array[2] })
    end,

    testFilterBoolIntComparison = function ()
        local array = {
            { id = 1, value = true },
            { id = 2, value = false },
        }
        local result, err = jp.query(array, '$[?(@.value==1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, { array[1] })

        local result, err = jp.query(array, '$[?(@.value>1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value>=1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value<1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value<=1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})
    end,
    
    testFilterBoolStrComparison = function ()
        local array = {
            { id = 1, value = true },
            { id = 2, value = false },
        }
        local result, err = jp.query(array, '$[?(@.value=="1")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value>"1")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value>="1")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value<"1")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value<="1")]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})
    end,

    testFilterArithmeticOpOnBool = function ()
        local array = {
            { id = 1, value = 0 },
            { id = 1, value = 1 },
            { id = 2, value = 2 },
        }
        local result, err = jp.query(array, '$[?(@.value==true+1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value==true*1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value==true/1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value==true%1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})

        local result, err = jp.query(array, '$[?(@.value<>false+1)]')
        lu.assertNil(err)
        lu.assertItemsEquals(result, {})
    end,
}


testLessons = {
    testCommaInEval = function()
        local path = "$..book[?(@.price && ',')]"
        local results, err = jp.query(data, path)
        lu.assertNil(err)
        lu.assertItemsEquals(results, data.store.book)
    end,

    testMemberNamesWithDots = function()
        local data = { ['www.google.com'] = 42, ['www.wikipedia.org'] = 190 }
        local results, err = jp.query(data, "$['www.google.com']")
        lu.assertNil(err)
        lu.assertItemsEquals(results, { 42 })
    end,

    testNestedObjectsWithFilter = function()
        local data = {
            dataResult = {
                object = {
                    objectInfo = {
                        className = 'folder',
                        typeName = 'Standard Folder',
                        id = 'uniqueId'
                    }
                }
            }
        }
        local results, err = jp.query(data, "$..object[?(@.className=='folder')]")
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            data.dataResult.object.objectInfo,
        })
    end,

    testScriptExpressionsWithAtChar = function()
        local data = {
            DIV = { {
                ['@class'] = 'value',
                val = 5
            } }
        }
        local results, err = jp.query(data, "$..DIV[?(@['@class']=='value')]")
        lu.assertNil(err)
        lu.assertItemsEquals(results, data.DIV)
    end,
}



testSugar = {
    testValueMethodGetsUsAValue = function()
        local data = { a = 1, b = 2, c = 3, z = { a = 100, b = 200 } }
        local b, err = jp.value(data, '$..b')
        lu.assertNil(err)
        lu.assertItemsEquals(b, data.b)
    end,
}



testGrammer = {

    testGrammerNodes = function()
        local assignment = lulpeg.C(lulpeg.R'az') * lulpeg.P'=' * lulpeg.P'"' * jp.grammer() * lulpeg.P'"'
        local var, ast = assignment:match('x="$..author"')
        lu.assertItemsEquals(var, 'x')
        lu.assertItemsEquals(ast, { '$', '..', 'author' })
        local results, err = jp.nodes(data, ast)
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store', 'book', 0, 'author'}, value = 'Nigel Rees' },
            { path = {'$', 'store', 'book', 1, 'author'}, value = 'Evelyn Waugh' },
            { path = {'$', 'store', 'book', 2, 'author'}, value = 'Herman Melville' },
            { path = {'$', 'store', 'book', 3, 'author'}, value = 'J. R. R. Tolkien' },
            { path = {'$', 'store', 'book', 4, 'author'}, value = 'Douglas Noël Adams' },
        })
    end,

    testGrammerQuery = function()
        local assignment = lulpeg.C(lulpeg.R'az') * lulpeg.P'=' * lulpeg.P'"' * jp.grammer() * lulpeg.P'"'
        local var, ast = assignment:match('x="$..author"')
        lu.assertItemsEquals(var, 'x')
        lu.assertItemsEquals(ast, { '$', '..', 'author' })
        local results, err = jp.query(data, ast)
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            'Nigel Rees',
            'Evelyn Waugh',
            'Herman Melville',
            'J. R. R. Tolkien',
            'Douglas Noël Adams',
        })
    end,
}


testDocumentation = {

    testReadmeIntroductoryExample = function()
        local cities = {
            { name = 'London', population = 8615246 },
            { name = 'Berlin', population = 3517424 },
            { name = 'Madrid', population = 3165235 },
            { name = 'Rome',   population = 2870528 }
        }
        local names, err = jp.query(cities, '$..name')
        lu.assertNil(err)
        lu.assertItemsEquals(names, { 'London', 'Berlin', 'Madrid', 'Rome' })
    end,

    testReadmeExpressionsTheAuthorsOfAllBooksInTheStore = function()
        testQuery.testAuthorsOfAllBooksInTheStore()
    end,

    testReadmeExpressionsAllAuthors = function()
        testQuery.testAllAuthors()
    end,

    testReadmeExpressionsAllThingsInStore = function()
        testQuery.testAllThingsInStore()
    end,

    testReadmeExpressionsThePriceOfEverythingInTheStore = function()
        testQuery.testPriceOfEverythingInTheStore()
    end,

    testReadmeExpressionsTheThirdBookViaArraySubscript = function()
        local results, err = jp.nodes(data, '$..book[2]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 2}, value = data.store.book[3] }
        }))
    end,

    testReadmeExpressionsTheThirdBookViaScriptSubscript = function()
        testQuery.testLastBookInOrderViaExpression()
    end,

    testReadmeExpressionsTheLastBookInOrder = function()
        local results, err = jp.nodes(data, '$..book[-1:]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 4 }, value = data.store.book[5] }
        }))
    end,

    testReadmeExpressionsTheLastTwoBooksInOrder = function()
        local results, err = jp.nodes(data, '$..book[-2:]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 3 }, value = data.store.book[4] },
            { path = {'$', 'store', 'book', 4 }, value = data.store.book[5] }
        }))
    end,

    testReadmeExpressionsTheSecondToLastBookInOrder = function()
        local results, err = jp.nodes(data, '$..book[-2:-1]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 3 }, value = data.store.book[4] }
        }))
    end,

    testReadmeExpressionsTheFirstTwoBooksViaSubscriptUnion = function()
        testQuery.testFirstTwoBooksViaUnion()
    end,

    testReadmeExpressionsTheFirstTwoBooksViaSubscriptArraySlice = function()
        local results, err = jp.nodes(data, '$..book[:2]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, sortByPath({
            { path = {'$', 'store', 'book', 0 }, value = data.store.book[1] },
            { path = {'$', 'store', 'book', 1 }, value = data.store.book[2] }
        }))
    end,

    testReadmeExpressionsFilterAllBooksWithIsbnNumber = function()
        testQuery.testFilterAllBooksWithIsbnNumber()
    end,

    testReadmeExpressionsFilterAllBooksCheaperThan10 = function()
        testQuery.testFilterAllBooksWithAPriceLessThan10()
    end,

    testReadmeExpressionsFilterAllBooksThatCost8P95 = function()
        local results, err = jp.nodes(data, '$..book[?(@.price==8.95)]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store', 'book', 0}, value = data.store.book[1] }
        })
    end,

    testReadmeExpressionsFilterAllFictionBooksCheaperThan30 = function()
        local results, err = jp.nodes(data, '$..book[?(@.price<30 && @.category=="fiction")]')
        lu.assertNil(err)
        lu.assertItemsEquals(results, {
            { path = {'$', 'store', 'book', 1}, value = data.store.book[2] },
            { path = {'$', 'store', 'book', 2}, value = data.store.book[3] },
            { path = {'$', 'store', 'book', 3}, value = data.store.book[4] }
        })
    end,

    testReadmeExpressionsAllMembersOfLuaStructure = function()
        testQuery.testAllElements()
    end,

    testReadmeQueryExample = function()
        local author, err = jp.query(data, '$..author')
        lu.assertNil(err)
        lu.assertItemsEquals(author, {
            'Nigel Rees',
            'Evelyn Waugh',
            'Herman Melville',
            'J. R. R. Tolkien',
            'Douglas Noël Adams',
        })
    end,

    testReadmeValueExample = function()
        local author, err = jp.value(data, '$..author')
        lu.assertNil(err)
        lu.assertItemsEquals(author, 'Nigel Rees')
    end,

    testReadmePathsExample = function()
        local author, err = jp.paths(data, '$..author')
        lu.assertNil(err)
        lu.assertItemsEquals(author, {
            {'$', 'store', 'book', 0, 'author' },
            {'$', 'store', 'book', 1, 'author' },
            {'$', 'store', 'book', 2, 'author' },
            {'$', 'store', 'book', 3, 'author' },
            {'$', 'store', 'book', 4, 'author' },
        })
    end,

    testReadmeNodesExample = function()
        testQuery.testAllAuthors()
    end,

    testReadmeParseExample = function()
        testParse.testParsePathForAllAuthors()
    end,

    testReadmeGrammerExample = function()
        testGrammer.testGrammerQuery()
    end,
}


-- Run test suites
os.exit(lu.LuaUnit.run())
