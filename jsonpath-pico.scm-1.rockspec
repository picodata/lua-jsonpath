-- An example of Picodata rockspec for jsonpath.
-- They are not being stored in this repo, but on the https://download.picodata.io/luarocks
package = "jsonpath"
version = "pico.scm-1"
source = {
   url = "git+https://github.com/picodata/lua-jsonpath",
   branch = "master"
}
description = {
   summary = "Query Lua data structures with JsonPath expressions. Robust and safe JsonPath engine for Lua.",
   detailed = [[
This library implements Stefan Goessner's JsonPath syntax (http://goessner.net/articles/JsonPath/) in Lua.

Lua JsonPath is compatible with David Chester's Javascript implementation (https://github.com/dchester/jsonpath).

The Lua JsonPath library was written from scratch by Frank Edelhaeuser. It's a pure Lua implementation based on a PEG grammer handled by LulPeg pattern-matching library (https://github.com/pygy/LuLPeg.git).
    ]],
   homepage = "https://github.com/picodata/lua-jsonpath",
   license = "MIT"
}
dependencies = {
   "lua >= 5.1",
   "lulpeg ~> pico.0.1.3-1"
}
build = {
   type = "builtin",
   modules = {
      jsonpath = "jsonpath.lua"
   }
}
