local BAD_REQUEST = 400
local NOT_FOUND = 404
local INTERNAL = 500

local function new(code, text)
	return setmetatable({
		code = code,
		text = text,
	}, {
		__tostring = function(self)
			return ("Error %s: %s"):format(self.code, self.text)
		end,
        __index = {
            is_bad_request = function(self) return self.code == BAD_REQUEST end,
            is_not_found = function(self) return self.code == NOT_FOUND end,
            is_internal = function(self) return self.code == INTERNAL end,
        }
	})
end

return {
	bad_request = function(text)
		return new(BAD_REQUEST, text)
	end,
	not_found = function(text)
		return new(NOT_FOUND, text)
	end,
	internal = function(text)
		return new(INTERNAL, text)
	end,
}
