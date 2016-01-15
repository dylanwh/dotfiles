local function index(_, method)
   return function(_, ...)
      local method_args = { ... }
      return function(object)
         return object[method](object, unpack(method_args))
      end
   end
end

local M = {}

setmetatable(M, { __index = index });

M.foo()

return M