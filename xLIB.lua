--[[
	This LIB was written by XEKEX
	if you want to copy / understand / New idea to the Lib please contact me on GameGuardian Forum : 
	https://gameguardian.net/forum/profile/1258371-XEK/
	
	If u want to import The Script with MakeRequest link : https://raw.githubusercontent.com/chihaamin/XEK/main/xLIB.lua
	
]]--
XEK = {
['import'] = function (url) -- import a lib from any github or pastbin
local lib = nil
load_lib = gg.makeRequest(url)
  if load_lib.code == 200 then -- check the status of the request
   lib = load(load_lib.content,'bt')() -- similar to require('json')
   return lib
   else 
     print('IMPORT FUNCTION ERROR!')
     return nil
  end
end
    ['Text2Dword'] = function(str)
  -- Create an empty table to store the bytes of the input string
  local bytes = {}
  local seperator = ''
  -- Loop over each character in the input string
  for i = 1,#str do 
    -- Get the ASCII value of the character and add it to the bytes table
    bytes[#bytes+1] = (string.byte(str,i,i))

    -- If the length of the input string is odd, add a null byte to the end of the bytes table
    if #str%2 ~= 0 and i == #str then bytes[#bytes+1] = 0 end
  end

  -- Create an empty string to store the final result
  local final = ''

  -- Loop over every other byte in the bytes table (since we are encoding two bytes at a time)
  for i = 2 ,#bytes,2 do 
    -- If this is the last byte in the bytes table, append a special separator to indicate the length of the original string
    if i == #bytes then seperator = '::'..#str+4 else seperator = ';' end

    -- Convert the two bytes into a single number, and append it to the final string
    final = ((final..bytes[i-1] + bytes[i] * 2^16)..seperator):gsub('.0','')
  end

  -- Return the final string
  return final
end,
    --[[
-- Text2Dword function Convert a text to dword value
local DWORD = XEK.Text2Dword("berry")

-- Print the result
print(DWORD) --> Output: '6619234;7471218;121::9'

    ]]
    ['Dword2Text'] = function(val)
		local string = ''
		for _ in val:gmatch('(%d+)%p') do
		local char2,char1 = math.modf((_)/(2^16))
		char1 = math.floor(char1*2^16)
		if char2 == 0 then string = string..string.char(char1) break end
		string = string..(string.char(char1)..string.char(char2))
		end
        return string
    end,

    --[[
-- Dword2Text function Convert a Dword value to text
local sampleValue = "6619234;7471218;121"
local text = XEK.Dword2Text(sampleValue)

-- Print the result
print(text) --> Output: 'berry'

    ]]
    ['hex'] = function(val,hx)
            local val1 = string.format('%08X', val):sub(1,8)-- change this to 1,16 incase x64 bit memory
            local val2 = tostring(val1)
            if hx == true then return '0x'..tostring(val2) elseif hx == false then
            return tostring(val2)..'h'
            else return tostring(val2)
            end
    end,
--[[
The 'hex' function takes two arguments: a value to convert to hexadecimal and a boolean 'hx' indicating whether to add '0x' or 'h' prefix to the output.

If hx is true, the function returns the hexadecimal value with '0x' prefix.
If hx is false, the function returns the hexadecimal value with 'h' suffix.
If hx is not provided or not a boolean, the function returns the hexadecimal value without any prefix or suffix.
The function uses string formatting to convert the value to hexadecimal.

Examples:

Convert decimal value to hexadecimal with '0x' prefix
print(XEK.hex(255, true)) --> Output: 0xFF

Convert decimal value to hexadecimal with 'h' suffix
print(XEK.hex(255, false)) --> Output: FFh

Convert decimal value to hexadecimal without any prefix or suffix
print(XEK.hex(255)) --> Output: FF
]]
    ['dump'] = function(tab) -- dump a table
                if type(tab) == 'table' then
                   local s = '{ '
                   for k,v in pairs(tab) do
                      if type(k) ~= 'number' then k = '"'..k..'"' end
                      s = s .. '['..k..'] = ' .. XEK.Dump(v) .. ','
                   end
                   return s .. '} '
                else
                   return tostring(tab)
            
                end
    end,
--[[
dump function takes a table as input and returns a string representation of the table.
If the input is not a table, it returns a string representation of the input.

Parameters:

tab (table): the table to be dumped
Returns:

(string): the string representation of the table
Example usage:

local myTable = {name = "John", age = 30, hobbies = {"reading", "running"}}
print(XEK.Dump(myTable))
-- Output: { ["name"] = "John", ["age"] = 30, ["hobbies"] = { [1] = "reading", [2] = "running", } }
]]
    ['split'] = function(s, delimiter)

            local result = {};
            for match in (s..delimiter):gmatch("(.-)"..delimiter) do
                table.insert(result, match);
            end
            return result;
    end,
    --[[

split function splits a string into a table of substrings using a specified delimiter
The function takes two parameters: s, which is the string to be split, and delimiter, which is the character or string used to separate the substrings

-- Example usage:
local myString = "apple,banana,cherry,orange"
local myTable = split(myString, ",")

 The above code will split the string "apple,banana,cherry,orange" into substrings using the comma as the delimiter and store the result in a table called myTable

The resulting table will contain the following values:

myTable[1] = "apple"
myTable[2] = "banana"
myTable[3] = "cherry"
myTable[4] = "orange"

    ]]

    ['ARMIT'] = function (value, datatype , aarch) 
      aarch = aarch or false 
      local instructions = {} -- this will contain the instructions generated
      local neg = nil -- this parameter in unused for future usage ( mvn )
      local hex_size = nil
      local mov = (aarch and {'K','Z'}) or {'W','T'} -- logic parameter to pick MOVT / MOVW or MOVK / MOVZ
      local ggSyntax = ((aarch) and '~A8' or '~A') -- logic whether ~A for x32 or ~A8 for x64
      local datatype = datatype or (math.type(value) == 'integer') and 'int' or (math.type(value) == 'float') and 'f' or (math.type(value) == 'double') and 'd' or (type(value) == 'boolean') and 'bool' -- check the datatype
      if datatype == 'double' or  datatype == 'float' then datatype = (datatype == 'double') and 'd' or 'f' end -- change the float and double to be correct to the string.pack
      if datatype ~= 'bool' then 
      neg = (value ~= math.abs(value)  ) and true end
    if datatype == 'int' then  -- convert our value to hex
      local maxInt =  (not aarch and 0xFFFFFFFF) or 0xFFFFFFFFFFFFFFFF 
      local packset = (not aarch and "%08X") or "%016X"
          local hexString = string.format("%X", value)
          if type(value) == "number" and value < 0 and value >= -maxInt then 
            value = maxInt + value + 1
            hexString = string.format("%X", value)
          else
          hexString = string.format(packset, value)
          end
      value = hexString
    elseif (datatype == 'f') or (datatype == 'd') then -- getting the hex value of float/double value
          local binary = string.pack(datatype, value)
        local hex = ""
        for i = 1, #binary do
          hex = hex .. string.format("%02X", string.byte(string.reverse(binary), i))
        end
        value = hex
    elseif datatype == 'bool' then -- setting true or false to numbers
      value = ((not value) and '0') or '1'
    end
      local decimalValue = tonumber(value,16)
      hex_size = (decimalValue >= -0x80 and decimalValue <= 0xFF) and 1 -- getting the size of the hex value
                   or (decimalValue >= -0x8000 and decimalValue <= 0xFFFF) and 2
                   or (decimalValue >= -0x80000000 and decimalValue <= 0xFFFFFFFF) and 4
                   or (decimalValue >= -0x8000000000000000 and decimalValue <= 0x7FFFFFFFFFFFFFFF) and 8
                   or error("Value out of range")
    local reg =  ((aarch and hex_size == 8) and 'X') or ((aarch and hex_size == 4) and 'W') or 'R' -- get registry name
       if hex_size == 1 then -- generate instructions depending on the size of the value
    instructions[#instructions + 1] = string.format("%s MOV %s%d, #0x%s",ggSyntax,reg,0,string.format('%X',tonumber(value,16)))
    elseif hex_size == 2 then
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[1],reg,0,string.format('%X',tonumber(value,16)))
    elseif hex_size == 4 then 
    if aarch  then 
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s, LSL #16",ggSyntax,mov[1],reg,0,string.format('%04X',tonumber('0x'..value) & 0xFFFF))
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s, LSL #32",ggSyntax,mov[1],reg,0,string.format('%04X',tonumber('0x'..value) >> 16 & 0xFFFF))
    else
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[1],reg,0,string.format('%04X',tonumber('0x'..value) & 0xFFFF))
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[2],reg,0,string.format('%04X',tonumber('0x'..value) >> 16 & 0xFFFF))
    end
    elseif hex_size == 8 and aarch then 
    local value_bot = (tonumber('0x'..value) & 0xFFFFFFFF) -- splitting the hex value into 2 parts
    local value_top = (tonumber('0x'..value) >> 32 & 0xFFFFFFFF)
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[2],reg,0,string.format('%X',tonumber(value_bot) & 0xFFFF)) -- this will be the last xxxx in the hex value
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s, LSL #16",ggSyntax,mov[1],reg,0,string.format('%X',tonumber(value_bot) >> 16 & 0xFFFF)) -- the next value
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s, LSL #32",ggSyntax,mov[1],reg,0,string.format('%X',tonumber(value_top) & 0xFFFF)) -- bot top value
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s, LSL #48",ggSyntax,mov[1],reg,0,string.format('%X',tonumber(value_top) >> 16 & 0xFFFF)) --top value
    elseif hex_size == 8 and not aarch then 
    local value_bot = (tonumber('0x'..value) & 0xFFFFFFFF)-- splitting the hex value into 2 parts
    local value_top = (tonumber('0x'..value) >> 32 & 0xFFFFFFFF)
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[1],reg,0,string.format('%X',tonumber(value_bot) & 0xFFFF))
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[2],reg,0,string.format('%X',tonumber(value_bot) >> 16 & 0xFFFF))
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[1],reg,1,string.format('%X',tonumber(value_top) & 0xFFFF))
    instructions[#instructions + 1] = string.format("%s MOV%s %s%d, #0x%s",ggSyntax,mov[2],reg,1,string.format('%X',tonumber(value_top) >> 16 & 0xFFFF))
    end
    if (datatype == 'f') or (datatype == 'd') then -- add the FPU instructions
       local fpu = {}
       fpu = ((aarch and hex_size == 8) and {
        ['f'] = "~A8 FMOV S15, %s%d\n~A8 VMOV.F32 S0, S15",
        ['d'] = "~A8 FMOV D16, %s%d\n~A8 VMOV.F64 D0, D16"
         
       } 
    or (aarch and hex_size == 4) and {
       ['f'] = "~A8 FMOV S15, %s%d\n~A8 VMOV.F32 S0, S15",
       ['d'] = "~A8 FMOV D16, %s%d\n~A8 VMOV.F64 D0, D16"
           
         }) -- x64
    or { --x32
       ['f'] = "~A VMOV S15, %s%d\n~A VMOV.F32 S0, S15",
       ['d'] = "~A VMOV D16, %s%d, %s%d\n~A VMOV.F64 D0, D16"
      
    } -- setting the FPU parameter for the instructions with the string.format( "formatstring",... )
if (datatype == 'f') or (datatype == 'd') then -- add the FPU instructions
      if (hex_size == 8) or (not aarch and hex_size == 4) then
        instructions[#instructions + 1] = ((not aarch and (hex_size == 8)) and string.format(fpu[datatype] ,reg,1,reg,0)) or string.format(fpu[datatype] ,reg,0)  -- x64 FPU and x32
    elseif aarch and hex_size == 4 then
      instructions[#instructions + 1] = string.format(fpu[datatype] ,reg,0) -- X64 FPU
    end
  end


  end
    instructions[#instructions + 1] = aarch and '~A8 RET' or '~A BX LR' -- add the return instructions
    return instructions -- return the instructions
end,
--[[

--->    ( WARRNING! ) ALL LOGIC BELOW ARE SET DEPENDING ON THE FUNCTION PARAMETERS  <---

The purpose of this function is to convert a given value to assembly language instructions, 
based on the data type and architecture (AArch32 or AArch64) specified. 
The output of the function is a table of assembly language instructions.
The function first checks the data type of the value and determines whether it's an integer, float, double or boolean. 
Based on this, it converts the value to hexadecimal form, which is used to generate assembly instructions.
The aarch parameter is used to specify the architecture. If it's not provided, AArch32 is used by default.
Based on this, the function determines the register names to be used in the assembly instructions.
Parameters:
value: the value that will be converted to assembly instruction
datatype: the type of the value. It can be one of the following:
"int" for integer
"f" for single-precision float
"d" for double-precision float
"bool" for boolean
aarch: the architecture in which the instruction will run. It is optional and defaults to false which means AArch32. If set to true, the instruction will run in AArch64.
Returns:
instructions: a table containing the generated assembly instructions.

# Example 1: Converting an Integer to Assembly Instructions

local instructions = XEK.ARMIT(123456, "int", true)

The above function call will generate assembly instructions to move the value 123456 into a register in AArch64.

--> Output:
 instructions = {
[1] = '~A8 MOVK W0, #0xE240, LSL #16',
[2] = '~A8 MOVK W0, #0x0001, LSL #32',
[3] = '~A8 RET',
}


# Example 2: Converting a Boolean to Assembly Instructions

local instructions = XEK.ARMIT(true, "bool")

The above function call will generate assembly instructions to move the value 1 (true) into a register in AArch32.

--> Output:
 instructions = {
[1] = '~A MOV R0, #0x1',
[2] = '~A BX LR',
}


# Example 3: Converting a Float to Assembly Instructions

local instructions = XEK.ARMIT(3.14159, "f", true)

The above function call will generate assembly instructions to move the value 3.14159 into a floating point register in AArch64.

--> Output:
instructions = {
[1] = '~A8 MOVK W0, #0x0FD0, LSL #16',
[2] = '~A8 MOVK W0, #0x4049, LSL #32',
[3] = '~A8 FMOV S0, W0',
[4] = '~A8 RET',
}


# Example 4: Converting a Double to Assembly Instructions

local instructions = XEK.ARMIT(123456789.987654321, "d")

The above function call will generate assembly instructions to move the value 123456789.987654321 into a double precision floating point register in AArch32.

--> Output:
 instructions = {
[1] = '~A MOVW R0, #0x5BA8',
[2] = '~A MOVT R0, #0x57F3',
[3] = '~A MOVW R1, #0x6F34',
[4] = '~A MOVT R1, #0x419D',
[5] = '~A VMOV D0, R1, R0',
[6] = '~A BX LR',
 }

]]
 ['readBytes'] = function (addr, size) -- read bytes from memory , take start address and bytes size
  local values = {}
  for i = 0, size - 1 do
      local value = {}
      value.address = addr + i
      value.flags = gg.TYPE_BYTE
      values[#values + 1] = value
  end
  local results = gg.getValues(values)
      local bytes = {}
      for i, result in ipairs(results) do
        if result.value <= 0  then break; end
          bytes[#bytes + 1] = result.value
      end
      return string.char(table.unpack(bytes)) --return a string
  end,
  ['readWord'] = function (addr, size, delimiter)
    size = size or 1 -- set size to 1 if it's not provided
    delimiter = delimiter or ''
    local values = {}
    for i = 0, size - 1 do
      values[i+1] = {address = addr + i*2, flags = gg.TYPE_WORD}
    end
    local results = gg.getValues(values)
    local words = {}
    for i, result in ipairs(results) do
      if result.value <= 0  then break; end
      words[#words + 1] = result.value
    end
    return (delimiter and delimiter ~= '') and table.concat(words, delimiter) or words
  end,
  
  ['readDword'] = function (addr, size, delimiter)
    size = size or 1 -- set size to 1 if it's not provided
    delimiter = delimiter or ''
    local values = {}
    for i = 0, size - 1 do
      values[i+1] = {address = addr + i*4, flags = gg.TYPE_DWORD}
    end
    local results = gg.getValues(values)
    local dwords = {}
    for i, result in ipairs(results) do
      if result.value <= 0  then break; end
      dwords[#dwords + 1] = result.value
    end
    return (delimiter and delimiter ~= '') and table.concat(dwords, delimiter) or dwords
  end,
  
  ['readFloat'] = function (addr, size, delimiter)
    size = size or 1 -- set size to 1 if it's not provided
    delimiter = delimiter or ''
    local values = {}
    for i = 0, size - 1 do
      values[i+1] = {address = addr + i*4, flags = gg.TYPE_FLOAT}
    end
    local results = gg.getValues(values)
    local floats = {}
    for i, result in ipairs(results) do
      if result.value <= 0  then break; end
      floats[#floats + 1] = result.value
    end
    return (delimiter and delimiter ~= '') and table.concat(floats, delimiter) or floats
  end,
  
  ['readDouble'] = function (addr, size, delimiter)
    size = size or 1 -- set size to 1 if it's not provided
    delimiter = delimiter or ''
    local values = {}
    for i = 0, size - 1 do
      values[i+1] = {address = addr + i*8, flags = gg.TYPE_DOUBLE}
    end
    local results = gg.getValues(values)
    local doubles = {}
    for i, result in ipairs(results) do
      if result.value <= 0  then break; end
      doubles[#doubles + 1] = result.value
    end
    return (delimiter and delimiter ~= '') and table.concat(doubles, delimiter) or doubles
  end,
--[[

local words = readWord(addr, size, ';')       <-| return a string
local dwords = readDword(addr, size, '-')     <-| return a string
local floats = readFloat(addr, size, '|')     <-| return a string
local doubles = readDouble(addr, size, ':')   <-| return a string

    OR

local words = readWord(addr, size)            <-| return a table
local dwords = readDword(addr, size)          <-| return a table
local floats = readFloat(addr, size)          <-| return a table
local doubles = readDouble(addr, size)        <-| return a table

<< these function purpose is to read values from memory for comparison >>

]]

['getResults'] = function(maxCount, skip, addressMin, addressMax, valueMin, valueMax, type, fractional, pointer)
  -- Set default values for optional parameters
  maxCount = maxCount or gg.getResultsCount()
  skip = skip or 0
  addressMin = addressMin or nil
  addressMax = addressMax or nil
  valueMin = valueMin or nil
  valueMax = valueMax or nil
  type = type or nil
  fractional = fractional or nil
  pointer = pointer or nil
  
  -- Create a results table with data and original values
  local results = {data = {}, original = {}}
  
  -- Call the original function and store the result
  results.data = gg.getResults(maxCount, skip, addressMin, addressMax, valueMin, valueMax, type, fractional, pointer)
  
  -- Extract original values and store them

  
  -- Add the focus function to the results table
  results.focus = function(self) -- reset the original values
    if self.original and #self.original > 0 then  self.original = {} end
    for k,v in pairs(self.data) do 
      self.original[#self.original + 1] = v.value 
    end
  end
   -- Add the get function to the results table
results.get = function (self)
  self.data = gg.getValues(self.data)
end
  -- Add the update function to the results table
  results.update = function(self, value) -- set the values of the result
    -- Update single value
    if type(value) == "number" then
      for k,v in pairs(self.data) do
        v.value = value
      end
      gg.setValues(self.data)
    -- Update multiple values
    elseif type(value) == 'table' and #value == #self.data then   -- the edit table length must be equal to the number of results
      for k , v in pairs(self.data) do 
        v.value = value[k]
      end
      gg.setValues(self.data)
    end
  end
  
  -- Add the reset function to the results table
  results.reset = function(self) -- this will reset the result to the original values
    for k,v in pairs(self.data) do
      v.value = self.original[k]
    end
    gg.setValues(self.data)
  end
  results.offset = function (self,offset,focus) -- table:offset(0x8) -- this won't save as original for reset/ table:offset(0x8, true) this will save the offset address values to original for reset later
    for k,v in pairs(self.data) do 
      v.address = v.address + offset
    end
    self.data = gg.getValues(self.data)
    if focus then self:focus() end
  end
results.clear = function(self) -- this will destroy the result table make it nil (clear garbage)
  self = nil
return self
end
  results.append = function(self,table) -- this will append two tables together ( the both tables need to be used by XEK.getResults())
if type(table) == 'table' then 
for k,v in pairs(table.data) do 
self.data[#self.data + 1] = v
end
self:focus() -- this will save the original values of both table
else 
  error('Result append need to be a table') -- not giving a table used by this function will trigger an error :)
end
  end
  -- Return the modified results table
  return results 
end,

--[[

# Example usage of getResults function and its returned table
local t = getResults(10) or t = getResults()                       <-| Get 10 results or all result | you can specified parameter same as GG
print(t.data[1].address)                                           <-| Print the address of the first result

# Example usage of focus function
t:focus()                                                          <-| Save original values of results table
print(t.original[1])                                               <-| Print the original value of the first result

# Example usage of update function
t:update(999)                                                      <-| Set all values in result table to 999

# Example usage of reset function
t:reset()                                                          <-| Reset all values in result table to their original values

# Example usage of offset function
t:offset(0x8)                                                      <-| Add 8 to the address of each result

# Example usage of append function
local t2 = getResults(5)                                           <-| Get 5 more results
t:append(t2)                                                       <-| Append t2 results to t
print(#t.data)                                                     <-| Print the total number of results in t

# Example usage of get function
t:get()                                                            <-| Refresh the results table

# Example usage of clear function
t:clear()                                                          <-| Destroy the results table and clear garbage

--]]


}

