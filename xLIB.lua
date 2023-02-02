--[[
	This LIB was written by XEKEX
	if you want to copy / understand / New idea to the Lib please contact me on GameGuardian Forum : 
	https://gameguardian.net/forum/profile/1258371-xekex/
	
	If u want to import The Script with MakeRequest link : https://raw.githubusercontent.com/chihaamin/XEKEX/main/xLIB.lua
	
]]--
XEK = {
    ['Text2Dword'] = function(str)
	local bytes = {}
	for i = 1,#str do 
	 bytes[#bytes+1] = (string.byte(str,i,i))
	 if #str%2 ~= 0 and i == #str then bytes[#bytes+1] = 0 end
	  end

	local final = ''
	for i = 2 ,#bytes,2 do 
	if i == #bytes then seperator = '::'..#str+4 else seperator = ';' end
	final = ((final..bytes[i-1] + bytes[i] * 2^16)..seperator):gsub('.0','')
	end
return final
    end,
    ['Dword2Text'] = function(val)
		string = ''
		for _ in val:gmatch('(%d+)%p') do
		char2,char1 = math.modf((_)/(2^16))
		char1 = math.floor(char1*2^16)
		if char2 == 0 then string = string..string.char(char1) break end
		string = string..(string.char(char1)..string.char(char2))
		end
        return string
    end,
    ['hex'] = function(val,hx)
            local val1 = string.format('%08X', val):sub(1,8)-- change this to 1,16 incase x64 bit memory
            local val2 = tostring(val1)
            if hx == true then return '0x'..tostring(val2) elseif hx == false then
            return tostring(val2)..'h'
            else return tostring(val2)
            end
    end,
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
    ['split'] = function(s, delimiter) -- split text with specific delimiter
            local result = {};
            for match in (s..delimiter):gmatch("(.-)"..delimiter) do
                table.insert(result, match);
            end
            return result;
    end,
    ['difference'] = function (str1,str2,str3) -- diffrence between two values / address (for double Xor encryption)
          local tab1 = {}
          local tab2 = {}
          local tab3 = {}
          local tab4 = {}
            for i = 1,#str1/2 do --Loop over strings
              tab1[i] = str1:sub(i%#str1*2-1,i%#str1*2)
              tab2[i] = str2:sub(i%#str2*2-1,i%#str2*2)
              tab3[i] = str3:sub(i%#str3*2-1,i%#str3*2)
              tab4[i] = str3:sub(i%#str3*2-1,i%#str3*2)
            end
        for i = 1,#tab3 do 
          local t,j = string.find(str2,tab1[i])
          local ind = math.floor(j/2)
          tab3[i]=tab4[ind]
        end
        local result = table.concat(tab3)
        return result
    end,
    ['encryp'] = function(message,enc) --encrypt text with runes
          local math = 'e19aa'
          local emt = {}
          local dmt = {}
               local function sp(s)
                    local result = {};
                    for match in (s):gmatch("(.)") do
                        table.insert(result, match);
                    end
                    return result;
                end
          local function hd(hex)
          
            return (hex:gsub("%x%x", function(digits) return string.char(tonumber(digits, 16)) end))
             end
          local function he(str)
            return (str:gsub(".", function(char) return string.format("%x", char:byte()) end))
             end
          if enc == nil then enc = true end
          if enc == true then 
        local t = {}
        for k=1,#message do
         local str = string.format('%X', string.byte(message:sub(k,k)))
          if str == 'A' or str == 'a' and #str == 1 then table.insert(t,'0a') else table.insert(t,str) end
         end
        emt = sp(table.concat(t))
        for k,v in pairs(emt) do 
          emt[k] = (math..v) end
        local encryp = table.concat(emt)
        return hd(encryp)
          elseif enc == false then 
             local  decryp = string.gsub(he(message), math, "")
              return hd(decryp)
          end
    end,
    ['ARMIT'] = function(value, precision, mode) 
	-- value any type true/123/263.3/..etc #should not be a string / precision need to be string for float and double / mode must be 32 or 64 as number / precision and mode are by default float and 32bit  if you want an int or bool as 64 bit make precision nil <- must
	-- it return a table each key 1.2.3..etc contain the instructions
precision = precision or "float"
mode = mode or 32

local instructions = {}
if type(value) == "boolean" then
if mode == 32 then
instructions[#instructions+1] = "~A MOV R0, #0x" .. (value and 1 or 0)
instructions[#instructions+1] = "~A BX LR"
elseif mode == 64 then
instructions[#instructions+1] = "~A MOV X0, #0x" .. (value and 1 or 0)
instructions[#instructions+1] = "~A RET"
end
elseif type(value) == "number" then
  if math.type(value) == "integer" then
  -- integer
  local hex = string.format("%X", value)
  if #hex <= 4 then
    if mode == 32 then
    instructions[#instructions+1] = "~A MOVW R0, #0x" .. hex
    instructions[#instructions+1] = "~A BX LR"
    elseif mode == 64 then
    instructions[#instructions+1] = "~A MOVZ X0, #0x" .. hex
    instructions[#instructions+1] = "~A RET"
    end
      else
    local low16 = string.format("%X", value & 0xFFFF)
    local high16 = string.format("%X", (value >> 16) & 0xFFFF)
        if mode == 32 then
        instructions[#instructions+1] = "~A MOVW R0, #0x" .. low16
        instructions[#instructions+1] = "~A MOVT R0, #0x" .. high16
        instructions[#instructions+1] = "~A BX LR"
        elseif mode == 64 then
        instructions[#instructions+1] = "~A MOVZ X0, #0x" .. low16
        instructions[#instructions+1] = "~A MOVK X0, #0x" .. high16
        instructions[#instructions+1] = "~A RET"
        end
    end
    elseif precision == "float" then
      local binary = string.pack("f", value)
    local hex = ""
    for i = 1, #binary do
      hex = hex .. string.format("%02X", string.byte(string.reverse(binary), i))
    end
    local low16 =  hex:sub(5, 8)
    local high16 = hex:sub(1, 4)
    if mode == 32 then
      instructions[#instructions+1] = "~A MOVW R0, #0x" .. low16
      instructions[#instructions+1] = "~A MOVT R0, #0x" .. high16
      instructions[#instructions+1] = "~A VMOV.F32 S0, R0"
      instructions[#instructions+1] = "~A BX LR"
    elseif mode == 64 then
      instructions[#instructions+1] = "~A MOVZ X0, #0x" .. low16
      instructions[#instructions+1] = "~A MOVK X0, #0x" .. high16
      instructions[#instructions+1] = "~A FMOV S0, X0"
      instructions[#instructions+1] = "~A RET"
    end
    elseif precision == "double" then 
      local binary = string.pack("d", value)
    local hex = ""
    for i = 1, 8 do
      hex = hex .. string.format("%02X", string.byte(string.reverse(binary), i))
    end

    if mode == 32 then
     instructions[#instructions+1] = "~A MOVT R0, #0x" .. string.sub(hex, 13, 16)
     instructions[#instructions+1] = "~A MOVW R0, #0x" .. string.sub(hex, 9, 12)
     instructions[#instructions+1] = "~A MOVT R1, #0x" .. string.sub(hex, 5, 8)
     instructions[#instructions+1] = "~A MOVW R1, #0x" .. string.sub(hex, 1, 4)
     instructions[#instructions+1] = "~A VMOV.F64 D0, R0, R1"
     instructions[#instructions+1] = "~A BX LR"
    elseif mode == 64 then
      instructions[#instructions+1] = "~A MOVK X0, #0x" .. string.sub(hex, 13, 16)
      instructions[#instructions+1] = "~A MOVZ X0, #0x" .. string.sub(hex, 9, 12)
      instructions[#instructions+1] = "~A MOVK X1, #0x" .. string.sub(hex, 5, 8)
      instructions[#instructions+1] = "~A MOVZ X1, #0x" .. string.sub(hex, 1, 4)
      instructions[#instructions+1] = "~A FMOV D0, X0, X1"
      instructions[#instructions+1] = "~A RET"
    end
    end
  end
  return instructions
end,
}
