--[[
	This LIB was written by XEKEX
	if you want to copy / understand / New idea to the Lib please contact me on GameGuardian Forum : 
	https://gameguardian.net/forum/profile/1258371-xekex/
	

	If u want to import The Script with MakeRequest link : 
	
]]--

XEK = {
    ['Text2Dword'] = function(text,order)
        local junk = {[1]=''}
        local stln = #text
        if stln%2 ~= 0 then stln = stln+1 end
      
        for i = 1,stln/2 do 
          local v1 = (string.byte(text,i%stln*2-1))
          local v2 = (string.byte(text,i%stln*2))
          if v2 == nil then v2 = 0 end
          local v3 = 65536*v2+v1
          if #text > 2 then table.insert(junk,1,junk[1]..';'..v3) 
          elseif #text <= 2 then
            table.insert(junk,1,v3) end
        end
        local function repeats(s,c)
          local _,n = s:gsub(c,"")
          return n
      end
        local ord = ''
        if order == true and stln > 2 then  ord = ('::'..repeats(tostring(junk[1]:sub(2)),';') * 4 + 1) end
        
      if #text > 2 then
        return junk[1]:sub(2)..ord else return junk[1]..ord end
    end,
    ['Dword2Text'] = function(val)
        local result= {[1] = ''}
        local splited = {};
        for match in (val..';'):gmatch("(.-)"..';') do
            table.insert(splited, match);
        end
        for i,v in pairs(splited) do 
        local chk = #tostring(splited[i])
        local v1 = math.floor(splited[i]/65536)
        local v2 = splited[i]-(65536*v1)
        local c1 = utf8.char(v2,v1)
        if chk < 7 then table.insert(result,1,result[1]..utf8.char(splited[i])) end
        if chk == 7 then table.insert(result,1,result[1]..c1) end
        if chk > 7 then table.insert(result,1,"Sorry this is not a readable string.") break end
        end
        return result[1]
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
    ['IEEE'] = function(Rvalue) -- IEEE754 floating point(convert float / double to hexdecimal )
	  if Rvalue == math.abs(Rvalue) then Sig = 0 else Sig = 1 end
Bias = nil
int,frac = math.modf(Rvalue)
if frac ~= 0 then frac = string.format("%."..(#tostring(Rvalue)-#tostring(int)-1).."f ",frac) end
Exponent_bias = (#tostring(Rvalue)-1)*math.log(2)
if Exponent_bias >= 0 and Exponent_bias <= 7.22	then Bias = 127 bit = 8 arc = 32
elseif Exponent_bias > 7.22	and Exponent_bias <= 15.95 then Bias = 1023 bit = 11 arc = 64
  elseif Exponent_bias > 15.95 then print('Error : Number too big') return end
function mul(_)
  _ = math.abs(_)
  local result = {}
  local pattern = {}
  local pf = false
  local start = nil
  repeat
    _ = _%1 * 2
    result[#result+1] = math.floor(_)
    pattern[#pattern+1] = string.format("%.2f",_)
for k = 2 ,#pattern-1 do 
  if pattern[#pattern] == pattern[k] then start = k+1 ;pf = true end 
  end
  until _ == 1 | 0 or pf == true
  if pf == true then
  pattern = {}
    for i = start ,#result do 
      pattern[#pattern+1] = result[i]
    end
  return table.concat(result),pattern
  end
   if pf == false then return table.concat(result),result end
end
mantissa,exponent = math.frexp(Rvalue/2)
exponent = mul(math.frexp((exponent + Bias )/2))
mantissa_,pattern = mul(mantissa)
mantissa = mantissa_:sub(2)
if #exponent < bit then 
  repeat 
    exponent = '0'..exponent 
    until #exponent == bit
    end
  value = Sig..exponent..mantissa
  value = value:sub(1,32)
if #value ~= arc then 
if table.concat(pattern) == mantissa_ then 
  repeat
    value = value..'0'
    until #value == arc+1
  else
    i = 1 
    repeat 
      if i == #pattern then value = value..pattern[i] i = 1 end
      value = value..pattern[i]
        i = i + 1
      until #value == arc
    end
end
local result = 0
for i=#value,1,-1 do 
power = #value-i
result = result + value:sub(i,i)*2^power
end
return (string.format('%X',result):sub(1,arc/4))
end,
	['Farm'] = function(value) -- hex value to arm 32 (float only)
if #value == 8 then
value1 = value:sub(1,4)
value2 = value:sub(5,8)
else 
print('Try Double Precision')
end
final = {
   [1] = '~A movw r0, #'..value2,
   [2] = '~A movt r0, '..value1,
   [3] = '~A vmov s15, r0',
   [4] = '~A vmov.f32 s0, s15',
   [5] = '~A bx lr',
  }
  return final
end,
}
