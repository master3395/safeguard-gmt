--SafeGuard: Version 1.0
--Client Edition

script.Parent=nil;

--Debug Management Section

local DebugEnabled=true;
local debug=function(...)
	if DebugEnabled==true then
		local args={...};
		print("[ClientDebug] "..unpack(args));
	end;
end;

--Utility functions

Create=function(Class)
    return(function(Data)
       	local It=Instance.new(Class);
        for _,v in pairs(Data)do
            if(type(v)=='userdata' and type(_)=='number')then
                v.Parent=It;
            elseif(type(v)=='function')then
                It[_]:connect(function(...)getfenv(v).self=It;ypcall(v,...)end);
            else
                r,e=ypcall(function()It[_]=v;end);
                if(not r)then print(e) end;
            end;
        end;
        return(It);
    end);
end;

IsInstance=function(obj)
	return pcall(function() return obj:IsA(obj.ClassName);end);
end;

IsRobloxLocked=function(obj)
	if not IsInstance(obj) then
		return;
	else
		local arch=obj.Archivable;
		local suc,res=pcall(function() obj.Archivable=arch==true and false or true;end);
		if not suc then
			return true;
		else
			obj.Archivable=arch;
			return false;
		end;
	end;
end;

PDestroy=function(obj)
	if not IsInstance(obj) then
		return false;
	else
		if not IsRobloxLocked(obj) then
			return pcall(function() obj:Destroy() end);
		else
			get'Debris':AddItem(obj,1);
			return true;
		end;
	end;
end;

get=function(s) return game:GetService(s);end;

JSONEncode=function(...)
	return get'HttpService':JSONEncode(...);
end;

JSONDecode=function(...)
	return get'HttpService':JSONDecode(...);
end;

EncodeURL=function(...)
	return get'HttpService':UrlEncode(...);
end;

--System Definitions


_sg={};
_sg.Functions={};
_sg.LocalPlayer=get'Players'.LocalPlayer;
_sg.Modules={};		--Contains "Modules" used to extend system features.
_sg.Registry={		--Replacement of the "Data" system.
	["SYSTEM"]={
		ServerVersion=1.0;
		ServerBuild=100001;
	};
	["DEFAULT_SETTINGS"]={
		["CHAT_REG"]={
			Starter="!";
			Split="//";
		};
	};
	["LOGS"]={
	
	};
	["REFERENCES"]={
		Sync=get'ReplicatedStorage':WaitForChild('SafeSync');
	};
};

--System Functions



--Registry Management Functions

--TODO: Add Registry Management Functions
_sg.Functions.AddReg=function()

end



--Data Logging Functions

_sg.Functions.CreateLogFolder=function(name)
	if not type(_sg.Registry.LOGS[name])=="table" then
		_sg.Registry.LOGS[name]={};
	end;
end;

--TODO: Finish Logging API

--Module related Functions

--TODO: Start off experimenting with a basic Module System,
--then expand to a more complex system.

_sg.Functions.CreateModule=function(ModuleName,ModuleLoadFunction,ModuleUnloadFunction)
	table.insert(_sg.Modules,{
		Name=ModuleName;
		Load=ModuleLoadFunction;
		Unload=ModuleUnloadFunction~=nil and ModuleUnloadFunction or function() return true end;
		API={};
		Loaded=false;
	});
	return true;
end;

_sg.Functions.GetModule=function(ModuleName)
	for i,v in next,_sg.Modules do
		if v.Name:lower():match(ModuleName:lower()) then
			return v;
		end;
	end;
	return false
end;

_sg.Functions.ModuleLoaded=function(ModuleName)
	local m=_sg.Functions.GetModule(ModuleName);
	if m==false then
		return false,"No module by \""..ModuleName.."\" exists";
	else
		return m.Loaded;
	end;
end;

_sg.Functions.LoadModule=function(ModuleName)
	for i,v in next,_sg.Modules do
		if v.Name:lower():match(ModuleName:lower()) then
			print("Found \""..v.Name.."\"!");
			if v.Loaded==false then
				print("Loading Module...");
				v.Loaded=true;
				local suc,res=pcall(function() v:Load() end);
				if not suc then
					print("An error occured when loading Module \""..v.Name.."\".");
					print("[ERROR] "..tostring(res));
					pcall(function() v:Unload() end);
					v.Loaded=false;
				else
					print("Module \""..v.Name.."\" was loaded successfully.");
				end;
			else
				print("Module \""..v.Name.."\" is already loaded.");
			end;
		end;
	end;
	print'Functions.LoadModule(): Task complete.';
end;

_sg.Functions.UnloadModule=function(ModuleName)
	for i,v in next,_sg.Modules do
		if v.Name:lower():match(ModuleName:lower()) then
			print("Found \""..v.Name.."\"!");
			if v.Loaded==true then
				print("Unloading Module...");
				v.Loaded=false;
				local suc,res=pcall(function() v:Unload() end);
				if not suc then
					print("An error occured when unloading Module \""..v.Name.."\".[");
					print("[ERROR] "..tostring(res));
					v.Loaded=true;
				else
					print("Module \""..v.Name.."\" was unloaded successfully.");
				end;
			else
				print("Module \""..v.Name.."\" was never loaded!");
			end;
		end;
	end;
end;

--[[	


		Server-Client Communications	
		
The communication system provides a secure, effective protocol for
transferring data between server and client. 


To interact with the server/client, a connection needs to be opened.
To open a connection, the server must ask the client to open a connection.
NOTE: This does not give the client ANY control after the connection
is opened.
]]--

_sg.Functions.OpenConnection=function(id,name,session)
	--TODO: Convert protocol for Client-sided use

	--TODO: Add "session" types for type of communication.
	local pack=_sg.Registry.REFERENCES.Sync:FindFirstChild(tostring(id));
	if not pack then
		return false,"Could not find Sync Package by Id \""..tostring(id).."\"!";
	end;
	local cq=pack:WaitForChild("Quene");
	local acnt=pack:FindFirstChild("Connections");
	
	--We have to ask the client to make a RemoteEvent for communication.
	Create'StringValue'{
		Name="Sess_"..#cq:GetChildren();
		Value="%sgreq>OpenCnt/ExeSess/"..name;
		Archivable=false;
		Parent=cq;
	};
	local cnt=acnt:WaitForChild(name);
	local api={
		Event=cnt;
		Connections={
			Server=cnt.OnClientEvent:connect(function(player,packet,hash)
				pcall(function() --Prevents errors from breaking connection.
					--If the packet can be decrypted, then we can assume to trust it.
					local pk=(function()
						if _sg.Functions.ModuleLoaded('SecurityModule')==false then
							_sg.Functions.LoadModule('SecurityModule');
						end
						local suc,res=pcall(function() return _sg.Functions.Decrypt(packet,"WXMZIllIpI0lBRTFdGKiggfviaqfvtHAU4HWI9BGk7XVEs0mM1") end);
						if suc then
							return res;
						else
							return false;
						end
					end)()
					if type(pk)=='string' then
						local tbl=JSONDecode(pk);
						local hsh=_sg.Functions.Hash(tbl.Packet);
						if hsh==hash then
							--The hash is verified. Executing code.
							local func,err=loadstring(tbl.Packet);
							if not err then
								--No errors, performing execution.
								--TODO: Add Environment Virtualization to test
								--for execution errors before executing on system.
								local suc,res=ypcall(function()
									func()
								end);
								--TODO: Add logging for success/error management.
								if not suc then
									print("Packet Execution Error: "..tostring(res));
								else
									print("Successfully executed packet.");
								end;
							else
								debug("An error ocurred during compile execution.");
								debug(err);return;
							end
						else
							debug("The hash failed to match.");return;
						end
					else
						debug("The packet did not decrypt properly.");
					end;
				end)
			end);
		};
		PushPacket=function(self,packet)
			if self==nil then error("Must call :PushPacket(), not .PushPacket()");end;
			local pak={
				Packet=packet;
			};
			local jsn=JSONEncode(pak);
			if _sg.Functions.ModuleLoaded('SecurityModule')==false then
				_sg.Functions.LoadModule('SecurityModule');
			end
			local data=_sg.Functions.Encrypt(jsn,"WXMZIllIpI0lBRTFdGKiggfviaqfvtHAU4HWI9BGk7XVEs0mM1");
			local hsh=_sg.Functions.Hash(packet);
			self.Event:FireAllClients(data,hsh);
			return true;
		end;
		Disconnect=function(self)
			if self==nil then error("Must call :Disconnect(), not .Disconnect()");end;
			for i,v in next,self.Connections do
				pcall(function() v:disconnect() end);
			end;
			return true;
		end;
	};
	return api;
end

--Since we will not be leaving (Remote/Bindable)Events/Functions lying
--around, we must give the client a StringValue containing request info.

--Example: %sgreq>OpenCnt/ExeSess


--Internal Module Creation

_sg.Functions.CreateModule("SecurityModule",function(self)
	local MOD = 2^32
	local MODM = MOD-1
	 
	local function memoize(f)
		local mt = {}
		local t = setmetatable({}, mt)
		function mt:__index(k)
			local v = f(k)
			t[k] = v
			return v
		end
		return t
	end
	 
	local function make_bitop_uncached(t, m)
		local function bitop(a, b)
			local res,p = 0,1
			while a ~= 0 and b ~= 0 do
				local am, bm = a % m, b % m
				res = res + t[am][bm] * p
				a = (a - am) / m
				b = (b - bm) / m
				p = p*m
			end
			res = res + (a + b) * p
			return res
		end
		return bitop
	end
	 
	local function make_bitop(t)
		local op1 = make_bitop_uncached(t,2^1)
		local op2 = memoize(function(a) return memoize(function(b) return op1(a, b) end) end)
		return make_bitop_uncached(op2, 2 ^ (t.n or 1))
	end
	 
	local bxor1 = make_bitop({[0] = {[0] = 0,[1] = 1}, [1] = {[0] = 1, [1] = 0}, n = 4})
	 
	local function bxor(a, b, c, ...)
		local z = nil
		if b then
			a = a % MOD
			b = b % MOD
			z = bxor1(a, b)
			if c then z = bxor(z, c, ...) end
			return z
		elseif a then return a % MOD
		else return 0 end
	end
	 
	local function band(a, b, c, ...)
		local z
		if b then
			a = a % MOD
			b = b % MOD
			z = ((a + b) - bxor1(a,b)) / 2
			if c then z = bit32_band(z, c, ...) end
			return z
		elseif a then return a % MOD
		else return MODM end
	end
	 
	local function bnot(x) return (-1 - x) % MOD end
	 
	local function rshift1(a, disp)
		if disp < 0 then return lshift(a,-disp) end
		return math.floor(a % 2 ^ 32 / 2 ^ disp)
	end
	 
	local function rshift(x, disp)
		if disp > 31 or disp < -31 then return 0 end
		return rshift1(x % MOD, disp)
	end
	 
	local function lshift(a, disp)
		if disp < 0 then return rshift(a,-disp) end
		return (a * 2 ^ disp) % 2 ^ 32
	end
	 
	local function rrotate(x, disp)
		x = x % MOD
		disp = disp % 32
		local low = band(x, 2 ^ disp - 1)
		return rshift(x, disp) + lshift(low, 32 - disp)
	end
	 
	local k = {
		0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
		0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
		0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
		0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
		0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
		0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
		0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
		0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
		0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
		0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
		0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
		0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
		0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
		0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
		0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
		0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
	}
	 
	local function str2hexa(s)
		return (string.gsub(s, ".", function(c) return string.format("%02x", string.byte(c)) end))
	end
	 
	local function num2s(l, n)
		local s = ""
		for i = 1, n do
			local rem = l % 256
			s = string.char(rem) .. s
			l = (l - rem) / 256
		end
		return s
	end
	 
	local function s232num(s, i)
		local n = 0
		for i = i, i + 3 do n = n*256 + string.byte(s, i) end
		return n
	end
	 
	local function preproc(msg, len)
		local extra = 64 - ((len + 9) % 64)
		len = num2s(8 * len, 8)
		msg = msg .. "\128" .. string.rep("\0", extra) .. len
		assert(#msg % 64 == 0)
		return msg
	end
	 
	local function initH256(H)
		H[1] = 0x6a09e667
		H[2] = 0xbb67ae85
		H[3] = 0x3c6ef372
		H[4] = 0xa54ff53a
		H[5] = 0x510e527f
		H[6] = 0x9b05688c
		H[7] = 0x1f83d9ab
		H[8] = 0x5be0cd19
		return H
	end
	 
	local function digestblock(msg, i, H)
		local w = {}
		for j = 1, 16 do 
			w[j] = s232num(msg, i + (j - 1)*4) 
		end
		for j = 17, 64 do
			local v = w[j - 15]
			local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
			v = w[j - 2]
			w[j] = w[j - 16] + s0 + w[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
		end
		local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
		for i = 1, 64 do
			local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
			local maj = bxor(band(a, b), band(a, c), band(b, c))
			local t2 = s0 + maj
			local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
			local ch = bxor (band(e, f), band(bnot(e), g))
			local t1 = h + s1 + ch + k[i] + w[i]
			h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
		end
		H[1] = band(H[1] + a)
		H[2] = band(H[2] + b)
		H[3] = band(H[3] + c)
		H[4] = band(H[4] + d)
		H[5] = band(H[5] + e)
		H[6] = band(H[6] + f)
		H[7] = band(H[7] + g)
		H[8] = band(H[8] + h)
	end

	self.API.Hash = function(msg)
		msg = preproc(msg, #msg)
		local H = initH256({})
		for i = 1, #msg, 64 do digestblock(msg, i, H) end
		return str2hexa(num2s(H[1], 4) .. num2s(H[2], 4) .. num2s(H[3], 4) .. num2s(H[4], 4) .. num2s(H[5], 4) .. num2s(H[6], 4) .. num2s(H[7], 4) .. num2s(H[8], 4))
	end
	
	self.API.Encrypt=function(message, key)
		local key_bytes
		if type(key) == "string" then
			key_bytes = {}
			for key_index = 1, #key do
				key_bytes[key_index] = string.byte(key, key_index)
			end
		else
			key_bytes = key
		end
		local message_length = #message
		local key_length = #key_bytes
		local message_bytes = {}
		for message_index = 1, message_length do
			message_bytes[message_index] = string.byte(message, message_index)
		end
		local result_bytes = {}
		local random_seed = 0
		for key_index = 1, key_length do
			random_seed = (random_seed + key_bytes[key_index] * key_index) * 37789 + 60061
			random_seed = (random_seed - random_seed % 256) / 256 % 65536
		end
		for message_index = 1, message_length do
			local message_byte = message_bytes[message_index]
			for key_index = 1, key_length do
				local key_byte = key_bytes[key_index]
				local result_index = message_index + key_index - 1
				local result_byte = message_byte + (result_bytes[result_index] or 0)
				if result_byte > 255 then
					result_byte = result_byte - 256
				end
				result_byte = result_byte + key_byte
				if result_byte > 255 then
					result_byte = result_byte - 256
				end
				random_seed = (random_seed * 37789 + 60061) % 65536
				result_byte = result_byte + (random_seed - random_seed % 256) / 256
				if result_byte > 255 then
					result_byte = result_byte - 256
				end
				result_bytes[result_index] = result_byte
			end
		end
		local result_buffer = {}
		local result_buffer_index = 1
		for result_index = 1, #result_bytes do
			local result_byte = result_bytes[result_index]
			result_buffer[result_buffer_index] = string.format("%02x", result_byte)
			result_buffer_index = result_buffer_index + 1
		end
		return table.concat(result_buffer)
	end;
	self.API.Decrypt=function(cipher, key)
		local key_bytes
		if type(key) == "string" then
			key_bytes = {}
			for key_index = 1, #key do
				key_bytes[key_index] = string.byte(key, key_index)
			end
		else
			key_bytes = key
		end
		local cipher_raw_length = #cipher
		local key_length = #key_bytes
		local cipher_bytes = {}
		local cipher_length = 0
		local cipher_bytes_index = 1
		for byte_str in string.gmatch(cipher, "%x%x") do
			cipher_length = cipher_length + 1
			cipher_bytes[cipher_length] = tonumber(byte_str, 16)
		end
		local random_bytes = {}
		local random_seed = 0
		for key_index = 1, key_length do
			random_seed = (random_seed + key_bytes[key_index] * key_index) * 37789 + 60061
			random_seed = (random_seed - random_seed % 256) / 256 % 65536
		end
		for random_index = 1, (cipher_length - key_length + 1) * key_length do
			random_seed = (random_seed * 37789 + 60061) % 65536
			random_bytes[random_index] = (random_seed - random_seed % 256) / 256
		end
		local random_index = #random_bytes
		local last_key_byte = key_bytes[key_length]
		local result_bytes = {}
		for cipher_index = cipher_length, key_length, -1 do
			local result_byte = cipher_bytes[cipher_index] - last_key_byte
			if result_byte < 0 then
				result_byte = result_byte + 256
			end
			result_byte = result_byte - random_bytes[random_index]
			random_index = random_index - 1
			if result_byte < 0 then
				result_byte = result_byte + 256
			end
			for key_index = key_length - 1, 1, -1 do
				cipher_index = cipher_index - 1
				local cipher_byte = cipher_bytes[cipher_index] - key_bytes[key_index]
				if cipher_byte < 0 then
					cipher_byte = cipher_byte + 256
				end
				cipher_byte = cipher_byte - result_byte
				if cipher_byte < 0 then
					cipher_byte = cipher_byte + 256
				end
				cipher_byte = cipher_byte - random_bytes[random_index]
				random_index = random_index - 1
				if cipher_byte < 0 then
					cipher_byte = cipher_byte + 256
				end
				cipher_bytes[cipher_index] = cipher_byte
			end
			result_bytes[cipher_index] = result_byte
		end
		local result_characters = {}
		for result_index = 1, #result_bytes do
			result_characters[result_index] = string.char(result_bytes[result_index])
		end
		return table.concat(result_characters)
	end;
	
	--Connections
	
	_sg.Functions.Encrypt=function(message,key)
		if _sg.Functions.ModuleLoaded('SecurityModule')==false then
			error("Functions.Encrypt(): This function depends on the Module \"SecurityModule\" to be loaded.");
		else
			return _sg.Functions.GetModule('SecurityModule').API.Encrypt(message,key);
		end;
	end;
	
	_sg.Functions.Decrypt=function(message,key)
		if _sg.Functions.ModuleLoaded('SecurityModule')==false then
			error("Functions.Decrypt(): This function depends on the Module \"SecurityModule\" to be loaded.");
		else
			return _sg.Functions.GetModule('SecurityModule').API.Decrypt(message,key);
		end;
	end;
	
	_sg.Functions.Hash=function(message)
		if _sg.Functions.ModuleLoaded("SecurityModule")==false then
			error("Functions.Hash(): This function depends on the Module \"SecurityModule\" to be loaded.");
		else
			return _sg.Functions.GetModule("SecurityModule").API.Hash(message);
		end;
	end;
	return true;
end,
function(self)
	for i,v in next,self.API do
		self.API[i]=nil;
	end;
	self.Loaded=false;
	return true;
end,nil);

_sg.Functions.CreateModule("StringAPI",function(self) --Since this is a "expandable" API, we will make it a Module.
	self.API.Explode=function(div,str)
		if div=='' then return false end;
		local pos,arr=0,{};
		for st,sp in function() return str:find(div,pos,true) end do
			table.insert(arr,str:sub(pos,st-1));
			pos=sp+1;
		end;
		table.insert(arr,str:sub(pos));
		return arr;
	end;
	
	--Connections
	
	_sg.Functions.Explode=function(div,str)
		if _sg.Functions.ModuleLoaded"StringAPI"==false then
			error("Functions.Explode(): This function depends on the Module \"StringAPI\" to be loaded.");
		else
			return _sg.Functions.GetModule("StringAPI").API.Explode(div,str);
		end;
	end;
end,function(self)
	for i,v in next,self.API do
		self.API[i]=nil;
	end;
	return true;
end,nil);


--Load required Modules
_sg.Functions.LoadModule('SecurityModule');
_sg.Functions.LoadModule("StringAPI");

_G.sg=_sg; --WARNING: HUGE SECURITY RISK! REMOVE ON LATER RELEASES!


debug("ClientGuard has been loaded.");

--SafeSync Setup
debug("Configuring SafeSync on the client...");
if _sg.Registry.REFERENCES.Sync:FindFirstChild(_sg.LocalPlayer.userId) then
	pcall(function() _sg.Registry.REFERENCES.Sync[_sg.LocalPlayer.userId]:Destroy() end);
end;

debug("Creating initial packages...");
_sg.Registry.REFERENCES.USERSYNC=Create'Backpack'{
	Name=tostring(_sg.LocalPlayer.userId);
	Parent=_sg.Registry.REFERENCES.Sync;
	Archivable=false;
};
debug("Created initial \"USERSYNC\" package");
Create'Backpack'{
	Name="Quene";
	Parent=_sg.Registry.REFERENCES.USERSYNC;
	Archivable=false;
};
debug("Created \"Quene\" package");
Create'Backpack'{
	Name="Connections";
	Parent=_sg.Registry.REFERENCES.USERSYNC;
	Archivable=false;
};
debug("Created \"Connections\" package");
Create'Backpack'{
	Name="ServiceEvents";
	Parent=_sg.Registry.REFERENCES.USERSYNC;
	Archivable=false;
};
debug("Created \"ServiceEvents\" package");
debug("Sync package created successfully.");
debug("Creating ChatConnection...");
Create'RemoteEvent'{
	Name="ChatConnection";
	Parent=_sg.Registry.REFERENCES.USERSYNC.ServiceEvents;
	Archivable=false;
}
debug("Created \"ChatConnection\".");
debug("Connecting events...");

_sg.LocalPlayer.Chatted:connect(function(msg)
	pcall(function() _sg.Registry.REFERENCES.USERSYNC.ServiceEvents.ChatConnection:FireServer(msg);end);
end);

debug("Connected \"ChatConnection\" Event to client.");

--Connection Processing Management [Rewrite 2]
--This includes ability for client to fire back data.
_sg.Registry.REFERENCES.USERSYNC.Quene.DescendantAdded:connect(function(c)
	if c.Parent~=_sg.Registry.REFERENCES.USERSYNC.Quene then
		debug("\""..c.Name.."\" is not a direct descendant of the quene. Removing...");
		pcall(function() c:Destroy();end);return;
	else
		if c.ClassName~='StringValue' then
			debug("\""..c.Name.."\" is not a valid StringValue. Removing...");
			pcall(function() c:Destroy();end);return;
		else
			--Alright, you are a direct descendant of the quene and a valid StringValue.
			debug("\""..c.Name.."\" passed check requirements. Attempting to decrypt value...");
			local pk=(function()
				if _sg.Functions.ModuleLoaded'SecurityModule'==false then
					_sg.Functions.LoadModule'SecurityModule';
				end;
				local suc,res=pcall(function() return _sg.Functions.Decrypt(c.Value,'test');end);
				if suc then
					debug("Success: "..tostring(res));
					return res;
				else
					debug("Failed: "..tostring(res));
					return false;
				end;
			end)();
			if type(pk)=='string' then
				debug("Decrypt was successful. Checking value for request...");
				local conn;
				local api;
				local args=_sg.Functions.Explode("/",pk);
				if args[1]:lower()=='%sgreq' then
					if args[2]:lower()=='intsess' then
						debug("Server is requesting an interactive session.");
						debug("Preparing connection...");
						conn=Create'RemoteEvent'{
							Name=args[3];
							Archivable=false;
							Parent=_sg.Registry.REFERENCES.USERSYNC.Connections;
						};
						debug("Created RemoteEvent. Creating API...")
						api={
							Event=conn;
							PushPacket=function(self,packet)
								if self==nil then error("Must call :PushPacket(), not .PushPacket()");end;
								local pak={
									Packet=packet;
								};
								local jsn=JSONEncode(pak);
								if _sg.Functions.ModuleLoaded'SecurityModule'==false then
									_sg.Functions.LoadModule'SecurityModule';
								end;
								local data=_sg.Functions.Encrypt(jsn,"WXMZIllIpI0lBRTFdGKiggfviaqfvtHAU4HWI9BGk7XVEs0mM1");
								local hsh=_sg.Functions.Hash(packet);
								self.Event:FireServer(data,hsh);
								debug("Pushed packet to server.");
								return true;
							end;
						}
						debug("API created. Creating Client connection...");
						conn.OnClientEvent:connect(function(packet,hash)
							pcall(function() --Prevents errors from breaking connection.
								--If the packet can be decrypted, then we can assume to trust it.
								debug("Got packet data from server. Attempting to decrypt.");
								local pk=(function()
									if _sg.Functions.ModuleLoaded'SecurityModule'==false then
										_sg.Functions.LoadModule'SecurityModule';
									end
									local suc,res=pcall(function() return _sg.Functions.Decrypt(packet,"WXMZIllIpI0lBRTFdGKiggfviaqfvtHAU4HWI9BGk7XVEs0mM1") end);
									if suc then
										return res;
									else
										return false;
									end
								end)()
								debug("Checking results...");
								if type(pk)=='string' then
									debug("Decrypt success. Converting to table.");
									local tbl=JSONDecode(pk);
									local hsh=_sg.Functions.Hash(tbl.Packet);
									debug("Verifying hash...");
									if hsh==hash then
										--The hash is verified. Executing code.
										debug("Hash verified. Compiling code.");
										local func,err=loadstring(tbl.Packet);
										if not err then
											debug("Code compiled. Executing...");
											--No errors, performing execution.
											--Alright, we are going to mess around with the environment.
											
											getfenv(func).API=api;											
											local suc,res=pcall(function()
												func()
											end);
											--TODO: Add logging for success/error management.
											if not suc then
												print("Packet Execution Error: "..tostring(res));
											else
												print("Successfully executed packet.");
											end;
										else
											debug("Compile error: "..tostring(err));return;
										end;
									else
										debug("Hash verification failed. Ignoring packet.");return;
									end;
								else
									print("Packet failed to decrypt properly. Ignoring packet.");
								end;
							end)
						end)
						--TODO: Finish Connection API for client.
						debug("Client connection established.");
						debug("Client successfully connected for an interactive session.");
					else
						debug("Option not supported at this time.");return;
					end;
				else
					debug("Option not supported at this time.");return;
				end;
			else
				debug("Package did not decrypt properly. Ignoring request.");
				pcall(function() c:Destroy();end);return;
			end;
		end;
	end;
end);
debug("Events connected.");
debug("Client setup stages configured. System ready.");