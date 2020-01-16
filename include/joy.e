-- various functions that brought me joy
include file.e

type nonemptyseq( sequence s)
	return length(s)
end type

global type non_empty_sequence( sequence s )
	return length(s)
end type

global function min(nonemptyseq s)
	object m
	m = s[1]
	for i = 2 to length(s) do
		if compare(s[i],m) < 0 then
			m = s[i]
		end if
	end for
	return m
end function

global function max(nonemptyseq s)
	object m
	m = s[1]
	for i = 2 to length(s) do
		if compare(s[i],m) > 0 then
			m = s[i]
		end if
	end for
	return m
end function
	
-- This returns true if m is between a and z or m is identical to either a or z.
-- This is useful because it changes a relatively complex condition like this:
--      if 1 <= ya + f * l and ya + f * l <= sqroot  
-- to this:
-- 	between_inclusive( 1, ya + f * l, sqroot )
global function between_inclusive(object a, object m, object z)
    return compare(a,m)<=0 and compare(m,z)<=0
end function

-- This returns true if m is between a and z but identical to neither a nor z.
global function between_exclusive(object a, object m, object z)
    return compare(a,m)<0 and compare(m,z)<0
end function

-- centre a string in whitespace and return it.
global function sputs_centre( sequence s, integer len )
    while length(s) + 2 <= len do
	s = " " & s & " "
    end while
    if length(s) < len then
	s &= " "
    end if
    return s[1..len]
end function

global function sum( sequence s ) 
	object o
	o = 0
	for i = 1 to length(s) do o += s[i] end for
	return o
end function
	
global type string_of_atoms(object o)
    if atom(o) then
	return 0
    end if
    for i = 1 to length(o) by 1 do
	if sequence(o[i]) then
	    return 0
	end if
    end for
    return 1
end type

global type string_of_integers(object o)
    if atom(o) then
	return 0
    end if
    for i = 1 to length(o) do
	if not integer(o[i]) then
	    return 0
	end if
    end for
    return 1
end type



global type string32bit(object o)
	return string_of_integers(o)
end type

global type string16bit(object o )
	if not string_of_integers(o) then return 0 end if
	return string_of_integers(o) and 
	sum(o<#10000)=length(o) and sum(0<=o)=length(o) 
end type

-- Yes, ASCII characters are only 7 bits long.  Look it up.
global type stringASCII( object o )
	if not string_of_integers(o) then return 0 end if
	return  sum( 0 <= o ) = length( o ) 
	and sum( o < 128 ) = length(o)
end type

global type ASCIIstring( object o ) return stringASCII( o ) end type

-- you might use this type to show you are using unportable strings
global type string8bit(sequence o)
	if not string_of_integers(o) then return 0 end if
	return string_of_integers(o) and sum( o < 256 ) = length(o)
	and sum( 0 < o ) = length(o)
end type

global type string_of_sequences( object x )
	if atom(x) then return 0 end if
	for i = 1 to length(x) do if atom(x[i]) then return 0 end if end for
	return 1
end type

-- returns the same sequence with an element missing from the end,
-- in the case of an empty sequence it returns an empty sequence
global function chop( sequence s )
	integer len
	len = length(s)
	if len=0 then
		return s
	else
		return s[1..len-1]
	end if
end function

-- returns the last element of the sequence
global function end_of( nonemptyseq s )
	return s[length(s)]
end function

-- returns the same sequence with the last element changed to o,
-- becareful not to send empty sequences into this.
-- Instead of foo[length(foo)] = baz you can do foo = set_end_of(foo,baz)
global function set_end_of( nonemptyseq s, object o )
	s[length(s)]=o
	return s
end function

-- returns the same with out the newline character on the end of a string
global function chomp( string_of_atoms o )
    if length(o) and o[length(o)]='\n' then
	return o[1..length(o)-1]
    end if
    return o
end function


-- joins a sequence of sequences together with foo
-- returns strings[1] & foo & ... foo & strings[length(strings)]
-- the following will return the "C:\Documents and Settings\User":
-- join("\\",{"C:","Documents and Settings","User"})
global function join(object foo, string_of_sequences strings)
    sequence ret
    ret = {}
    if length(strings) then
	ret = strings[1]
    end if
    for i = 2 to length(strings) do
	ret = ret & foo & strings[i] 
    end for
    return ret
end function


-- returns foo & strings[1] & foo & strings[2] & ... strings[length(strings)]
--  & foo
-- like foo & join(foo, strings ) & foo.
global function enclose(object foo, sequence strings )
    return foo & join(foo, strings) & foo
end function


-- Does the opposite of join:
-- Finds occurences of needle in haystack and returns a sequence 
-- of sequences where each entry is before or after an occurance of needle.
--  NOTE: To split a path up you need to pass "/"  as foo instead of '/'!.
-- Example: 
-- The following will return the sequence :{"C:","Documents and Settings","User"}
-- split( "\\", "C:\Documents and Settings\User" )
global function sequence_split(sequence needle, sequence haystack )
    integer loc
    object ret
    --printf(1,"\nsplit(",{})print(1,needle)puts(1,",")print(1,haystack)puts(1,")=")
    loc = match(needle, haystack )
    --printf(1,"loc=%d\n",{loc})
    if loc then
	ret = {haystack[1..loc-1]} & sequence_split(needle, haystack[loc+1..length(haystack)])
    else
	ret = {haystack}
    end if
    --print(1,ret)    puts(1,"\n")
    return ret
end function

-- local short name for internal calls inside joy.e
function split(object a, object b)
    return sequence_split(a,b)
end function

-- changes an ASCII string to upper case.  It will not covert
-- characters that are not ASCII.
global function toupper_ASCII( object x )
    if atom(x) then
	if between_inclusive('a',x,'z') then
	    return x - 'a' + 'A'
	else
	    return x
	end if
    end if
    for i = 1 to length(x) do
	if between_inclusive('a',x[i],'z') then
	    x[i] += ('A'-'a')
	end if
    end for
    return x
end function

-- changes an ASCII string to lower case.  It only only
-- convert ASCII strings
global function tolower_ASCII(object x)
     if atom(x) then
     	 if between_inclusive('A',x,'Z') then
	 	x += 'a' - 'A'
	end if
	return x
    end if
    for i = 1 to length(x) do
	if compare('A',x[i])<=0 and compare(x[i],'Z')<=0 then
	    x[i] += ('a'-'A')
	end if
    end for
    return x
end function

-- returns n!.
function factorial(integer n)
    integer m
    if n < 1 then
	return 1
    end if
    m = 1
    while n > 1 do
	m *= n
	n -= 1
    end while
    return m
end function

-- returns count objects from the sequence S at random.
global function choose_noreplacement( integer count, object S )
    sequence outset
    object choice
    if 0 > count  then
    	return 0
    end if
    -- here count >= 0
    if sequence(S) then
	if count > length(S) then
	    -- there are no sets of count size that we can take from
	    -- a smaller set.
	    return 0
	end if
	-- here 0 <= count < length(S)
	outset = {}
	while count > 0 do
	    choice = rand(length(S))
	    outset = append( outset, S[choice] )
	    S = S[1..choice-1] & S[choice+1..length(S)]
	    count -= 1
	end while
	return outset
    elsif integer(S) then	
	-- In this case it will return the number of such possible
	-- sets by choosing count from a set of length S.
	if not between_inclusive(0,count,S) then
	  return 0
	end if
	return factorial(S)/(factorial(count)*factorial(S-count))
    else
    	-- here S is a non-integer number
	-- not defined.  Maybe you should call a Gamma function?
	return -1
    end if
end function

type pair_of_integers( sequence p ) return length(p)=2 and integer(p[1]) and integer(p[2]) end type

-- A sequence where each member could be an index for another sequence.
type index_sequence(sequence s)
	for i = 1 to length(s) do
		if not integer(s[i]) then return 0 end if
	end for
	return 1
end type

-- s and i a sequence of indicies... returns s[i[1]]....[i[length(i)]
-- if i is empty it returns s.
-- Each i[j] must be an index of s[i[1]][...][i[j-1]] for j > 1 and i[1] must be an index of s or
-- the program will crash.
-- This is useful especially for getting information out of structures returned without putting the
-- return value into a sequence first.
global function index_get( sequence s, index_sequence i )
	while length(i) > 2 and string_of_integers(i) do
		s = s[i[1]][i[2]][i[3]]
		i = i[4..length(i)]
	end while
	if length(i) = 2 then
		return s[i[1]][i[2]]
	elsif length(i) = 1 then
		return s[i[1]]
	elsif length(i) = 0 then
		return s        
	end if
end function


-- index_set( s, {3,4}, 8.777 ) is equivalent to: s[3][4] = 8.777 
global function index_set( sequence s, index_sequence i, object v )
	if length(i) = 0 then
		return v
	elsif length(i) = 1 then
		s[i[1]] = v
		return s
	elsif length(i) = 2 then
		s[i[1]][i[2]] = v
		return s
	else
		s[i[1]][i[2]] = index_set( s[i[1]][i[2]], i[3..length(i)], v )
	end if
	return s          
end function

-- same as find but search from the end to the beginning
global function rfind(object o, sequence s)
    for i = length(s) to 1 by -1 do
	if equal(o,s[i]) then
	    return i
	end if
    end for
    return 0
end function

--constant ext = { ".ex", ".exw", ".exu" }
constant ps  = { ';', ';', ':' }
constant slash = { '\\', '\\', '/' }
constant SLASH = slash[platform()]

-- The next are like slash and split with the
-- OS specific file path seperators entered for you
global function slashjoin(string_of_sequences s)
	return join({SLASH},s)
end function

global function slashsplit(sequence s)
	return sequence_split({SLASH},s)
end function

-- Seach the string var for locations the file named by s might be.
-- var is seperated by semicolons in Windows and colons for other 
-- platforms.  
--
-- For example.  Under UNIX you can find the location of grep using:
-- which( "grep", getenv("PATH") )
--
-- Under MSDOS find the location of fdisk.exe with:
-- which("fdisk.exe", getenv("PATH") )
--
-- Under any platform find the location of  a EUPHORIA library file with:
-- which("unicode.ew", getenv("EUINC") )
global function which( string8bit s, object V )
    sequence dirs
    if not stringASCII(V) then
    	return ""
    end if
    dirs = split({ps[platform()]}, V )
    for i = 1 to length(dirs) do
	if sequence(dir(dirs[i] & SLASH & s)) then
	    return dirs[i] & SLASH & s
	end if
    end for
    return s
end function

-- Returns consequtive number sequence starting from 1:
-- example: make_id_permutation(6) = { 1,2,3,4,5,6 }
global function make_id_permutation(integer k)
    sequence result
    result = repeat( 0, k )
    for i = 1 to k  do
	result[i] = i
    end for
    return result
end function

-- perl style || operator.  Take a sequence and perl or them together.
-- the result is the first non-zero entry in s is returned.  This is useful for constant declarations.
global function ior( non_empty_sequence s )
	object result
	result = s[1]
	while length(s) do
		if compare(s[1],0)!=0 then 
			return result
		end if
		s = s[2..length(s)]
	end while
	return 0
end function

-- returns a the sequence s with all of the members of s equal to x removed.
-- For example:  remove-objects( {5,4,3,2,4,5,1,{5} }, 5 ) = { 4,3,2,4,1,{5}}
global function remove_objects( sequence s, object x )
	integer p
	p = find( x, s )
	while p do
		s = s[1..p-1] & s[p+1..length(s)]
		p = find( x, s )
	end while
	return s
end function

-- returns a sequence without any 0 members:
-- Useful with find and min/max functions:
-- i = min( remove_0s( { find('.',s), match("foo",s), length(s) + 1 } ) )
-- i is the first occurance of either ., foo or the length of s + 1.
global function remove_0s( sequence s )
	return remove_objects( s, 0 )
end function