without trace

--seqop.e

--This file contains misc, sequence, find, and file operations.

--May 3, 2004  by Emlyn Merlyn

--This reads a file by its lines and return the result.
global function read_file(sequence name)
    integer fn   --fn = file number
    object line --The current line being read
    sequence lines
    
    lines = {}
    fn = open(name,"r") --Open the file
    if fn = -1 then
	return -1
    end if

    --Read the file
	line = gets(fn)  --Get either a line, or a -1
    while sequence(line) do
        if line[length(line)] = '\n' then
            line = line[1..length(line)-1]
        end if
	    lines = append(lines,line)  --Add the line to sequence lines
    	line = gets(fn)  --Get either a line, or a -1
    end while

    close(fn)    
    return lines
end function

--This routine removes all elements less than index.
global function remove_less_than(sequence seq, integer index)
    if index <= 1 then
	--Nothing happens
    elsif index > length(seq) then
	seq = {}
    else
	seq = seq[index..length(seq)]
    end if
    
    return seq
end function

--This routine removes all elements greater than index.
global function remove_greater_than(sequence seq, integer index)
    if index > length(seq) then
	--Nothing happens
    elsif index < 1 then
	seq = {}
    else
	seq = seq[1 .. index]
    end if
    
    return seq
end function

--Find an object in a sequence at a point from integer where
function find_greater(object needle, sequence haystack, integer from_pos)
    integer n
    haystack = remove_less_than(haystack,from_pos)
    n = find(needle,haystack)
    if n != 0 then
    	n += from_pos - 1
    end if
    
    return n
end function

--Find an object in a sequence at a point up to integer from_pos
function find_less(object needle, sequence haystack, integer from_pos)
    haystack = remove_greater_than(haystack,from_pos)
    
    return find(needle,haystack)
end function

--This routine removes a element from a sequence using sequence slicing.
--Pass the input to seq and the element number to remove to index
--When 'index' is "$" it removes the last element.
global function remove(sequence seq, object index)
    if equal(index, "$") then
        index = length(seq)
    end if
    if index < 1 or index > length(seq) then
        return seq
    end if

	return seq[1..index-1]&seq[index+1..length(seq)] --Remove it!
end function

--This routine removes all elements of a sequence that match object o.
global function remove_subscripts(sequence seq, object o)
    integer removing
    removing = 1
    
    while removing <= length(seq) do
    	if equal(seq[removing],o)  then  --If they match...
    	    seq = remove(seq,removing)        --...remove the subscript.
    	    --Don't add onto removing because the position is still the same.
    	else
    	    removing = removing + 1 --Don't remove it, move on.
    	end if
    end while
    
    return seq
end function

--This routine returns a list of all different members of a sequence
global function members(sequence seq)
    sequence m --For members
    m = {}
    
    for counter = 1 to length(seq) do
    	if find(seq[counter],m) = 0 then -- If it's new...
    	    m = append(m,seq[counter])   -- ...add it to the list
    	end if
    end for
    
    return m
end function

--This replaces all o1's in seq with o2's
global function replace_all(sequence seq, object o1, object o2)
    --o1 is what to find
    --o2 is what to replace all occurences of o1 with.

    for counter = 1 to length(seq) do
    	if equal(seq[counter],o1) then  -- If it equals o1...
    	    seq[counter] = o2                 -- ...replace it with o2
    	end if
    end for

    return seq
end function

-- win32lib.ew

global procedure LimitText(integer id, integer pSize )
    limitText(id,pSize)
end procedure

global function Current_Directory()
    return getCurrentDirectory()
end function

global procedure Set_Directory(sequence x)
    setCurrentDirectory(x)
end procedure

global procedure setSize(integer win, object w, object h)
    setCtlSize(win, w, h)
end procedure

------------------------------------------------------------------------------
global function readXpm( sequence fName )

    -- read a file into a sequence
    -- removes line feeds

    integer handle, at
    sequence file
    object data

    file = {}

    -- open the file
    handle = w32FileOpen( fName, "r" )
    if handle = -1 then
    -- error opening file
        return XPM_ERR_FILE
    end if

    -- check header
    data = gets( handle )
    if not match( "/* XPM */", data ) then
        return XPM_ERR_HEADER
    end if

    -- read to eof
    while 1 do

        -- read a line
        data = gets( handle )

        -- eof?
        if integer( data ) then
            exit
        end if

        -- remove line feed, if any
        if data[length(data)] = '\n' then
            data = data[1..length(data)-1]
        end if

        -- blank line?
        if length( data ) = 0 then
            -- ignore

        -- not quoted?
        elsif data[1] != '"' then
            -- ignore

        else

            -- first quote
            at = find( '"', data )
            data = data[at+1..length(data)]

            -- last quote
            at = find( '"', data )
            data = data[1..at-1]

            -- add to file
            file = append( file, data )

        end if

    end while

    -- close the file
    close( handle )

    -- reads an XPM from disk, returns bitmap
    return xpmToEuBmp( file )

end function

integer fn_debug fn_debug = 1
global constant w32DebugLog = -1772
global procedure wDebug(sequence pData)

    if length(pData) = 2 then
        if sequence(pData[1]) then
            printf(fn_debug, pData[1], pData[2])
        else
            if pData[1] = w32DebugLog then
                if fn_debug > 2 then
                    close(fn_debug)
                end if
                fn_debug = w32FileOpen(pData[2], "w")
                if fn_debug < 0 then
                    fn_debug = 1
                end if
            end if
        end if
    else
        puts(fn_debug, w32ToString(pData))
        if not equal(pData[length(pData)], '\n') then
            puts(fn_debug, '\n')
        end if
    end if
end procedure

constant TTDT={TTDT_INITIAL,TTDT_AUTOPOP,TTDT_RESHOW}

--/topic System Attributes
--/func setTooltipTiming(integer id,object timings)
--/desc Possibly sets the timings for all the tooltips the tooltip conrol /i id manages.
--/ret The previous or current values.
-- Three durations are associated to a tooltip. If /i timings is a sequence, its elements are:
--/li the initial delay (in miliseconds) during which the mouse must hover for the tooltip window to pop up;
--/li the autopop duration, in miliseconds: this is the time during which the tooltip window remeinas visible;
--/li the reshow delay, in milliseconds: delay from a change of hovered upon control and
-- the next popping up of a tooltip window for the new target control.
-- For each of these, use w32False to set to default and w32GetValue to return without setting.
-- If /i timings is a positive integer, the initial delay is set to the supplied value, the autopop duration to ten times the initial delay, and the reshow delay to a fifth of the initial delay. Using w32False will return the values to default - 500, 5000 and 100ms respetivelly. Using w32GetValue will skip any setting.
-- At any rate, the three timings are returned.

global function setTooltipTimings(integer id,object timings)
    sequence result

    if ctrl_Type[id]!=ToolTip then
        return {}
    end if
    result={sendMessage(id,TTM_GETDELAYTIME,TTDT_INITIAL,0),
            sendMessage(id,TTM_GETDELAYTIME,TTDT_AUTOPOP,0),
            sendMessage(id,TTM_GETDELAYTIME,TTDT_RESHOW,0)}
    if atom(timings) then
        if timings>0 then
            VOID=sendMessage(id,TTM_SETDELAYTIME,TTDT_AUTOMATIC,timings)
        elsif timings!=w32GetValue then
            VOID=sendMessage(id,TTM_SETDELAYTIME,TTDT_AUTOMATIC,-1)
        end if
    else
        for i=1 to 3 do
            if timings[i]>0 then
                VOID=sendMessage(id,TTM_SETDELAYTIME,TTDT[i],timings[i])
            elsif timings[i]!=w32GetValue then
                VOID=sendMessage(id,TTM_SETDELAYTIME,TTDT[i],-1)
            end if
        end for
    end if
    return result
end function

global procedure w32release_all_mem()
-- No longer needed.
end procedure

-- w32tk.e

global function abs( object a )
  return w32abs(a)
end function

global function acquire_mem( atom pOwner, object pData )
  return w32acquire_mem (pOwner, pData)
end function

global function allot( object pDataType )
  return w32allot(pDataType)
end function

global function allotted_handle(sequence pHandle)
  return w32allotted_handle(pHandle)
end function

global function allotted_sofar()
  return w32allotted_sofar()
end function

global function allotted_size()
  return w32allotted_size()
end function

global function new_memset()
  return w32new_memset()
end function

global procedure manage_mem( atom pOwner, atom pAddr )
  w32manage_mem(pOwner, pAddr)
end procedure

global procedure release_mem( atom pData )
  w32release_mem(pData)
end procedure

global procedure release_all_mem()
  -- w32release_all_mem()
end procedure

global function llSetAbort(integer i)
  return w32llSetAbort(i)
end function

global function get_bits(atom b32)
  return w32get_bits(b32)
end function

global function signed_word( atom a )
  return w32signed_word( a )
end function

global function pack_word( integer low, integer high )
  return w32pack_word( low, high )
end function

global function lo_word( atom pData)
  return w32lo_word(pData)
end function

global function hi_word( atom pData)
  return w32hi_word(pData)
end function

global function shortInt( atom i )
  return w32shortInt(i)
end function

global function TextToNumber( sequence text)
  return w32TextToNumber(text)
end function

global function findKey( object key, sequence list )
  return w32findKey(key, list)
end function

global function removeIndex( integer index, sequence list )
  return w32removeIndex(index, list)
end function

global function removeItem( object item, sequence list )
  return w32removeItem(item, list)
end function

global function peek_string(atom a)
  return w32peek_string(a)
end function

global function address( atom addr, object offset )
  return w32address(addr,offset)
end function

global function split(sequence pSource, object pDelim)
  return w32split(pSource, pDelim)
end function

global function lookup(object pItem, sequence pSource, sequence pTarget)
  return w32lookup(pItem, pSource, pTarget)
end function

global function CType( integer pChar, object pSet)
  return w32CType(pChar, pSet)
end function

global function GetCType( object pChar)
  return w32GetCType(pChar)
end function

global procedure SetCType( object pChar, object pSet)
  w32SetCType(pChar,pSet)
end procedure

global type int(object x)
  return integer(x)
end type

global type seq(object x)
  return sequence(x)
end type

global function iff( atom test, object ifTrue, object ifFalse )
  return w32iff(test, ifTrue, ifFalse)
end function

/*
global function trim(sequence pSource)
return w32trim(pSource)
end function
*/



