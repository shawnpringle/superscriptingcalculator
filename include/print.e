-- Derek Parnell, June 2004
-- A smarter 'print' statement and a 'string' type

-- This calls the original RDS print routine.
procedure rdsprint(integer fh, object x)
    print(fh,x)
end procedure

-- Routine ID Addresses
integer r_rdsprint  -- ID for RDS's print
integer r_print     -- ID for my print

-- By default, we crash the program when a bad string type is used.
-- If you want to avoid crashing, then assign zero to this.
global integer StringAbort 
StringAbort = 1

-- This determines if an object is really an ASCII string.
function IsASCIIString(object x)
    if atom(x) then 
        return 0
    end if
    for i = 1 to length(x) do
        if not integer(x[i]) or x[i] < 0 or x[i] > 127 then
            return -i
        end if
    end for

    return 1
end function

type string(object x)
    integer lRC
    -- Test for string type and abort only if the user wants to.
    lRC = IsASCIIString(x)

    if lRC = 0 then
        puts(2,"Invalid string value:")
        rdsprint(2,x)
        puts(2,"\n")
        return not StringAbort
    end if

    if lRC < 0 then
        puts(2,"Invalid string character value:")
        if IsASCIIString(x[-lRC]) > 0 then
            puts(2,x[-lRC])
        else
            rdsprint(2,x[-lRC])
        end if
        puts(2,"\n")
        return not StringAbort
    end if

    return 1
end type

procedure myprint(integer fh, object x, integer lvl, integer nlvl)

    -- If nesting depth is more than 1, start to show braces and commas
    if lvl > 1 then
        if nlvl = lvl then
            puts(fh, '{')
        else
            puts(fh, ',')
        end if
    end if

    -- Look for any nested sequences.
    if sequence(x) then
        for i = 1 to length(x) do
            if IsASCIIString(x[i]) > 0 then
                -- Nesting begins...
                for j = 1 to length(x) do
                    if j = length(x) then
                        -- Print the last item
                        myprint(fh, x[j],lvl+1, 0)
                    else
                        -- Print the next item
                        myprint(fh, x[j],lvl+1, lvl + j)
                    end if
                end for
                -- None left to do.
                return
            end if
        end for
    end if
    
    -- I only get here if there were no nested sequences.
    if IsASCIIString(x) > 0 then
        -- Show a string
        puts(fh, x)

    elsif integer(x) then
        -- Convert int to a string then show it.
        printf(fh, "%d", x)
    elsif atom(x) then
        -- Convert float to a string, trim to shortest, then show it.
        x = sprintf("%15.15g", x)
        while(x[1]) = ' ' do
            x = x[2..length(x)]
        end while
        while(x[length(x)]) = '0' do
            x = x[1..length(x)-1]
        end while
        puts(fh, x)
    else
        -- A mixed sequence so let RDS show it.
        call_proc(r_rdsprint, {fh, x})
    end if

    -- If nesting is deeper than 1 and this is the last item,
    -- then show a closing brace.
    if lvl > 1 then
        if nlvl < lvl then
            puts(fh, '}')
        end if
    end if

end procedure

without warning
global procedure print(object fh, object x)
    integer rfh

    if sequence(fh) then
        rfh = open(fh, "a")
        if rfh = -1 then
            rfh = open(fh, "w")
        end if
    else
        rfh = fh
    end if

    if rfh < 0 then 
        return
    end if

    myprint(rfh, x, 0, 0)

    if sequence(fh) then
        close(fh)
    end if
end procedure
r_print = routine_id("print")
r_rdsprint = routine_id("rdsprint")
with warning