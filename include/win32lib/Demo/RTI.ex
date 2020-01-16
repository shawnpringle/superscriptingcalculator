-- RTI.EX 
-- Author: Derek Parnell, 2004
-- Extract information from a euphoria program and copies it
-- to a formatted output file.

include std/filesys.e
include w32common.ew

constant True  = (1=1)
constant False = (1=0)

constant
    kReturnCodes = next_number(0),
    kOKAY = next_number(kReturnCodes)

integer vVerbose
integer vSingleLevel
-----------------------------------------------------
function ProcessFile(sequence pFileName, object pFileAttr)
-----------------------------------------------------
    sequence lReturn
    
    pFileName = pFileName
    pFileAttr = pFileAttr
    
    lReturn = {kOKAY}
    
    
    return lReturn
end function

-----------------------------------------------------
procedure Usage(sequence pArgs)
-----------------------------------------------------
    sequence lExeFile
    
    if equal(pArgs[1], pArgs[2]) then
        lExeFile = pArgs[1]
    else
        lExeFile = pArgs[1] & ' ' & pArgs[2]
    end if
    printf(1, "%s v1.0, (c) 2004, Derek Parnell\n", {pArgs[2]})
    printf(1, "  usage: %s <programfile> [<outputfile>] [-V] [-S]\n", {lExeFile})
    puts  (1, "   -V  ==> Verbose mode\n")
    puts  (1, "   -S  ==> Single level (no include file)\n")
end procedure

-----------------------------------------------------
procedure Main(sequence pArgs)
-----------------------------------------------------
    sequence lInputFile
    sequence lOutputFile
    
    if length(pArgs) = 2 then
        -- No file supplied.
        printf(2, "\n** Error: No filename supplied\n\n",{})
        Usage(pArgs)
        abort(1)
    end if
    
    lInputFile = {}
    lOutputFile = {}
    vVerbose = False
    vSingleLevel = False
    
    for i = 3 to length(pArgs) do
        if equal(pArgs[i], "-V") then
            vVerbose = True
        elsif equal(pArgs[i], "-S") then
            vSingleLevel = True
        elsif pArgs[i][1] != '-' then
            if length(lInputFile) = 0 then
                lInputFile = pArgs[i]
            elsif length(lOutputFile) = 0 then
                lOutputFile = pArgs[i]
            else
                printf(2, "\n** Error: Too many filenames '%s'\n\n", {pArgs[i]})
                Usage(pArgs)
                abort(1)
            end if
        else
            printf(2, "\n** Error: Unrecognised commandline arg '%s'\n\n", {pArgs[i]})
            Usage(pArgs)
            abort(1)
        end if
    end for
    
end procedure

Main( command_line()  )

