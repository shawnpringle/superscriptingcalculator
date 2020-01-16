--/makedoc autobookmark on
--/makedoc autoxref on
--/topic Introduction
--/h General Hashing Routines
--/H /b<File: /i<hash.e>> /n
--   /b<Version: /i<v1.00>> /n
--   /b<Date: /i<01-sep-2000>> /n
--   /b<Author: /i<Derek Parnell>> /n
-- test ( kHASH_ERROR)
-- These routines help manage hash tables.

--/topic Hash Routines,Hashing,"hash tables"
--/info
-- These routines help manage hash tables. A Hash table is like a RAM-based
-- database, but with limitations. It is usually only for small numbers of records,
-- and for small record sizes, but it is very fast. Individual records can only be
-- retrieved if you know their key; there is no FindFirst, FindNext type of functions,
-- because there is no assumed relationship between the records in a table. However,
-- there is a function to retrieve all the stored records, optionally with a user-
-- written filtering function.
--
-- Typically, hash tables are used for syntax analysis, but can be used in any application
-- that has to retrieve data based on unique keys.
-- The record data to be stored can be anything, as these routine do not use this data in
-- any way at all. In fact, the data probably should not include the record's key as this
-- is stored by the routines anyway, so it would be redundant and wasteful.
--
-- These routines store any error messages in a global sequence called /Hash_Errors. After
-- calling any of the hash routines, you can check this sequence and if its length is not
-- zero it means that at least one error was detected. Each entry in this sequence is a 
-- three-element sequence - the first is the error code, the third is a sequence of relevant
-- data relating to the particular error, and the second is an English text message
-- relating to the error.

--/topic Hash Keys,"hashkeys"
--/info
-- The reason they are called "hash tables" is that each record's key is converted into 
-- a "Hash Key" that is, in turn, used to identify a specific queue that the record is
-- stored in. The conversion algorithms examine each key calculate a number for the key.
-- The number is divided by the number of queues available to see which queue the record
-- can be stored in. Because it is possible that different keys can all map into the same
-- queue number, some queues store multple records. The more effecient "hash tables" are
-- those that have the fewest number of records per queue.
--
-- This library helps you to distribute records over the queues by allowing you to define
-- a "hash table" to use multple algorihms. Each table created with this library has a set
-- of "dimensions", and each dimension uses a different algorithm. You not only specify
-- how many dimensions to use, but also their relative weightings. For example, a one
-- dimension table defined with /ff"{x}", a 2-dim table as /ff"{x,y}", a 3-dim table as /ff"{x,y,z}", and
-- so on. There is no limit to the number of dimensions except memory. In practice, a maximum
-- or 4-6 is realistic. These dimensions also determine the number of queues to use. The total
-- number of queues is the product of the dimension values, ie. a 3-dim table of /ff"{x,y,z}" has
-- /ff"x * y * z" queues. You can bias the algorithm(s) to use by having larger dimension values. 
-- For example, a table of /ff"{3,19,3}" has /ff"3 * 19 * 3", or 171, queues and the second dimension's 
-- algorithm is more heavily relied upon to distribute the keys than the other two. Generally,
-- the more dimensions and the more queues, the better the key distribution. But you must
-- trade-off memory for speed.
--
-- To make it easier, the table defintion funtion also allows you to supply an estimate of
-- the number of records you think will be stored, it then calculates a dimension set for
-- the table that should be suitable for the data, however it tends to be a bit memory hungry.
-- Some functions allow you to supply your own hash keys. You might do this if the default
-- algorithms don't suit your data. When you do this, you must supply a hash key set with
-- exactly the same number of elements as there are in the table's dimension. Plus,
-- each element must be in the range /i</b<1 .. N>>, where /i</b<N>> is the value in the 
-- respective dimension. Thus for a table defined with dimensions of /ff"{3,5,7}", you must
-- supply a hash-key-set with three elements and the first must be from 1 - 3, the 2nd
-- from 1-5, and the 3rd from 1-7. 
                                                                           
-- Set for SPEED!
without trace
without type_check
without profile
include std/text.e

-----------Local Variables----------------
sequence vHashTable     -- The hashed entries go into this.
-- vHashTable is a sequence that contains zero or more Tables.
-- A Table is a sequence that contains one or more Queues.
-- A Queue is a sequence that contains 2 subsequnces - Keys and Entries
-- Keys is a sequence containing zero or more inserted keys.
-- Entries is a sequence containing the same number of objects as in Keys,
--    each object is the data that is associated with its key.

sequence vTableDefs     -- The table definitions. 
sequence vTableOffsets  -- The precalc'd subscript offsets
integer  vNextId        -- The last table id used.
sequence vSeeds         -- Precalc'd hash seeds for 4 dims X 32 Keylens
integer  vDefDim        -- The default number of dimensions if a record-estimate supplied.
integer  vSeedLen       -- The maximun keylen for pre-calc'd seeds.
sequence vHashState     -- The State Stack

-- Initialise local variables --
vNextId        = 0
vHashTable     = {}
vTableDefs     = {}             
vTableOffsets  = {}
vSeeds         = {}
vDefDim        = 4
vSeedLen       = 33
vHashState     = {}

-- Local Constants --
constant
         kKeys    = 1,
         kEnts    = 2,
         kSeedDim = 4,
         kErrMsg  = {
                   "Insert callback must return a 2-element sequence.",  -- 1
                   "Table ID '%d' doesn't exist.",     -- 2
                   "Table ID '%d' has been deleted.",  -- 3
                   "The queue length for sub-table '%d' must be an integer.",  -- 4
                   "The queue length for sub-table '%d' must be greater than zero.",  -- 5
                   "Insertion mode '%d' is not known.",  -- 6
                   "Insert callback action '%d' is not known.",  -- 7
                   "No record for '%s' was found.",    -- 8
                   "Expecting %d hash values but received %d",   -- 9
                   "'%d' is an unknown Default ID",      -- 10
                   "'%s' is not an valid value for this Default",  -- 11
                   "No STATE is available to be restored.", -- 12
                   "" -- no message, just a placeholder for editing convenience.
                   }
         
         
-----------Local "SHARED" Variables----------------
---- *** WARNING *** ------
-- I'm using these instead of passing parameters among
-- the internal functions in an endevour to improve
-- the library's performance. This is a trade off to
-- good programming practice, but justified in this
-- small, localised set of functions.
-- The global functions set these variables and then 
-- invoke the internal functions to do the real work.
-- The internal functions get their "parameters" from
-- these variables and only call other internal functions.
-- As a major side-effect, the "callback" routines must
-- not call any Hash function without calling /Hash_SaveState first,
-- otherwise subsequent actions will be compromised.

integer  sTable -- The table to use.
sequence sKey   -- The key to generate hashes from.
sequence sHash  -- The current hash list.
sequence sNewEntry -- The entry to go into the table.
integer  sMode  -- The insertion mode.
integer  sQueue  -- The hash coordinates converted to a queue number.
integer  sCoord  -- The subscript into the correct queue.

      ----------Global Constants----------------
      
--/topic Action Codes,"insert modes"
--/info 
--/h These codes are used to indicate some action is required or was performed.
--/define
--/term   kHASH_ERROR         An error was detected. See Hash_Errors for details.
--/term   kHASH_UNIQUE        Insert mode: Keys must be unique, duplicate keys ignored.
--/term   kHASH_INSERT        Callback action: Insert the returned data.
--/term   kHASH_REPLACE       Callback action: Replace the existing with the returned data.
--/term   kHASH_IGNORE        Callback action: Ignore the insert request.
--/term   kHASH_DEF           Used by calling routines to request the default /hashing algorithms
--/term   kHASH_DEF_DIM       Set Default number of dimensions for record estimates.
--/term   kHASH_DEF_SEEDKEYLEN Set Max Key length for precalculated seeds.
--/enddefine

--/topic Error Codes
--/info 
--/h These codes are returned in the Hash_Errors sequence.
--/define
--/term   kHASH_ERR_BADCALLBACK  The data returned by an Insert callback function was not a 2-element sequence
--/term   kHASH_ERR_TABLEID_BAD  The supplied TableId is not any of the current Ids
--/term   kHASH_ERR_TABLEID_DEL  The supplied TableId refers to a deleted table.
--/term   kHASH_ERR_NONATOMIC_DIM  One of the Table dimensions is not an atom/integer
--/term   kHASH_ERR_NONPOS_DIM  One of the Table dimensions is than 1.
--/term   kHASH_ERR_BADMODE  A unknown Insert mode was supplied.
--/term   kHASH_ERR_CALLBACK  The action code in the Insert callback data is not recognised.
--/term   kHASH_ERR_RNF  The data to be replaced wasn't found.
--/term   kHASH_ERR_BAD_HASH  Wrong number of hash values supplied.
--/term   kHASH_ERR_BAD_DEFAULT  An unknown Default ID was supplied.
--/term   kHASH_ERR_BAD_VALUE  An invalid value for the specific default ID was supplied.
--/term   kHASH_ERR_NOSTATE  Unable to restore system state because there is no state to restore from.
--/enddefine


      
global constant
   --An error was detected. See Hash_Errors for details.
   kHASH_ERROR       = 0,
   
   --Insert mode: Keys must be unique, duplicate keys ignored.
   kHASH_UNIQUE      = -1, -- >=0 means a routine_id          
   
   --Callback action: Insert the returned data.
   kHASH_INSERT      = -2,

   --Callback action: Replace the existing with the returned data.
   kHASH_REPLACE     = -3,

   --Callback action: Ignore the insert request.
   kHASH_IGNORE      = -4,
   
   --Used by calling routines to request the default /hashing algos
   kHASH_DEF         = {},
   
   -- Default ID - Default number of dimensions for record estimates.
   kHASH_DEF_DIM     = -5,
   
   -- Default ID - Max keylen for precalculated seeds.
   kHASH_DEF_SEEDKEYLEN  = -6,

   --The data returned by an Insert callback function was not a 2-element sequence
   kHASH_ERR_BADCALLBACK    = 1,                                                  
   
   --The supplied TableId is not any of the current Ids
   kHASH_ERR_TABLEID_BAD    = 2,
   
   --The supplied TableId refers to a deleted table.
   kHASH_ERR_TABLEID_DEL    = 3,
   
   --One of the Table dimensions is not an atom/integer
   kHASH_ERR_NONATOMIC_DIM  = 4,
   
   --One of the Table dimensions is than 1.
   kHASH_ERR_NONPOS_DIM     = 5, 
                   
   --A unknown Insert mode was supplied.
   kHASH_ERR_BADMODE        = 6,
   
   --The action code in the Insert callback data is not recognised.
   kHASH_ERR_CALLBACK       = 7,

   --The data to be replaced wasn't found.
   kHASH_ERR_RNF            = 8,

   --Wrong number of hash values supplied.
   kHASH_ERR_BAD_HASH       = 9,

   --An unknown Default ID was supplied.
   kHASH_ERR_BAD_DEFAULT    = 10,

   --An invalid value for the specific default ID was supplied.
   kHASH_ERR_BAD_VALUE      = 11,
   
   --Unable to restore system state because there is no state to restore from.
   kHASH_ERR_NOSTATE        = 12,
   
   --A place holder for editing convenience
   kHASH_EMPTY              = 0

      ----------Global Variables----------------
--/topic Error Messages
--/info
-- All error messages are posted to a special area in the library, 
-- rather than being returned in the function or procedure call.
-- This is because the library supports multiple messages per call.
--/var Hash_Errors 
--/desc Stores /<error codes>"Error Code" and messages.
-- This sequence is updated whenever an error is detected. Each new error
-- is appended, enabling this to handle multiple errors in a single call. All
-- previous errors are deleted when you call a Hash function. Each entry in
-- this consists of a 3-element sequence.
--/li    [1] An Error Code
--/li    [2] A sequence of data for the error
--/li    [3] A message template in English
--/code
--   sequence lRC
--   lRC = Hash_...( ... )
--   if length(lRC) != 0 then
--    for i = 1 to length(lRC) do
--       printf(2, "Hash Err #%d, '%s'\n",
--                {lRC[i][1], 
--                 sprintf(lRC[i][3], lRC[i][2])
--                }
--             )
--    end for
--   else
--    ... -- Function worked.
--   end if
--/endcode
global sequence
   Hash_Errors    Hash_Errors = {}
   
      -----------Local Functions---------------
----------------------------------------
function f_NewId() -- Returns the next table if to use. --
----------------------------------------
   -- Initialise the seeds on first call to NewId()
   if length(vSeeds) = kHASH_EMPTY
   then
      -- The seeds for keylens 0-32 and dimension 1-4 are precalc'd for speed.
      vSeeds = repeat({0,0,9,0}, vSeedLen)
      for lKeyLen = 1 to vSeedLen do
            vSeeds[lKeyLen][1] = lKeyLen - 1
            vSeeds[lKeyLen][2] = lKeyLen * 4 - 4
            -- vSeeds[lKeyLen][3] is always = 9
            vSeeds[lKeyLen][4] = lKeyLen * lKeyLen - lKeyLen + 17
      end for
   end if

   -- N.B. Table IDs are never to be reused.
   vNextId += 1
   return vNextId  -- The next ID code to use.
end function             

----------------------------------------
function f_ValidTable() -- Validate the table id supplied. --
----------------------------------------
-- Uses: sTable

   -- Check for a range error.
   if sTable <= 0
        or
      sTable > length(vTableDefs)
   then                            
      Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_TABLEID_BAD,
                            {sTable},
                            kErrMsg[kHASH_ERR_TABLEID_BAD]
                            })
      return kHASH_ERROR -- Invalid table id detected.
   end if
                              
   -- Check for deleted tables.
   if length(vTableDefs[sTable]) = kHASH_EMPTY
   then                            
      Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_TABLEID_DEL,
                            {sTable},
                            kErrMsg[kHASH_ERR_TABLEID_DEL]
                            })
      return kHASH_ERROR -- Invalid table id detected.
   end if
   
   return 1 -- Table id is valid
end function

----------------------------------------
function f_Calc_Hash() -- Calculate the data key's hash set
----------------------------------------
-- Uses: sTable
--       sKey
atom     lVal
integer  lKeyLen, lMod, lAdjust, lDims
sequence lTableDef, lResults
sequence lInterim                   
integer  lCurChar, lCurCP, lRevSub

   lTableDef = vTableDefs[sTable]
   lDims     = length(lTableDef)
   lKeyLen   = length(sKey)
             
   if lKeyLen < vSeedLen 
         and
      lDims <= kSeedDim
   then -- use precalc'd seed values
         lResults = vSeeds[lKeyLen + 1][1 .. lDims]
   else
      lResults = repeat(0, lDims)
      lMod = 0
      for lSub = 1 to lDims do
         lMod  += 1
         if lMod > 4
         then lMod = 1
         end if
                                           
         lAdjust = (lSub * (1 + lMod)) - 2
         if    lMod = 1 then lVal = lKeyLen + lAdjust
         elsif lMod = 2 then lVal = lKeyLen * lAdjust
         elsif lMod = 3 then lVal = lAdjust
         else                lVal = lKeyLen * lKeyLen - lKeyLen + lAdjust
         end if
         
         lResults[lSub] = lVal
      end for
   end if

                         
    

   lInterim = repeat(0, lDims)
   if lDims = 1 then
      for lSub = 1 to lKeyLen do
         lCurChar = sKey[lSub]
         lInterim[1] = (lCurChar * lSub + lCurChar)
         lResults += lInterim
      end for
   elsif lDims = 2 then            
      for lSub = 1 to lKeyLen do
         lCurChar = sKey[lSub]
         lCurCP   = lCurChar * lSub
         lInterim[1] = (lCurCP + lCurChar)
         lInterim[2] = (lCurCP * (1 + lKeyLen - lSub))
         lResults += lInterim
      end for
   elsif lDims = 3 then            
      for lSub = 1 to lKeyLen do
         lCurChar = sKey[lSub]
         lCurCP   = lCurChar * lSub
         lInterim[1] = (lCurCP + lCurChar)
         lInterim[2] = (lCurCP * (1 + lKeyLen - lSub))
         lInterim[3] = (lCurChar + (lSub * (lSub + 2)) * sKey[1] )
         lResults += lInterim
      end for
   elsif lDims = 4 then            
      for lSub = 1 to lKeyLen do
         lCurChar = sKey[lSub]
         lCurCP   = lCurChar * lSub
         lRevSub  = 1 + lKeyLen - lSub
         lInterim[1] = (lCurCP + lCurChar)
         lInterim[2] = (lCurCP * lRevSub)
         lInterim[3] = (lCurChar + (lSub * (lSub + 2)) * sKey[1] )
         lInterim[4] = ((lSub * lSub) + (256 - lCurChar) * (sKey[lRevSub]))
         lResults += lInterim
      end for
   else  
      for lSub = 1 to lKeyLen do
         lMod = 0
         
         lCurChar = sKey[lSub]
         lCurCP   = lCurChar * lSub
         lRevSub  = 1 + lKeyLen - lSub
         for lDim = 1 to lDims do
            lMod  += 1
            if lMod > 4
            then lMod = 1
            end if
            
            if    lMod = 1 then
               lResults[lDim] += (lCurCP + lCurChar)
            elsif lMod = 2 then
               lResults[lDim] += (lCurCP * lRevSub)
            elsif lMod = 3 then
               lResults[lDim] += (lCurChar + (lSub * (lSub + 2)) * sKey[1] )
            else
               lResults[lDim] += ((lSub * lSub) + (256 - lCurChar) * (sKey[lRevSub]))
            end if 
            
         end for
      end for
   end if
                            

   lResults = remainder(lResults, lTableDef) + 1
  
   return lResults
end function     


----------------------------------------
procedure f_Calc_Queue() -- Calculate the queue from the hash coordinates
----------------------------------------
-- Uses: STable
--       sHash

sequence lOffset

   if length(sHash) = kHASH_EMPTY
   then
      sHash = f_Calc_Hash()
   end if
                  
   lOffset = vTableOffsets[sTable]
   sQueue  = sHash[length(sHash)]
   for lSub = 1 to length(lOffset) - 1 do
       sQueue += (lOffset[lSub] * (sHash[lSub] - 1))
   end for                   
   

end procedure

----------------------------------------
function f_Fetch() 
----------------------------------------
-- Uses: sTable
--       sCoord
                                          
   -- ** This has duplicated code to improve performance. --
                                              
   if sCoord != 0 -- tested first because most of the time it will be non-zero.
   then
      return {vHashTable[sTable][sQueue][kEnts][sCoord]}
   else -- however sometime I will have to look it up.
      -- Locate the particular subsequence
      sCoord = find(sKey,vHashTable[sTable][sQueue][kKeys]) 
      if sCoord != 0
      then
         return {vHashTable[sTable][sQueue][kEnts][sCoord]}
      else
         return {}
      end if   
   end if
   

end function
      
----------------------------------------
function f_Replace()
----------------------------------------
-- Uses: sTable
--       sKey
--       sNewEntry
--       sCoord
   if sCoord != 0
   then   
      vHashTable[sTable][sQueue][kEnts][sCoord] = sNewEntry
      return kHASH_REPLACE
   else
      sCoord = find(sKey,vHashTable[sTable][sQueue][kKeys]) 
      if sCoord != 0
      then 
         vHashTable[sTable][sQueue][kEnts][sCoord] = sNewEntry
         return kHASH_REPLACE
      else
         Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_RNF,
                            {sKey},
                            kErrMsg[kHASH_ERR_RNF]
                            })
         return kHASH_ERROR
      end if
   end if
   
end function

----------------------------------------
function f_Insert()
----------------------------------------
-- Uses: sTable
--       sKey
--       sNewEntry
--       sHash
--       sMode
--       sQueue

sequence  lCallBackResult

   if sMode = kHASH_UNIQUE
   then
      if find(sKey,vHashTable[sTable][sQueue][kKeys]) = 0
      then
         vHashTable[sTable][sQueue][kKeys] = append(vHashTable[sTable][sQueue][kKeys], sKey)
         vHashTable[sTable][sQueue][kEnts] = append(vHashTable[sTable][sQueue][kEnts], sNewEntry)
         return kHASH_INSERT
      else
         return kHASH_IGNORE
      end if
   elsif sMode > kHASH_UNIQUE
   then
      lCallBackResult = call_func(sMode, {sKey, f_Fetch(), sNewEntry})
      if sequence(lCallBackResult)
            and
         length(lCallBackResult) = 2
      then
         if  lCallBackResult[1] = kHASH_REPLACE -- Replace existing entry
         then
            sNewEntry = lCallBackResult[2]
            return f_Replace()
         elsif lCallBackResult[1] = kHASH_INSERT -- Do insert this
         then
            if sCoord = 0
            then
               vHashTable[sTable][sQueue][kKeys] = append(vHashTable[sTable][sQueue][kKeys], sKey)
               vHashTable[sTable][sQueue][kEnts] = append(vHashTable[sTable][sQueue][kEnts], lCallBackResult[2])
               return kHASH_INSERT
            else
               return kHASH_IGNORE
            end if
         elsif lCallBackResult[1] = kHASH_IGNORE -- Don't insert this
         then
            return kHASH_IGNORE
         else
            Hash_Errors = append(Hash_Errors, 
                        {kHASH_ERR_CALLBACK,
                         {lCallBackResult[1]},
                         kErrMsg[kHASH_ERR_CALLBACK]
                         })
            return kHASH_ERROR -- Invalid callback result.
         end if
      else  
         Hash_Errors = append(Hash_Errors, 
                        {kHASH_ERR_BADCALLBACK,
                         {},
                         kErrMsg[kHASH_ERR_BADCALLBACK]
                         })
         return kHASH_ERROR -- Invalid callback result.
      end if
   else
      Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_BADMODE,
                            {sMode},
                            kErrMsg[kHASH_ERR_BADMODE]
                            })
      return kHASH_ERROR -- Bad mode used.
   end if
  
end function


      -----------Global Functions--------------
with type_check
--/topic System State
--/info
-- The Hash Routines have an internal state which can be compromised if not dealt with correctly.
--/func Hash_SaveState( )
--/desc This saves the internal state of the hash functions.
-- This allows re-enterant code to work. This must be used by callback routines if they
-- call any other Hash function.
--/ret Nothing is returned as this is a procedure. Does not update Hash_Errors
--/code
-- Examples:-
--   function InsertCallback( ... )
--    ...
--    Hash_SaveState()
--    lRC = Hash_Fetch ( lHT, "GrandTotal", kHASH_DEF)
--    lRC[1][1] += 1
--    lRC = Hash_Replace( lHT, "GrandTotal", lRC[1], kHASH_DEF)
--    Hash_RestoreState()                     
--    ...
--    return ...
--   end function
--/endcode
----------------------------------------
global procedure Hash_SaveState( )
-----------------------------------------
   Hash_Errors = {}  -- Clean out any earlier errors.
   
   -- Insert a new State record on the top of the stack.
   vHashState = prepend(vHashState,
                        {
                           sTable,
                           sKey,
                           sHash,
                           sNewEntry,
                           sMode,
                           sQueue,
                           sCoord,
                           Hash_Errors
                        }
                      )
end procedure

--/topic System State
--/func Hash_RestoreState( )
--/desc This restores the internal state of the hash functions.
-- The state, saved by Hash_SaveState, is restore to allow re-enterant code to work.
-- This must be used by callback routines if they call any other Hash function.
-- Hash_Errors is updated with code HASH_ERR_NOSTATE if there is no state
-- to restore.
--/ret Nothing is returned as this is a procedure. 
--/code
-- Examples:-
--   function InsertCallback( ... )
--    ...
--    Hash_SaveState()
--    lRC = Hash_Fetch ( lHT, "GrandTotal", kHASH_DEF)
--    lRC[1][1] += 1
--    lRC = Hash_Replace( lHT, "GrandTotal", lRC[1], kHASH_DEF)
--    Hash_RestoreState()                     
--    ...
--    return ...
--   end function
--/endcode
----------------------------------------
global procedure Hash_RestoreState( )
-----------------------------------------

   if length(vHashState) != kHASH_EMPTY
   then
      -- Restore values from top State record.
      sTable      = vHashState[1][1]
      sKey        = vHashState[1][2]
      sHash       = vHashState[1][3]
      sNewEntry   = vHashState[1][4]
      sMode       = vHashState[1][5]
      sQueue      = vHashState[1][6]
      sCoord      = vHashState[1][7]
      Hash_Errors = vHashState[1][8]
      
      -- Pop top State record off the stack.
      vHashState  = vHashState[2 .. length(vHashState)]
   else
      Hash_Errors = append(Hash_Errors, 
                        {kHASH_ERR_NOSTATE,
                         {},
                         kErrMsg[kHASH_ERR_NOSTATE]
                         })
   end if
end procedure

--/topic Table Management
--/info
-- This group of routines is used to manage hash tables as a whole.
--/func Hash_NewTable( object Dimensions )
--/desc This defines a new "hash table" and returns its ID.
-- 'Dimensions' is either a sequence containing a list
--    of atoms or a single integer. 
-- If a sequence then the number of atoms specifies the number
--   of sub-tables to use. Each sub-table uses a different
--   algorithm to generate a /hashing index. The value of
--   each atom specifies the number of queues in its sub-table.
--   The queues hold the inserted entries. Which queue is actually
--   used for a given entry depends on the number of sub-tables
--   and the number of queues in each sub-table. By adjusting
--   these dimensions, you can influence the efficiency of finding
--   entries in a table. The aim is to get the entries evenly 
--   spread amonsgt to queues and to have a minimum of entries
--   per queue.
-- If an integer, 'Dimensions' is an estimate of the number of
--   entries expected to be stored. This function calculates a
--   default queue length for four sub-tables. The default is
--   biased towards efficient lookup so it uses more memory than
--   may be required.
--/ret Returned is an integer representing a table id. 
-- This id must be
--   used in subsequent /hashing functions. Zero is returned when 
--   the function finds something wrong with the 'Dimensions' 
--   parameter.
--/code
-- Examples:-
--
--   integer lHT
--   lHT = Hash_NewTable( {7,5,91} ) -- This creates a "hash table"
--                                      with 3 sub-tables
--                                      Sub table #1 has 7 queues,
--                                      Sub table #2 has 5 queues,
--                                      Sub table #3 has 91 queues
--   lHT = Hash_NewTable ( 7000 ) -- This creates a "hash table" that
--                                   is expected to store about 7000
--                                   entries.
--/endcode

----------------------------------------
global function Hash_NewTable(
                              object pDim -- Dimensions for a new table.
                             )
-----------------------------------------
integer  lQueues                                
sequence lOffset

   Hash_Errors = {}  -- Clean out any earlier errors.
                   
   if atom(pDim) 
   then -- Use this estimate to distribute entries over kSeedDim queues.
      pDim = repeat(floor(power((pDim * vDefDim), (1/vDefDim))) + 1, vDefDim)
   end if
   
   lQueues = 1      
   -- Validate the dimensions supplied.
   for lSub = 1 to length(pDim) do
      if not atom(pDim[lSub])
      then
         Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_NONATOMIC_DIM,
                            {lSub},
                            kErrMsg[kHASH_ERR_NONATOMIC_DIM]
                            })
         return kHASH_ERROR -- All dimensions must be atoms
      end if
      if pDim[lSub] < 1
      then
         Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_NONPOS_DIM,
                            {lSub},
                            kErrMsg[kHASH_ERR_NONPOS_DIM]
                            })
         return kHASH_ERROR -- All dimensions must greater than 0
      end if
      
      lQueues *= pDim[lSub] -- Calc the total # of queues required.
   end for
      

   -- Precalc the subscript offsets for the queues.
   -- eg. If there are 4 dimensions {a,b,c,d} then the
   --     offsets would be { b*c*d, c*d, d, 1 }
   lOffset = repeat(1, length(pDim))
   for i = 1 to length(pDim) - 1 do
      for j = i + 1 to length(pDim) do
         lOffset[i] *= pDim[j]
      end for
   end for
   
   -- Store the dimensions for the table
   vTableDefs    = append(vTableDefs, pDim)
   
   -- Build an empty table with 'lQueues' number of queues.
   -- Each queue has 2 entries, one for the keys and the other for the data.
   vHashTable    = append(vHashTable, repeat({{}, {}}, lQueues))
   
   -- Store the subscript offsets for this table.
   vTableOffsets = append(vTableOffsets, lOffset)
   
   return f_NewId() -- Grab a new ID number and return it.
end function

--/topic Hash Routines
--/func Hash_Calc( integer TableId, sequence Key )
--/desc This calculates the "hash" key for your supplied data key.
-- The hash key is based on the dimensions defined for this table. 
-- You would typically use this to store the hash key for common data keys
-- to save the lookup routine recalculating it. /n
-- This function /b<does not> store any data!
--/ret Returned is a sequence that contains the this data key's hash key.
--/code
-- Examples:-
--
--   integer lHT
--   sequence lHashKey
--   lHT = Hash_NewTable( {7,5,91} ) -- This creates a "hash table"
--   lHashKey = Hash_Calc ( lHT , "Derek") -- Returns the hash key for "Derek"
--/endcode

----------------------------------------
global function Hash_Calc(
----------------------------------------
                   integer pTable, -- The table to use.
                   sequence pKey   -- The key to generate hashes from.
                  )
----------------------------------------

   Hash_Errors = {}  -- Clear any earlier errors.
   
   -- Store the user's parameters.
   sTable = pTable   
   sKey   = pKey
                                  
   -- Only do the calculation if we get a valid table id.
   if  f_ValidTable()
   then
      return f_Calc_Hash()
   else
      return {}   -- Invalid "hash table" id.
   end if             
   
end function     

--/topic Record Management
--/info
-- This group of routines manage the records (key and data) in a specific table.
--/func Hash_Fetch( integer TableId, sequence Key, sequence Hashkey )
--/desc This tries to find the data that was stored in the Table
-- It uses the the data Key. If Hashkey is kHASH_DEF the routine first calculates
-- a hash key for this data key, otherwise it uses whatever hashing data
-- you provide. See HashKeys for more details
-- If the returned sequence is empty, then the key has not been
-- inserted into the "hash table", otherwise the returned data is in the first 
-- element. I've done it this way so that I can support empty sequences as 
-- valid data. That is, an empty data sequence would be returned as /b<{{}}> whereas
-- No-Data-Found is returned as /b<{}>.
--/ret Returned is a sequence that contains the data that was stored against the data Key.
--/code
-- Examples:-
--
--   integer lHT -- Hash Table ID
--   sequence lMyData
--   lMyData = Hash_Fetch ( lHT , "Derek", kHASH_DEF)
--   if length(lMyData) = 0
--   then --- no data found.
--   else --- data is in lMyData[1] !!! N.B. Not in lMyData, but in lMyData's 1st element.
--   end if
--/endcode
----------------------------------------
global function Hash_Fetch(
----------------------------------------
                      integer pTable, -- The table to use.
                      sequence pKey,  -- The key to check.
                      sequence pHash  -- List of hashes.
                     )
----------------------------------------

   Hash_Errors = {}  -- clear any earlier errors.
   
   -- Store the user's parameters.
   sTable = pTable   
   sKey   = pKey
   sHash  = pHash
   sCoord = 0
                       
   -- Don't bother if the table id is bad.
   if not f_ValidTable()
   then
      return {}   -- Invalid "hash table" id.
   end if
                                          
   if length(pHash) != kHASH_EMPTY
         and
      length(pHash) != length(vTableDefs[sTable])
   then
      Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_BAD_HASH,
                            {length(vTableDefs[sTable]), length(pHash)},
                            kErrMsg[kHASH_ERR_BAD_HASH]
                            })
      return {} -- Invalid hash supplied.
   end if
   
   f_Calc_Queue()  -- Calculate the queue to search.
                                    
   return f_Fetch() -- Do the search.
end function
      

--/topic Record Management
--/func Hash_Replace( integer TableId, sequence Key, sequence NewData, sequence Hashkey )
--/desc This is used to replace/update the data previously stored.
-- After your data has been inserted into your table via a call to
-- Hash_Insert, you can use this function to fetch the store data. 
-- You must supply the Table the data was stored in, the data's Key,
-- the new data, and optionally the key's hash value. You can use kHASH_DEF
-- to have this routine calculate the hash for you. See HashKeys for more detail.
--/ret Returned is a success code, either kHASH_ERROR or kHASH_REPLACE.
--/code
-- Examples:-
--
--   integer lHT -- Hash Table ID
--   integer lRC
--   lRC = Hash_Replace ( lHT , "Derek", {1,2,"xyz"}, kHASH_DEF)
--   if lRC = kHASH_ERROR
--   then --- an error detected. See Hash_Errors for details.
--   else --- data has been succesfully replaced.
--   end if
--/endcode
----------------------------------------
global function Hash_Replace(
----------------------------------------
                     integer pTable,     -- The table to use.
                     sequence pKey,      -- The key to check.
                     sequence pNewEntry, -- The replacement entry.
                     sequence pHash      -- List of hashes
                    )
----------------------------------------

   Hash_Errors = {}  -- Clear any earlier errors
   
   -- Store user's parameters.
   sTable = pTable
   sKey = pKey
   sNewEntry = pNewEntry
   sHash = pHash
   sCoord = 0
                              
   -- Don't bother if a bad table id passed.
   if not f_ValidTable()
   then
      return kHASH_ERROR   -- Invalid "hash table" id.
   end if
   
   if length(pHash) != kHASH_EMPTY
         and
      length(pHash) != length(vTableDefs[sTable])
   then
      Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_BAD_HASH,
                            {length(vTableDefs[sTable]), length(pHash)},
                            kErrMsg[kHASH_ERR_BAD_HASH] 
                              })
      return kHASH_ERROR -- Invalid hash supplied.
   end if
   
   f_Calc_Queue()  -- Calculate the queue to search.

   return f_Replace()  -- Go do the replace.
   
end function

--/topic Record Management
--/func Hash_Insert( integer TableId, sequence Key, sequence Data, integer Mode, sequence Hashkey )
--/desc This is used to insert new data into the table. 
-- You must supply the Table to use, the data's Key,
-- the data to store, the Insertion Mode, and optionally the key's hash value.
-- You can use kHASH_DEF to have this routine calculate the hash for you. 
-- See /HashKeys for more detail.
-- The Insertion Mode is either kHASH_UNIQUE or a routine_id. A value of kHASH_UNIQUE inserts 
-- the data and its key if the key has not already been inserted, otherwise it just ignores it.
-- A routine_id allows the use of more sophosticated control of the data. With a callback routine,
-- you can elect to modify the new data prior to insertion, or update existing data, or ignore
-- new data. The callback routine receives three parameters - the key, any existing data, and 
-- the new data. It must return a sequence of two elements - the first must be either kHASH_REPLACE,
-- kHASH_INSERT, or kHASH_IGNORE. The second must be a sequence - for kHASH_INSERT and kHASH_REPLACE
-- this the data put into the table, for kHASH_IGNORE it can be anything because it is just ignored.
--/ret Returned is a success code, either kHASH_ERROR, kHASH_INSERT, kHASH_IGNORE or kHASH_REPLACE.
--/code
-- Examples:-
-- #1 - Simple Insertion...
--   integer lHT -- Hash Table ID
--   integer lRC
--   lRC = Hash_Insert ( lHT , "Derek", {1,2,"xyz"}, kHASH_UNIQUE, kHASH_DEF)
--   if lRC = kHASH_ERROR
--   then --- an error detected. See /Hash_Errors for details.
--   elsif lRC = kHASH_INSERT --- data has been succesfully inserted.
--   else  --- data already exists
--   end if
-- #2 - Insertion with callback...
--   function MyCallback(sequence pKey, sequence pExisting, sequence pNew)
--   This counts the occurances of non-numeric records.
--   if length(pExisting) = 0 
--   then  -- New Record
--      if IsNumeric(pKey)
--      then
--         return {{kHASH_INSERT, pNew} ) Don't add a counter.
--      else
--         return {{kHASH_INSERT, append(pNew, 1)} ) Attach a counter to the data.
--      end if
--   else
--      if length(pExisting[1]) = 4
--      then
--         pExisting[1][4] += 1 -- Increment the counter
--         return {{kHASH_REPLACE, pExisting[1]} ) 
--      else
--         return {{kHASH_IGNORE, {}} ) 
--      end if
--   end if
--   end function
--
--   integer lHT -- Hash Table ID
--   integer lRC
--   lRC = Hash_Insert ( lHT , "Derek", {1,2,"xyz"}, routine_id("MyCallback"), kHASH_DEF)
--   if lRC = kHASH_ERROR
--   then --- an error detected. See /Hash_Errors for details.
--   elsif lRC = kHASH_INSERT --- data has been succesfully inserted.
--   else  --- data already exists
--   end if
--/endcode
----------------------------------------
global function Hash_Insert(
----------------------------------------
                     integer pTable,     -- The table to use.
                     sequence pKey,      -- The key to check.
                     sequence pNewEntry, -- The entry to insert.
                     integer pMode,      -- Insertion mode.
                     sequence pHash      -- List of hashes
                    )
----------------------------------------

   Hash_Errors = {}
   sTable = pTable
   sKey = pKey
   sNewEntry = pNewEntry
   sHash = pHash
   sMode = pMode
   sCoord = 0
   
   if not f_ValidTable()
   then
      return kHASH_ERROR   -- Invalid "hash table" id.
   end if

   if length(pHash) != kHASH_EMPTY
         and
      length(pHash) != length(vTableDefs[sTable])
   then
      Hash_Errors = append(Hash_Errors, 
                           {kHASH_ERR_BAD_HASH,
                            {length(vTableDefs[sTable]), length(pHash)},
                            kErrMsg[kHASH_ERR_BAD_HASH] 
                              })
      return kHASH_ERROR -- Invalid hash supplied.
   end if
   
   f_Calc_Queue()
   
   return f_Insert()   
   
end function


--/topic Table Management
--/func Hash_TableStats( integer TableId)
--/desc This is used to fetch the current statistics of a specific table.
--  Can be used to determine the best set of table dimensions to use with
--  the given set of data being inserted.
-- You must supply the Table to use.
--/ret Returned is a sequence containing various statistics.
--/h Layout of Statistics Sequence
--/li    [1] ==> Total records inserted
--/li    [2] ==> # of Queues with records
--/li    [3] ==> # of Empty Queues
--/li    [4] ==> Maximum record count in a queue
--/li    [5] ==> Minimum record count in a (non-empty) queue
--/li    [6] ==> Average number of records per queue
--/li    [7] ==> Standard Deviation of records in queues
--/li    [8] ==> # of queues with higher than average record count
--/li    [9] ==> # of queues with lower than average record count
--/li    [10]==> Queue Spread. A sequence containing queue counts per queue size.
-- That is, the # of queues with 1 record, 2 records, 3 records ...
--/li    [11]==> Table Definition. A sequence containing the table's dimesions.
--/code
-- Examples:-
--   integer lHT -- Hash Table ID
--   sequence lRC
--   lRC = Hash_TableStats ( lHT)
--   if length(lRC) = 0
--   then --- an error detected. See Hash_Errors for details.
--   else
--      printf(1, "Records inserted: %d, Avg Q len=%f\n",
--                 {lRC[1], lRC[6]})
--   end if
--/endcode
----------------------------------------
global function Hash_TableStats(
----------------------------------------
                     integer pTable     -- The table to use.
                    )
----------------------------------------
sequence lTable, lSpread
integer  lTotal, lFree, lInUse, lMax, lMin, lHigh, lLow, lCnt
atom     lAvg, lSumOfSquares, lSD

   sTable = pTable                          
   Hash_Errors = {}

   if f_ValidTable()
   then

      lTable = vHashTable[pTable]           
      lTotal = 0
      lFree = 0
      lInUse = 0
      lMax = 0
      lMin = #7FFFFF
      lHigh = 0
      lLow = 0
      lSumOfSquares = 0
      
      for lQueue = 1 to length(lTable) do
         if length(lTable[lQueue][kEnts]) != kHASH_EMPTY
         then
            lCnt = length(lTable[lQueue][kEnts])
            lInUse += 1
            lTotal += lCnt
            lSumOfSquares += (lCnt * lCnt)
            
            if lCnt > lMax
            then
               lMax = lCnt
            end if
            
            if lCnt < lMin
            then
               lMin = lCnt
            end if
   
         else
            lFree += 1
         end if
         
      end for
     
      lAvg =  lTotal / lInUse
      lSD = power( (lSumOfSquares / lInUse) - (lAvg * lAvg), 0.5)
      
      lSpread = repeat(0, lMax)
      
      for lQueue = 1 to length(lTable) do
         if length(lTable[lQueue][kEnts]) != kHASH_EMPTY
         then
            if length(lTable[lQueue][kEnts]) > lAvg
            then
               lHigh += 1
            end if
            if length(lTable[lQueue][kEnts]) < lAvg
            then
               lLow += 1
            end if
   
            lSpread[length(lTable[lQueue][kEnts])] += 1
            
         end if
         
      end for
   
      return {
         lTotal,              -- [1]
         lInUse,              -- [2]
         lFree,               -- [3]
         lMax,                -- [4]
         lMin,                -- [5]
         lAvg,                -- [6]
         lSD,                 -- [7]
         lHigh,               -- [8]
         lLow,                -- [9]
         lSpread,             -- [10]
         vTableDefs[sTable]   -- [11]
             }
   else
      return {}
   end if                      
end function
             
--/topic Table Management
--/func Hash_Stats( )
--/desc This is used to fetch the current statistics of all tables.
-- This is the same as Hash_TableStats , but returns the data for
-- every table in use.
--/ret Returned is a sequence containing a sequence for each table in use.
-- See /Hash_TableStats for the details.
--/code
-- Examples:-
--   sequence lRC
--   lRC = Hash_Stats ()
--   if length(lRC) = 0
--   then --- an error detected. See Hash_Errors for details.
--   else
--      for i = 1 to length(lRC) do
--         printf(1, "Records inserted for %s: %d, Avg Q len=%f\n",
--                 {sprint(lRC[1][11]), lRC[i][1], lRC[i][6]})
--      end for
--   end if
--/endcode
----------------------------------------
global function Hash_Stats(
----------------------------------------
                     -- No Parameters
                    )
----------------------------------------
sequence lStats
      
   Hash_Errors = {}
   lStats = {}

   for lTable = 1 to length(vHashTable) do
      sTable = lTable
      if f_ValidTable()
      then
         lStats = append(lStats, Hash_TableStats(lTable))
      else
         Hash_Errors = {}
      end if
   end for
              
   return lStats
end function

--/topic Table Management
--/func Hash_GetTable( integer TableId, integer Filter, sequence Extras)
--/desc This is used to fetch the all the data inserted into a specific table.
-- You must supply the Table to use, and an optional filtering routine, and optional
-- data for the filter function.
-- To use a filtering routine, the second parameter must be a valid routine_id. This
-- routine is passed three parameters, the record's key, the record's data and the Extras.
-- The routine must return a sequence. If the sequence is empty, the record will be skiped.
-- Otherwise the returned sequence must contain a single element, and this element is returned
-- to the caller of Hash_GetTable.
--/ret Returned is a sequence containing the selected data. 
-- Each element in the returned sequence
-- is a two-element sequence containing the record's key and data. 
-- The order of the returned records is random; it is not sorted.
--/code
-- Examples:-                       
--   function NumFilter( sequence pKey, sequence pData, sequence pExtra)
--     if IsNumeric(pKey)
--     then
--        return {pData}
--     else
--        return{}
--     end if
--   end function
--
--   function AlphaFilter( sequence pKey, sequence pData, sequence pExtra)
--   Only select alpha-keys that match the 'type' in pExtra.
--     if not IsNumeric(pKey)
--     then
--        if equal(pData[3], pExtra[1])
--        then
--          return {pData}
--        else
--          return {}
--        end if
--     else
--        return{}
--     end if
--   end function
--
--   integer lHT -- Hash Table ID
--   sequence lNums, lWords
--   lNums  = Hash_GetTable ( lHT, routine_id("NumFilter"), {})
--   lWords = Hash_GetTable ( lHT, routine_id("AlphaFilter"), {"CONST"})
--/endcode
----------------------------------------
global function Hash_GetTable(
----------------------------------------
                     integer pTable,     -- The table to use.
                     integer pFilter,    -- The routine_id of a filter function
                     sequence pExtra     -- Data passed to the flter function.
                    )
----------------------------------------
sequence lEntries 
object lCallbackResult
         
   Hash_Errors = {}
   sTable = pTable                          

   lEntries = {}
   if f_ValidTable()
   then
      for lQueue = 1 to length(vHashTable[sTable]) do -- each queue
         for j = 1 to length(vHashTable[sTable][lQueue][kEnts]) do
            if pFilter < 0
            then                           
               lCallbackResult = {vHashTable[sTable][lQueue][kEnts][j]}
            else                                                
               lCallbackResult = call_func(pFilter,
                        {vHashTable[sTable][lQueue][kKeys][j], 
                         vHashTable[sTable][lQueue][kEnts][j],
                         pExtra})
            end if
            
            if sequence(lCallbackResult)
                  and
               length(lCallbackResult) != kHASH_EMPTY
            then
               lEntries = append(lEntries, 
                     {vHashTable[sTable][lQueue][kKeys][j],
                      lCallbackResult[1]}
                              )
            end if

         end for
      end for
   end if
   
   return lEntries
end function

--/topic Table Management
--/proc Hash_DeleteTable( integer TableId)
--/desc This removes a specific table, and all the data inserted into it.
-- You must supply the Table to use.
-- Once a table is deleted, its Table ID cannot be used again.
--/ret Nothing is returned. This is a Procedure.
--/code
-- Examples:-                       
--   integer lHT -- Hash Table ID
--   Hash_DeleteTable ( lHT )
--/endcode
----------------------------------------
global procedure Hash_DeleteTable(
----------------------------------------
                     integer pTable     -- The table to use.
                    )
----------------------------------------

   Hash_Errors = {}
   sTable = pTable                          
   
   if f_ValidTable()
   then
      -- The data for a table is removed. 
      -- N.B. A deleted table ID is prevented from being reused. This
      --      just might prevent a bug in the user's program.
      vHashTable[sTable]    = {}
      vTableDefs[sTable]    = {}
      vTableOffsets[sTable] = {}
   end if                    
   
end procedure

--/topic System State
--/proc Hash_SetDefault( integer pDefaultId, object pValue)
--/desc This allows the caller to set various default values used by these routines.
-- You must supply two parameters, the first contains the code for the default being set,
-- the second is the new value for that default.
--/define
--/term kHASH_DEF_DIM Default number of dimensions when record estimates are supplied to
--                Hash_NewTable function. /i"Must be an integer greater than zero."
--/term kHASH_DEF_SEEDKEYLEN Maximum length of key for pre-calculated seeds.
--               /i"Must be an integer greater than zero."
--/enddefine
--/ret Returns kHASH_ERROR if an error was detected, and kHASH_IGNORE if the default was set.
--/code
-- Examples:-                       
--   Hash_SetDefault ( kHASH_DEF_DIM, 3 )  -- Set the default dimensions to 3.
--/endcode
----------------------------------------
global function Hash_SetDefault(
----------------------------------------
                     integer pDefaultId,  -- The ID of the default to change
                     object  pValue       -- The value to set it to.
                    )
----------------------------------------

   Hash_Errors = {}
               
   if pDefaultId = kHASH_DEF_DIM
   then
      if atom(pValue)
         and
         pValue > 0
      then
         vDefDim = pValue
         return kHASH_IGNORE
      else
         Hash_Errors = append(Hash_Errors,
                              {kHASH_ERR_BAD_VALUE,
                               {sprint(pDefaultId)},
                               kErrMsg[kHASH_ERR_BAD_VALUE]
                              })
         return kHASH_ERROR
      end if
   elsif pDefaultId = kHASH_DEF_SEEDKEYLEN
   then
      if atom(pValue)
         and
         pValue > 0
      then
         vSeedLen = pValue + 1
         return kHASH_IGNORE
      else
         Hash_Errors = append(Hash_Errors,
                              {kHASH_ERR_BAD_VALUE,
                               {sprint(pDefaultId)},
                               kErrMsg[kHASH_ERR_BAD_VALUE]
                              })
         return kHASH_ERROR
      end if
   else
      Hash_Errors = append(Hash_Errors,
                           {kHASH_ERR_BAD_DEFAULT,
                            {pDefaultId},
                            kErrMsg[kHASH_ERR_BAD_DEFAULT]
                           })
      return kHASH_ERROR
   end if
end function
