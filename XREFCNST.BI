'~~~~~ Define Constants
CONST True = -1, False = 0

CONST NameLen = 40                      'Maximum length for a procedure name

CONST BASIC = 0
CONST DefFnProc = 1
CONST FuncProc = 2
CONST SubProc = 3
CONST Label = 4
CONST NumConst = 5
CONST Constant = 6
CONST Simple = 7
CONST SimpleType = 8
CONST NearArray = 9
CONST NearTypeArray = 10
CONST FarArray = 11
CONST FarTypeArray = 12

CONST Automatic = 1
CONST Parameter = 2

CONST Commn = 1
CONST Atrb = 2
CONST DimType = 3


'~~~~~ Define TYPEs for procedure Info
TYPE GenInfo
   Formated AS INTEGER
   UBSymbols    AS INTEGER              'Maximum number of objects per module
   UBWords AS INTEGER                   'Maximum number of words per line of
                                        '  source
   DBHandle AS INTEGER
   ObjHandle AS INTEGER
   RefHandle AS INTEGER

   Modules      AS INTEGER              '12
   ModuleData   AS LONG                 '14
   Incs         AS INTEGER              '18
   IncData      AS LONG                 '20

   Objects      AS INTEGER              '24
   ObjData      AS LONG                 '26
   NameData     AS LONG                 '30
   TypeNameData AS LONG                 '34

   ObjRefs      AS LONG
   RefData      AS LONG
   MaxNameLen   AS INTEGER
   
   ModNo AS INTEGER
   IncNo AS INTEGER

   ProcNo AS INTEGER
   DefFnProc AS INTEGER
   LineNo AS INTEGER
   Lines AS INTEGER

   InType AS INTEGER
   Dynamic AS INTEGER

   PrnWidth AS INTEGER
   PrnLength AS INTEGER
   PrnLeftMarg AS INTEGER
   PageIf AS INTEGER

   LineNumbers AS INTEGER
   ProcPage AS INTEGER
   GraphChars AS INTEGER
   SortProcTree AS INTEGER
   DetailProcTree AS INTEGER
   ProcTreePages AS INTEGER
   TabSpaces AS INTEGER
   DispText AS INTEGER
   Extract AS INTEGER
   ListInclude AS INTEGER

   ScrClr AS INTEGER
   BarClr AS INTEGER

   DateTime AS STRING * 19
   Page AS INTEGER
   LCount AS INTEGER
   TabConLine AS INTEGER

   ErrCode AS INTEGER
END TYPE

