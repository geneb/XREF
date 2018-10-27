TYPE ObjectInfo                         'Object information for program
   OName AS STRING * NameLen     '0     'Object name
   OType AS INTEGER              '40    'Object type (see constants above)
   Flag  AS INTEGER              '42
   Share AS INTEGER              '44    'Sharing pointer/flag
   DefModInc AS INTEGER          '46    'Defined in Module/Include
   InProcNo AS INTEGER           '48    'Defined in Procedure Number (DIM/REDIM)
   OnLineNo AS INTEGER           '50    'Defined on line number
   NumRefs AS INTEGER            '52    'Number of references to object
   FirstRef AS LONG
END TYPE


TYPE ReferenceInfo                      'Object reference information
   ObjNo AS INTEGER              '0     'Object number
   Module AS STRING * 1          '2     'Used in Module
   Include AS STRING * 1         '3     '        Include
   InProcNo AS INTEGER           '4     '  "  in Procedure number
   LineNo AS INTEGER             '6     '  "  on Line number
   Assign AS STRING * 1          '8     'Assignment flag
   NextRef AS LONG
END TYPE


