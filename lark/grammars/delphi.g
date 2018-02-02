// taken from https://github.com/fabriciocolombo/sonar-delphi/blob/master/src/main/antlr3/org/sonar/plugins/delphi/antlr/Delphi.g

//****************************
//section start
//****************************
file                         : program | library | unit | package_e
//****************************
//section file_definition
//****************************

program                      : (program_head)? (uses_file_clause)? block "."

program_head                 : "program" namespace_name (program_parm_seq)? ";"
program_parm_seq               : "(" (ident ("," ident)* )? ")"
library                      : library_head (uses_file_clause)? block "."
library_head                  : "library" namespace_name (hinting_directive)* ";" // ^("library" namespace_name)
package_e                     : package_head requires_clause (contains_clause)? "end" "."
package_head                  : "package" namespace_name ";"
unit                         : unit_head unit_interface unit_implementation unit_block "."
unit_head                     : "unit" namespace_name (hinting_directive)* ";" // ^("unit" namespace_name (hinting_directive)*)
unit_interface                : "interface" (uses_clause)? (interface_decl)* // ^("interface" (uses_clause)? (interface_decl)*)
unit_implementation           : "implementation" (uses_clause)? (decl_section)* // ^("implementation" (uses_clause)? (decl_section)*)
unit_block                    : unit_initialization "end"
                             | compound_statement
                             | "end"
unit_initialization           : "initialization" statement_list (unit_finalization)?
unit_finalization             : "finalization" statement_list
//****************************
//section file_usage
//****************************
contains_clause               : "contains" namespace_file_name_list
requires_clause               : "requires" namespace_name_list
uses_clause                   : "uses" namespace_name_list // ^("uses" namespace_name_list)
uses_file_clause               : "uses" namespace_file_name_list // ^("uses" namespace_file_name_list)
namespace_file_name_list        : namespace_file_name ("," namespace_file_name)* ";" // namespace_file_name (namespace_file_name)*
namespace_file_name            : namespace_name ("in" quoted_string)? // ^(namespace_name (quoted_string)?)
namespace_name_list            : namespace_name ("," namespace_name)* ";" // namespace_name (namespace_name)*
//****************************
//section declaration
//****************************
block                        : (decl_section)* (block_body)?
block_body                    : compound_statement
                             | assembler_statement
decl_section                  : label_decl_section
                             | const_section
                             | type_section
                             | var_section
                             | exported_proc_heading
                             | method_decl
                             | proc_decl
                             | exports_section
interface_decl                : const_section
                             | type_section
                             | var_section
                             | exported_proc_heading
                             | exports_section
                             | proc_decl
                             | method_decl
label_decl_section             : "label" label ("," label)* ";"
const_section                 : const_key (const_declaration)* // ^(const_key (const_declaration)*)  //CHANGED, erased one const_declaration, for: "const {$include versioninfo.inc }"
const_key                     : "const"
                             | "resourcestring"
const_declaration             : (custom_attribute)? ident (":" type_decl)? "=" const_expression (hinting_directive)* ";" // ident (type_decl)? "=" const_expression
type_section                  : "type" type_declaration (type_declaration)* // ^("type" type_declaration (type_declaration)*)
type_declaration              : (custom_attribute)? generic_type_ident "=" type_decl (hinting_directive)* ";" // ^(tk_new_type (custom_attribute)? ^(generic_type_ident type_decl (hinting_directive)*))
var_section                   : var_key var_declaration (var_declaration)* // ^(var_key var_declaration (var_declaration)*)
var_key                       : "var"
                             | "threadvar"
// threadvar geen initializations alleen globaal
var_declaration               : (custom_attribute)? ident_list_flat ":" type_decl (var_value_spec)? (hinting_directive)* ";" // (custom_attribute)? ^(tk_variable_idents ident_list_flat) ^(tk_variable_type type_decl)
var_value_spec                 : "absolute" ident
                             | "absolute" const_expression
                             | "=" const_expression
exports_section               : "exports" ident export_item ("," ident export_item)* ";"
export_item                   : ("(" (formal_parameter_list)? ")")? (INDEX expression)? (NAME expression)? ("resident")?
//****************************
//section type
//****************************
type_decl                     : struc_type
                             | pointer_type
                             | string_type
                             | procedure_type 
                             | variant_type
                             | ("type")? type_id (generic_postfix)?
                             | simple_type
struc_type                    : ("packed")? struc_type_part // struc_type_part
struc_type_part                : array_type
                             | set_type
                             | file_type
                             | class_decl

array_type                    :  "array" ("[" (array_index)? ("," (array_index)?)* "]")? "of" array_sub_type 
                             // ^(array_sub_type "array" ("[" (array_index)? ("," (array_index)?)* "]")? )          //CHANGED we only need type info

array_index                   : type_id
                             | expression ".." expression

array_sub_type                 : "const"
                             | type_decl
set_type                      : "set" "of" type_decl // "set" type_decl          //CHANGED we only need type info
// set type alleen ordinal of subrange type
file_type                     : "file" ("of" type_decl)?
pointer_type                  : "^" type_decl
                             | "pointer"
string_type                   : "string" ("[" expression "]")? // "string"
                             | ("type")? ANSISTRING (code_page_number)?
code_page_number               : "(" int_num ")"
procedure_type                : method_type
                             | simple_procedure_type
                             | procedure_reference
method_type                   : procedure_type_heading "of" "object"
simple_procedure_type          : procedure_type_heading ( (";")? call_convention_no_semi)?
procedure_reference           : "reference" "to" procedure_type_heading
procedure_type_heading         : "function" (formal_parameter_section)? ":" (custom_attribute)? type_decl // "function" (formal_parameter_section)? ^(tk_function_return type_decl)
                             | "procedure" (formal_parameter_section)?
variant_type                  : "variant" // Sz_j TODO TEMP
simple_type                   : ident
                             | sub_range_type
                             | enum_type
sub_range_type                 : const_expression (".." const_expression)?
enum_type                     : "(" ident ("=" expression)? ("," ident ("=" expression)? )* ")"
type_id                       : namespaced_qualified_ident
//****************************
//section generics
//****************************
generic_type_ident             : qualified_ident (generic_definition)? // qualified_ident    //CHANGED we don"t need <Type> data, it produced empty nodes
generic_definition            : simple_generic_definition
                             | constrained_generic_definition
simple_generic_definition      : "<" ident ("," ident)* ">"
constrained_generic_definition : "<" constrained_generic (";" constrained_generic)* ">"
constrained_generic           : ident (":" generic_constraint ("," generic_constraint)*)?
generic_constraint            : ident
                             | "record"
                             | "class"
                             | "constructor"
generic_postfix               : "<" type_decl ("," type_decl)* ">"
//****************************
//section class
//****************************
class_decl                    : class_type_type_decl
                             | class_type_decl // ^(tk_class class_type_decl)
                             | class_helper_decl // ^(tk_class class_helper_decl)
                             | interface_type_decl // ^(tk_interface interface_type_decl)
                             | object_decl // ^(tk_object object_decl)
                             | record_decl // ^(tk_record record_decl)
                             | record_helper_decl // ^(tk_record_helper record_helper_decl)
class_type_type_decl            : "class" "of" type_id // ^(tk_class_of_type type_id)
class_type_decl                : "class" (class_state)? (class_parent)? (class_item)* "end" // "class" ^(tk_class_parents (class_parent)?) (class_item)*
                             | "class" (class_parent)? // "class" ^(tk_class_parents (class_parent)?)
class_state                   : "sealed"
                             | "abstract"
class_parent                  : "(" generic_type_ident ("," generic_type_ident)* ")" // generic_type_ident (generic_type_ident)*   //CHANGEd from type_id to class_parent_id
class_item                    : visibility
                             | class_method
                             | class_field
                             | class_property
                             | const_section
                             | type_section
                             | ("class")? var_section
class_helper_decl              : "class" "helper" (class_parent)? "for" type_id (class_helper_item)* "end" // ^("class" type_id ) (class_helper_item)* //CHANGED, we only need "for" class name
class_helper_item              : visibility
                             | class_method
                             | class_property
                             | ("class")? var_section
interface_type_decl            : interface_key (class_parent)? (interface_guid)? (interface_item)* "end" // interface_key ^(tk_class_parents (class_parent)? ) ^(tk_guid (interface_guid)?) (interface_item)*
                             | interface_key (class_parent)?  // interface_key ^(tk_class_parents (class_parent)?)
interface_key                 : "interface"
                             | "dispinterface"
interface_guid                : "[" quoted_string "]" // quoted_string
interface_item                : class_method
                             | ("class")? class_property
object_decl                   : "object" (class_parent)? (object_item)* "end" // "object" (class_parent)? (object_item)*
object_item                   : visibility
                             | class_method
                             | class_field
record_decl                   : simple_record
                             | variant_record
simple_record                 : "record" (record_field)* (record_item)* "end" // "record" (record_field)* (record_item)*
variant_record                : "record" (record_field)* record_variant_section "end" // "record" (record_field)* record_variant_section
record_item                   : visibility     //ADDED
                             | class_method
                             | class_property
                             | const_section
                             | type_section
                             | record_field
                             | ("class")? var_section
record_field                  : ident_list ":" type_decl (hinting_directive)* (";")?  //CHANGED not needed ; at the end
                             // ident_list ^(tk_variable_type type_decl)          
record_variant_field           : ident_list ":" type_decl (hinting_directive)* (";") ?
                             // ident_list ^(tk_variable_type type_decl)          
record_variant_section         : "case" (ident ":")? type_decl "of" (record_variant | ";") (record_variant | ";")*
record_variant                : const_expression ("," const_expression)* ":" "(" (record_variant_field)* ")"   //CHANGED to record_variant_field from record_field
record_helper_decl             : "record" "helper" "for" type_id (record_helper_item)* "end"
record_helper_item             : visibility
                             | class_method
                             | class_property
class_method                  : (custom_attribute)? ("class")? method_key ident (generic_definition)? (formal_parameter_section)? ";" (method_directive)*  //  (custom_attribute)? ("class")? ^(method_key ^(TK_FUNCTION_NAME ident) (generic_definition)? ^(TK_FUNCTION_ARGS (formal_parameter_section)?) (method_directive)*)
                             | (custom_attribute)? ("class")? "function" ident (generic_definition)? (formal_parameter_section)? ":" (custom_attribute)? type_decl ";" (method_directive)* // (custom_attribute)? ("class")? ^("function" ^(TK_FUNCTION_NAME ident) (generic_definition)? ^(TK_FUNCTION_ARGS (formal_parameter_section)?) (custom_attribute)? ^(tk_function_return type_decl) (method_directive)*)
                             | (custom_attribute)? ("class")? "operator" ident (generic_definition)? (formal_parameter_section)? ":" (custom_attribute)? type_decl ";" // (custom_attribute)? ("class")? ^("operator" ^(TK_FUNCTION_NAME ident) (generic_definition)? ^(TK_FUNCTION_ARGS (formal_parameter_section)?) (custom_attribute)? type_decl )                             
class_field                   : (custom_attribute)? ident_list ":" type_decl ";" (hinting_directive)* 
                             // (custom_attribute)? ^(tk_class_field ^(tk_variable_idents ident_list) ^(tk_variable_type type_decl))
class_property                : (custom_attribute)? ("class")? "property" ident (class_property_array)? (":" generic_type_ident)? (class_property_index)? (class_property_specifier)* ";" (class_property_end_specifier)*
                              // ^("property" ^(tk_variable_idents ident) ^(tk_variable_type generic_type_ident?) (class_property_specifier)* )
                              // CHANGED added (class_property_specifier)* at end for "default;"
                              // CHANGEDD to generic_type_ident for "property Query_builder : IQuery_builder<Generic_record>"
class_property_array           : "[" formal_parameter_list "]"
class_property_index           : "index" expression (";")?  //CHANGED to (";")?
class_property_specifier       : class_property_read_write   //CHANGED removed ";"
                             | class_property_disp_interface
                             | STORED expression
                             | "default" expression
                             | "default"                // for array properties only (1 per class)
                             | "nodefault"
                             | IMPLEMENTS type_id
class_property_end_specifier    : STORED expression ";"    //ADDED used in class_property at end
                             | "default" expression ";"
                             | "default" ";"             
                             | "nodefault" ";"

class_property_read_write       : "read" qualified_ident ("[" expression "]")?  // Waarom qualified ident???  //ADDED [] // ^("read" qualified_ident)
                             | "write" qualified_ident ("[" expression "]")? //ADDED [] // ^("write" qualified_ident)
class_property_disp_interface   : "readonly" ";"
                             | "writeonly" ";"
                             | disp_id_directive
visibility                   : (STRICT)? "protected" 
                             | (STRICT)? "private"
                             | "public"
                             | "published" 
                             | "automated"     // win32 deprecated
//****************************
//section procedure
//****************************
exported_proc_heading          : "procedure" ident (formal_parameter_section)? ":" (custom_attribute)? type_decl ";" (function_directive)*
                             | "function" ident (formal_parameter_section)? ";" (function_directive)*
method_decl                   : method_decl_heading ";" (method_directive)* (method_body)? // method_decl_heading (method_body)?
method_decl_heading            : (custom_attribute)? ("class")?  method_key method_name (formal_parameter_section)? // (custom_attribute)? ("class")?  ^(method_key ^(TK_FUNCTION_NAME method_name) ^(TK_FUNCTION_ARGS (formal_parameter_section)?) )
                             | (custom_attribute)? ("class")? "function" method_name (formal_parameter_section)? (":" (custom_attribute)? type_decl)? // (custom_attribute)? ("class")? ^("function" ^(TK_FUNCTION_NAME method_name) ^(TK_FUNCTION_ARGS (formal_parameter_section)?) ^(tk_function_return (custom_attribute)? type_decl?) )
                             | (custom_attribute)? "class" "operator" method_name (formal_parameter_section)? (":" (custom_attribute)? type_decl)? // (custom_attribute)? "class" ^("operator" ^(TK_FUNCTION_NAME method_name) ^(TK_FUNCTION_ARGS (formal_parameter_section)?) ^(tk_function_return (custom_attribute)? type_decl?) )              
method_key                    : "procedure"
                             | "constructor"
                             | "destructor"
method_name                   : ident (generic_definition)? ("." ident (generic_definition)?)? "." ident (generic_definition)?                             
proc_decl                     : proc_decl_heading ";" (function_directive)* (proc_body)? // proc_decl_heading (proc_body)?    //CHANGED
proc_decl_heading              : (custom_attribute)? "procedure" ident (formal_parameter_section)?             //CHANGED // ^("procedure" ^(TK_FUNCTION_NAME ident) ^(TK_FUNCTION_ARGS (formal_parameter_section)?) )
                             | (custom_attribute)? "function" ident (formal_parameter_section)? ":" type_decl // ^("function" ^(TK_FUNCTION_NAME ident) ^(TK_FUNCTION_ARGS (formal_parameter_section)?) ^(tk_function_return type_decl) )
formal_parameter_section       : "(" (formal_parameter_list)? ")" // (formal_parameter_list)?
formal_parameter_list          : formal_parameter (";" formal_parameter)* // formal_parameter (formal_parameter)*
formal_parameter              : (custom_attribute)? (parm_type)? ident_list_flat (":" type_decl)? ("=" expression)? // (custom_attribute)? ^(tk_variable_idents ident_list_flat) ^(tk_variable_type type_decl?) ^(tk_variable_param parm_type)?
               //expressions was cut out, beacause we dont have to know default variable values; they were causing troubles with Delphi_code_analyser
parm_type                     : "const"
                             | "var"
                             | "out"
method_body                   : block ";" // block
proc_body                     : "forward" ";" (function_directive)*   // CHECKEN ; en directive plaats!
                             | "external" ("name" expression | "index" expression)* (function_directive)* // CHECKEN directive plaats
                             | block ";"
//****************************
//section custom_attributes
//****************************
custom_attribute              : custom_attribute_list
custom_attribute_list          : (custom_attribute_decl)*
custom_attribute_decl          : "[" namespaced_qualified_ident ("(" (expression_list)? ")")? "]"  // ^(tk_custom_attribute "[" namespaced_qualified_ident ("(" (expression_list)? ")")? "]")                             

//****************************
//section expression
//****************************
expression                   : anonymous_expression // ^(tk_anonymous_expression anonymous_expression)
                             | simple_expression (rel_op simple_expression)? ("=" expression)?   //CHANGED, added expression for: "if( function_call(x, 7+66) = true ) then" syntax                           
anonymous_expression          : "procedure" (formal_parameter_section)? block
                             | "function" (formal_parameter_section)? ":" type_decl block
simple_expression             : factor (operator factor)*
factor                       : "@" factor
                             | "@@" factor       // used to get address of proc var
                             | "not" factor
                             | "+" factor
                             | "-" factor
                             | "^" ident           // geeft volgnummer van letter
                             | int_num
                             | real_num
                             | tk_asm_hex_num          // Alleen in asm statement
                             | "true"
                             | "false"
                             | "nil"
                             | "(" expression ")" ("^")? ("." expression)?        //CHANGED, added  ("^")? ("." qualified_ident)?
                             | string_factor
                             | set_section
                             | designator
                             | type_id "(" expression ")"
string_factor                 : control_string (quoted_string control_string)* (quoted_string)?
                             | quoted_string (control_string quoted_string)* (control_string)?
set_section                   : "[" (expression (("," | "..") expression)*)? "]"

designator                   : ("inherited")? ( (namespaced_qualified_ident | type_id) )? (designator_item)*
designator_item               : "^"
                             | ("." | "@") ident              //CHANGED added "@"
                             | ("<" generic_type_ident ("," generic_type_ident)* ">")       //ADDED for proc<sth, sth>.foo;
                             | "[" expression_list "]"
                             | "(" (expression (colon_construct)? ("," expression (colon_construct)?)*)? ")" // "(" (expression (colon_construct)? (expression (colon_construct)?)*)? ")"
expression_list               : expression ("," expression)*
colon_construct               : ":" expression (":" expression)?
// Alleen voor Write/Write_ln.
operator                     : "+"
                             | "-"
                             | "or"
                             | "xor"
                             | "*"
                             | "/"
                             | "div"
                             | "mod"
                             | "and"
                             | "shl"
                             | "shr"
                             | "as"
rel_op                        : "<"
                             | ">"
                             | "<="
                             | ">="
                             | "<>"
                             | "="
                             | "in"
                             | "is"
//****************************
//section statement
//****************************

statement                    : if_statement
                             | case_statement
                             | repeat_statement
                             | while_statement
                             | for_statement
                             | with_statement
                             | try_statement
                             | raise_statement
                             | assembler_statement
                             | compound_statement
                             | label ":" statement
                             | simple_statement
if_statement                  : "if" expression "then" statement ("else" statement)? 
case_statement                : "case" expression "of" (case_item)* ("else" statement_list (";")?)? "end"
case_item                     : case_label ("," case_label)* ":" statement (";")? // checken of ; sep of scheider is
case_label                    : expression (".." expression)?
repeat_statement              : "repeat" (statement_list)? "until" expression
while_statement               : "while" expression "do" statement
for_statement                 : "for" designator ":=" expression "to" expression "do" statement
                             | "for" designator ":=" expression "downto" expression "do" statement
                             | "for" designator "in" expression "do" statement
with_statement                : "with" with_item "do" statement
with_item                     : designator "as" designator       //ADDED
                             | designator ("," designator)*
compound_statement            : "begin" (statement_list)? "end" // ^("begin" (statement_list)? "end")
statement_list                : (statement)? (";" (statement)?)*
simple_statement              : designator ":=" expression
                             | designator // call
                             | goto_statement
goto_statement                : "goto" label
                             | "exit" ("(" expression ")")?   
                             | "break"                          
                             | "continue"
//****************************
//section const_expression
//****************************
const_expression              : "(" record_const_expression (";" record_const_expression)* ")" //CHANGED reversed order
                             | "(" const_expression ("," const_expression)* ")"
                             | expression
record_const_expression        : ident ":" const_expression
//****************************
//section exception_statement
//****************************
try_statement                 : "try" (statement_list)? "except" handler_list "end"  
                             | "try" (statement_list)? "finally" (statement_list)? "end"
handler_list                  : (handler)* ("else" statement_list)?
                             | statement_list
handler                      : "on" (handler_ident)? type_id "do" handler_statement  //CHANGED - ; is not required ; handler_ident not required, example:  "on einvalidoperation do;"
handler_ident                 : ident ":"
handler_statement             : statement (";")?
                             | ";"
raise_statement               : "raise" (designator)? (AT designator)? // CHECKEN!           
//****************************
//section Assembler_statement
//****************************
assembler_statement           : "asm" ("end")* "end"    //ADDED we don"t realy care about assembler statements, since they don"t contribute to                //any measure, just skip, allow all
//****************************
//section directive
//****************************
method_directive              : reintroduce_directive         // 1
                             | overload_directive            // 2
                             | binding_directive             // 3
                             | abstract_directive            // 3 virtual;
                             | inline_directive              // 4 niet virtual or dynamic
                             | call_convention               // 4
                             | hinting_directive ";"       // 4 (niet abstract)
                             | old_call_convention_directive   // 1
                             | disp_id_directive
function_directive            : overload_directive          // 1
                             | inline_directive            // 1
                             | call_convention             // 1
                             | old_call_convention_directive // 1
                             | hinting_directive ";"      // 1
                             | (call_convention_no_semi)? external_directive          // 1
                             | "unsafe" ";"              // 1 .net?
reintroduce_directive         : "reintroduce" ";"
overload_directive            : "overload" (";")?    //CHANGE ; not needed
binding_directive             : "message" expression ";"
                             | "static" ";"
                             | "dynamic" ";"
                             | "override" ";"
                             | "virtual" ";"
abstract_directive            : "abstract" ";"
                             | "final" ";"
inline_directive              : "inline" ";"
                             | "assembler" ";" // deprecated
call_convention               : "cdecl" ";"    //
                             | "pascal" ";"   //
                             | "register" ";" //
                             | "safecall" ";" //
                             | "stdcall" ";"  //
                             | "export" ";"   // deprecated
call_convention_no_semi         : "cdecl"    //    //ADDED for procedure_type error fixing, without ";" at the end
                             | "pascal"   //
                             | "register" //
                             | "safecall" //
                             | "stdcall"  //
                             | "export"   // deprecated
old_call_convention_directive   : "far" ";"      // deprecated
                             | "local" ";"    // niet in windows maakt functie niet exporteerbaar
                             | "near" ";"     // deprecated
hinting_directive             : "deprecated" (string_factor)?
                             | "experimental"  // added 2006
                             | "platform"
                             | "library"
external_directive            : "varargs" ";"   // alleen bij external cdecl
                             | "external" ";"
                             | "external" const_expression (external_specifier)* ";" // expression : dll name
external_specifier            : "name" const_expression
                             | "index" const_expression   // specific to a platform
disp_id_directive              : "dispid" expression ";"
//****************************
////section general
//****************************
ident                        : IDENTIFIER
                             | "&" IDENTIFIER
                             | used_keywords_as_names                 
used_keywords_as_names          : (NAME | READONLY | ADD | AT | MESSAGE | POINTER | INDEX | DEFAULT | STRING | CONTINUE)
                             | (READ | WRITE | REGISTER | VARIANT | OPERATOR | REMOVE | LOCAL | REFERENCE | CONTAINS | FINAL)
                             | (BREAK | EXIT | STRICT | OUT | OBJECT | EXPORT | ANSISTRING | IMPLEMENTS | STORED)                           
ident_list                    : ident ("," ident)* // ^(ident (ident)*)
ident_list_flat                : ident ("," ident)* // ident (ident)*   //ADDED used in formal_paremeter                                                          
label                        : IDENTIFIER
                             | INTEGER
                             | tk_hex_num
                             | used_keywords_as_names
int_num                       : INTEGER
                             | tk_hex_num                             
real_num                      : REAL                             
namespaced_qualified_ident     : (namespace_name ".")? qualified_ident
namespace_name                : ident ("." ident)*
qualified_ident               :  (ident ".")*  ident   //must stay the way it is, with "." for proper class method identyfication
                                   
// KEYWORDS
ABSOLUTE          : "absolute"       
ABSTRACT          : "abstract"       
ADD               : "add"            
AND               : "and"            
ANSISTRING        : "ansistring"     
ARRAY             : "array"          
AS                : "as"             
ASM               : "asm"            
ASSEMBLER         : "assembler"      
ASSEMBLY          : "assembly"       
AT                : "at"             
AUTOMATED         : "automated"      
BEGIN             : "begin"          
BREAK             : "break"          
CASE              : "case"           
CDECL             : "cdecl"          
CLASS             : "class"          
CONST             : "const"          
CONSTRUCTOR       : "constructor"    
CONTAINS          : "contains"       
CONTINUE          : "continue"       
DEFAULT           : "default"        
DEPRECATED        : "deprecated"     
DESTRUCTOR        : "destructor"     
DISPID            : "dispid"         
DISPINTERFACE     : "dispinterface"  
DIV               : "div"            
DO                : "do"             
DOWNTO            : "downto"         
DQ                : "dq"             
DW                : "dw"             
DYNAMIC           : "dynamic"        
ELSE              : "else"           
END               : "end"            
EXCEPT            : "except"         
EXIT              : "exit"           
EXPERIMENTAL      : "experimental"   
EXPORT            : "export"         
EXPORTS           : "exports"        
EXTERNAL          : "external"       
FAR               : "far"            
FILE              : "file"           
FINAL             : "final"          
FINALIZATION      : "finalization"   
FINALLY           : "finally"        
FOR               : "for"            
FORWARD           : "forward"        
FUNCTION          : "function"       
GOTO              : "goto"           
HELPER            : "helper"         
IF                : "if"             
IMPLEMENTATION    : "implementation" 
IMPLEMENTS        : "implements"     
IN                : "in"             
INDEX             : "index"          
INHERITED         : "inherited"      
INITIALIZATION    : "initialization" 
INLINE            : "inline"         
INTERFACE         : "interface"      
IS                : "is"             
LABEL             : "label"          
LIBRARY           : "library"        
LOCAL             : "local"          
MESSAGE           : "message"        
MOD               : "mod"            
NAME              : "name"           
NEAR              : "near"           
NIL               : "nil"            
NODEFAULT         : "nodefault"      
NOT               : "not"            
OBJECT            : "object"         
OF                : "of"             
ON                : "on"             
OPERATOR          : "operator"       
OR                : "or"             
OUT               : "out"            
OVERLOAD          : "overload"       
OVERRIDE          : "override"       
PACKAGE           : "package"        
PACKED            : "packed"         
PASCAL            : "pascal"         
PLATFORM          : "platform"       
POINTER           : "pointer"        
PRIVATE           : "private"        
PROCEDURE         : "procedure"      
PROGRAM           : "program"        
PROPERTY          : "property"       
PROTECTED         : "protected"      
PUBLIC            : "public"         
PUBLISHED         : "published"      
RAISE             : "raise"          
READ              : "read"           
READONLY          : "readonly"       
RECORD            : "record"         
REFERENCE         : "reference"      
REGISTER          : "register"       
REINTRODUCE       : "reintroduce"    
REMOVE            : "remove"         
REPEAT            : "repeat"         
REQUIRES          : "requires"       
RESIDENT          : "resident"       
RESOURCESTRING    : "resourcestring" 
SAFECALL          : "safecall"       
SEALED            : "sealed"         
SET               : "set"            
SHL               : "shl"            
SHR               : "shr"            
STATIC            : "static"         
STDCALL           : "stdcall"        
STORED            : "stored"         
STRICT            : "strict"         
STRING            : "string"         
THEN              : "then"           
THREADVAR         : "threadvar"      
TO                : "to"             
TRY               : "try"            
TYPE              : "type"           
UNIT              : "unit"           
UNSAFE            : "unsafe"         
UNTIL             : "until"          
USES              : "uses"           
VAR               : "var"            
VARARGS           : "varargs"        
VARIANT           : "variant"        
VIRTUAL           : "virtual"        
WHILE             : "while"          
WITH              : "with"           
WRITE             : "write"          
WRITEONLY         : "writeonly"      
XOR               : "xor"            
FALSE             : "false"          
TRUE              : "true"           

//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------
PLUS              : "+"   
MINUS             : "-"   
STAR              : "*"   
SLASH             : "/"   
ASSIGN            : ":="  
COMMA             : ","   
SEMI              : ";"   
COLON             : ":"   
EQUAL             : "="   
NOT_EQUAL         : "<>"  
LT                : "<"   
LE                : "<="  
GE                : ">="  
GT                : ">"   
LPAREN            : "("   
RPAREN            : ")"   
LBRACK            : "["    // line_tab[line]
LBRACK2           : "(."   // line_tab(.line.)
RBRACK            : "]"   
RBRACK2           : ".)"  
POINTER2          : "^"   
AT2               : "@"   
DOT               : "."   // ("." {$set_type(DOTDOT)})?  
DOTDOT            : ".."    

//****************************
//section token
//****************************
TK_GLOBAL_FUNCTION        : "FUNCTION_GLOBAL"
                        
TK_FUNCTION_NAME          : "FUNCTION_NAME"
                        
TK_FUNCTION_ARGS          : "FUNCTION_ARGS"
                        
tk_function_body          : "FUNCTION_BODY"
                        
tk_function_return        : "FUNCTION_RETURN"
                        
tk_custom_attribute       : "CUSTOM_ATTRIBUTE"
                        
tk_custom_attribute_args   : "CUSTOM_ATTRIBUTE_ARGS"
                        
tk_new_type               : "NEW_TYPE"
                        
tk_class                 : "CLASS"
                        
tk_record                : "RECORD_TYPE"
                        
tk_record_helper          : "RECORD_HELPER"
                        
tk_interface             : "INTERFACE_TYPE"
                        
tk_object                : "OBJECT_TYPE"
                        
tk_class_of_type           : "CLASS_OF_TYPE"
                        
tk_variable_type          : "VARIABLE_TYPE"
                        
tk_variable_idents        : "VARIABLE_IDENTS"
                        
tk_variable_param         : "VARIABLE_PARAM"
                        
tk_guid                  : "INTERFACE_GUID"
                        
tk_class_parents          : "CLASS_PARENTS"
                        
tk_class_field            : "CLASS_FIELD"
                        
tk_anonymous_expression   : "ANONYMOUS_EXPRESSION"

IDENTIFIER : CNAME

INTEGER                : INT
                        
REAL               : FLOAT
                        
tk_hex_num                : "$" HEXDIGIT+
                        
tk_asm_hex_num             :  HEXDIGIT+ ("h"|"H")
                        
tk_asm_hex_label           :  HEXDIGIT+ ":"
                        
quoted_string            : "\"" ("\"\"" | ("\""))* "\""   //taken from PASCAL grammar
                        
control_string           : control_char (control_char)*
                                      
control_char             : "#" INT
                        | "#" "$" HEXDIGIT+
                                  

%ignore WS
                        
COMMENT                 :  "(*" /(.|\n|\r)+/ "*)"     
                        |  "{" /(.|\n|\r)+/ "}"      
                        |  "//" /(.)+/ NEWLINE

%ignore COMMENT 

// %include common.g doesn't work yet, so cut&paste it below 

//
// Numbers
//

DIGIT: "0".."9"
HEXDIGIT: "a".."f"|"A".."F"|DIGIT

INT: DIGIT+
SIGNED_INT: ["+"|"-"] INT
DECIMAL: INT "." INT? | "." INT

// float = /-?\d+(\.\d+)?([eE][+-]?\d+)?/
_EXP: ("e"|"E") SIGNED_INT
FLOAT: INT _EXP | DECIMAL _EXP?
SIGNED_FLOAT: ["+"|"-"] INT

NUMBER: FLOAT | INT
SIGNED_NUMBER: ["+"|"-"] NUMBER

//
// Strings
//
STRING_INNER: ("\\\""|/[^"]/)
ESCAPED_STRING: "\"" STRING_INNER* "\""


//
// Names (Variables)
//
LCASE_LETTER: "a".."z"
UCASE_LETTER: "A".."Z"
UNICODE_CHAR:  "\u0080".."\ufffe"

LETTER: UCASE_LETTER | LCASE_LETTER | UNICODE_CHAR
WORD: LETTER+

CNAME: ("_"|LETTER) ("_"|LETTER|DIGIT)*


//
// Whitespace
//
WS_INLINE: (" "|/\t/)+
WS: /[ \t\f\r\n]/+

CR : /\r/
LF : /\n/
NEWLINE: (CR? LF)+
                    



                                                     
                 
