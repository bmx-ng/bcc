' Copyright (c) 2013-2015 Bruce A Henderson
'
' Based on the public domain Monkey "trans" by Mark Sibly
'
' This software is provided 'as-is', without any express or implied
' warranty. In no event will the authors be held liable for any damages
' arising from the use of this software.
'
' Permission is granted to anyone to use this software for any purpose,
' including commercial applications, and to alter it and redistribute it
' freely, subject to the following restrictions:
'
'    1. The origin of this software must not be misrepresented; you must not
'    claim that you wrote the original software. If you use this software
'    in a product, an acknowledgment in the product documentation would be
'    appreciated but is not required.
'
'    2. Altered source versions must be plainly marked as such, and must not be
'    misrepresented as being the original software.
'
'    3. This notice may not be removed or altered from any source
'    distribution.
'
SuperStrict

Import "config.bmx"

Include "translator.bmx"
Include "decl.bmx"
Include "expr.bmx"
Include "stmt.bmx"

Type TType

	Method ActualType:TType()
		Return Self
	End Method

	Method EqualsType:Int( ty:TType )
		Return False
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Return EqualsType( ty )
	End Method
	
	Method Semant:TType(option:Int = False)
		Return Self
	End Method

	Method GetClass:TClassDecl()
		Return Null
	End Method
	
	Method ToString$()
		Return "??Type??"
	End Method
	
	Method GetSize:Int()
		Return WORD_SIZE
	End Method

	Method ArrayOf:TArrayType()
		If Not _arrayOf Then
			_arrayOf=New TArrayType.Create( Self )
		End If
		Return _arrayOf
	End Method

	Method OnCopy:TType() Abstract
	
	Global voidType:TVoidType=New TVoidType
	Global emptyArrayType:TArrayType=New TArrayType.Create( voidType )
	Global objectType:TIdentType=New TIdentType.Create( "brl.classes.object" )
	Global nullObjectType:TNullType=New TNullType
	Global stringType:TStringType=New TStringType

	Rem
	bbdoc: map to a pointer type
	End Rem
	Function MapToPointerType:TType(ty:TType)
		If ty = stringType Then
			ty = ty.Copy()
		End If
		
		Local flag:Int = T_POINTER & ty._flags
		
		If flag & T_PTR Then
			ty._flags :~ T_PTR
			ty._flags :| T_PTRPTR
			Return ty
		End If

		If flag & T_PTRPTR Then
			ty._flags :~ T_PTRPTR
			ty._flags :| T_PTRPTRPTR
			Return ty
		End If

		ty._flags :| T_PTR
		Return ty
		
	End Function

	Function MapToVarType:TType(ty:TType)
		If ty = stringType Then
			ty = ty.Copy()
		End If
		
		If Not (ty._flags & T_VAR) Then
			ty._flags :| T_VAR
			Return ty
		End If
		
		' TODO : error if already mapped?
		
		Return ty
	End Function

	Rem
	bbdoc: map to a var pointer type
	End Rem
	Function MapToVarPointerType:TType(ty:TType)
		If ty = stringType Then
			ty = ty.Copy()
		End If

		If Not (ty._flags & T_VARPTR) Then
			ty._flags :| T_VARPTR
			Return ty
		End If

		Return Null
	End Function
	
	Function MapPointerToPrim:TType(ty:TNumericType)
		Local nty:TType = ty.Copy()

		If ty._flags & T_PTRPTRPTR Then
			nty._flags :~ T_PTRPTRPTR
			nty._flags :| T_PTRPTR
		Else If ty._flags & T_PTRPTR Then
			nty._flags :~ T_PTRPTR
			nty._flags :| T_PTR
		Else If ty._flags & T_PTR Then
			nty._flags :~ T_PTR
		End If

		Return nty
	End Function

	Field _arrayOf:TArrayType
	
	' one or more of
	' T_VAR, T_VARPTR, T_PTR, T_PTRPTR, T_PTRPTRPTR
	Field _flags:Int
	
	Const T_VAR:Int = $01
	Const T_VARPTR:Int = $02
	Const T_PTR:Int = $04
	Const T_PTRPTR:Int = $08
	Const T_PTRPTRPTR:Int = $10

	' for strings
	Const T_CHAR_PTR:Int  = $1000
	Const T_SHORT_PTR:Int = $2000

	Const T_POINTER:Int = T_PTR | T_PTRPTR | T_PTRPTRPTR

	Const T_BYTE:Int        = $001
	Const T_SHORT:Int       = $002
	Const T_INT:Int         = $004
	Const T_LONG:Int        = $008
	Const T_FLOAT:Int       = $010
	Const T_DOUBLE:Int      = $020
	Const T_STRING:Int      = $040
	Const T_ARRAY:Int       = $080
	Const T_FUNCTIONPTR:Int = $100


	Method Copy:TType()
		Local ty:TType = OnCopy()
		ty._flags = _flags
		ty._arrayOf = _arrayOf
		Return ty
	End Method
	
	Method ToStringParts:String()
		Local s:String

		If _flags & T_PTR Then
			s:+ " Ptr"
		Else If _flags & T_PTRPTR Then
			s:+ " Ptr Ptr"
		Else If _flags & T_PTRPTRPTR Then
			s:+ " Ptr Ptr Ptr"
		End If

		If _flags & T_VAR Then
			s:+ " Var"
		End If
		
		Return s
	End Method
	
End Type

Function NewType:TType(kind:Int = 0)
	Local ty:TType
	
	Select kind
		Case TType.T_BYTE
			ty = New TByteType
		Case TType.T_SHORT
			ty = New TShortType
		Case TType.T_INT
			ty = New TIntType
		Case TType.T_LONG
			ty = New TLongType
		Case TType.T_FLOAT
			ty = New TFloatType
		Case TType.T_DOUBLE
			ty = New TDoubleType
		Case TType.T_STRING
			ty = New TStringType
		Case TType.T_ARRAY
			ty = New TArrayType
		Case TType.T_FUNCTIONPTR
			ty = New TFunctionPtrType
		Default
			Err "Don't have a pointer type for " + kind
	End Select

	Return ty
End Function

Function NewPointerType:TType(kind:Int = 0)
	Local ty:TType = NewType(kind)

	Return TType.MapToPointerType(ty)
End Function

Function IsPointerType:Int(ty:TType, kind:Int = 0, pType:Int = TType.T_PTR)
' TODO : 
	If kind Then
		If IsType(ty, kind) Then
			Return ty._flags & pType
		Else
			Return False
		End If
	Else
		Return ty._flags & pType
	End If
	
End Function

Function IsNumericType:Int(ty:TType)
	Return (TNumericType(ty) <> Null) And Not IsPointerType(ty, 0, TType.T_POINTER)
End Function

Function IsType:Int(ty:TType, kind:Int)
	Select kind
		Case TType.T_BYTE
			Return TByteType(ty) <> Null
		Case TType.T_SHORT
			Return TShortType(ty) <> Null
		Case TType.T_INT
			Return TIntType(ty) <> Null
		Case TType.T_LONG
			Return TLongType(ty) <> Null
		Case TType.T_FLOAT
			Return TFloatType(ty) <> Null
		Case TType.T_DOUBLE
			Return TDoubleType(ty) <> Null
		Case TType.T_STRING
			Return TStringType(ty) <> Null
		Case TType.T_ARRAY
			Return TArrayType(ty) <> Null
		Case TType.T_FUNCTIONPTR
			Return TFunctionPtrType(ty) <> Null
	End Select

	Return False
End Function

Type TVoidType Extends TType

	Method EqualsType:Int( ty:TType )
		If opt_issuperstrict Then
			Return TVoidType( ty )<>Null
		Else
			Return TVoidType( ty )<>Null Or TIntType( ty) <> Null
		End If
	End Method
	
	Method ToString$()
		Return "Void"
	End Method

	Method OnCopy:TType()
		Return New TVoidType
	End Method

End Type

Type TNullType Extends TType

	Method EqualsType:Int( ty:TType )
		Return False
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Return True
	End Method
	
	Method ToString$()
		Return "NULL"
	End Method

	Method OnCopy:TType()
		Return New TNullType
	End Method

End Type

Type TBoolType Extends TType

	Method EqualsType:Int( ty:TType )
		Return TBoolType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Return TNumericType( ty )<>Null Or TBoolType( ty )<>Null Or TStringType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Bool"
	End Method

	Method GetSize:Int()
		Return 4
	End Method

	Method OnCopy:TType()
		Return New TBoolType
	End Method

End Type

Type TNumericType Extends TType


	Method ToPointer:TType()
		Local ty:TType = Copy()
		Return MapToPointerType(ty)
	End Method
	
End Type

Type TIntType Extends TNumericType
	
	Method EqualsType:Int( ty:TType )
		Return TIntType( ty )<>Null And (_flags = ty._flags Or (_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR)) ' varptr = ptr
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		If _flags & T_VARPTR And (TIntType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null 'Or TIntVarPtrType( ty )<> Null
	End Method
	
	Method OnCopy:TType()
		Return New TIntType
	End Method

	Method ToString$()
		Return "Int" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return 4
	End Method

End Type

Type TByteType Extends TNumericType
	
	Method EqualsType:Int( ty:TType )
		Return TByteType( ty )<>Null And (_flags = ty._flags Or (_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR)) ' varptr = ptr
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		If (_flags & T_VARPTR) And (TByteType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null 'Or TByteVarPtrType( ty )<> Null
	End Method

	Method OnCopy:TType()
		Return New TByteType
	End Method

	Method ToString$()
		Return "Byte" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return 1
	End Method

End Type

Type TShortType Extends TNumericType

	Method EqualsType:Int( ty:TType )
		Return TShortType( ty )<>Null And (_flags = ty._flags Or (_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR)) ' varptr = ptr
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		If _flags & T_VARPTR And (TShortType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null 'Or TShortVarPtrType( ty )<> Null
	End Method
	
	Method OnCopy:TType()
		Return New TShortType
	End Method

	Method ToString$()
		Return "Short" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return 2
	End Method

End Type

Type TLongType Extends TNumericType ' BaH Long
	
	Method EqualsType:Int( ty:TType )
		Return TLongType( ty )<>Null And (_flags = ty._flags Or (_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR)) ' varptr = ptr
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		If _flags & T_VARPTR And (TLongType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null 'Or TLongVarPtrType( ty )<> Null
	End Method
	
	Method OnCopy:TType()
		Return New TLongType
	End Method

	Method ToString$()
		Return "Long" + ToStringParts()
	End Method
End Type

Type TDecimalType Extends TNumericType
End Type

Type TFloatType Extends TDecimalType
	
	Method EqualsType:Int( ty:TType )
		Return TFloatType( ty )<>Null And (_flags = ty._flags Or (_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR)) ' varptr = ptr
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf	
		If _flags & T_VARPTR And (TFloatType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null 'Or TFloatVarPtrType( ty )<> Null
	End Method

	Method OnCopy:TType()
		Return New TFloatType
	End Method
	
	Method ToString$()
		Return "Float" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return 4
	End Method

End Type

Type TDoubleType Extends TDecimalType
	
	Method EqualsType:Int( ty:TType )
		Return TDoubleType( ty )<>Null And (_flags = ty._flags Or (_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR)) ' varptr = ptr
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf	
		If _flags & T_VARPTR And (TDoubleType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null 'Or TDoubleVarPtrType( ty )<> Null
	End Method

	Method OnCopy:TType()
		Return New TDoubleType
	End Method

	Method ToString$()
		Return "Double" + ToStringParts()
	End Method

End Type

Type TStringType Extends TType

	Field cdecl:TClassDecl

	Method EqualsType:Int( ty:TType )
		Return TStringType( ty )<>Null And _flags = ty._flags
	End Method

	Method ExtendsType:Int( ty:TType )	
		Return EqualsType( ty ) Or (TObjectType( ty ) And TObjectType( ty ).classDecl.ident="Object") Or (TStringType(ty) And (_flags & T_VAR)) ..
			Or (TStringType(ty) And (ty._flags & T_VAR)) Or (TStringType(ty) And (ty._flags & T_CHAR_PTR)) Or (TStringType(ty) And (ty._flags & T_SHORT_PTR)) ..
			Or IsPointerType(ty) Or (TStringType(ty) And (_flags & T_CHAR_PTR)) Or (TStringType(ty) And (_flags & T_SHORT_PTR))
	End Method
	
	Method GetClass:TClassDecl()
		If cdecl Return cdecl
		
		Local modid$="brl.classes"
		Local mdecl:TModuleDecl=_env.FindModuleDecl( modid )
		If Not mdecl Err "Module '"+modid+"' not found"
		'clsid=ident[i+1..] ' BaH
	'DebugStop
		cdecl=TClassDecl(mdecl.FindDecl( "string" ))

		'Return _env.FindClassDecl( "brl.classes.string" )
		Return cdecl
	End Method
	
	Method Semant:TType(option:Int = 0)
		GetClass()
		Return Self
	End Method

	Method OnCopy:TType()
		Local ty:TStringType = New TStringType
		ty.cdecl = cdecl
		If _flags & T_CHAR_PTR Then
			ty._flags :| T_CHAR_PTR
		End If
		If _flags & T_SHORT_PTR Then
			ty._flags :| T_SHORT_PTR
		End If
		Return ty
	End Method

	Method ToString$()
		Return "String" + ToStringParts()
	End Method
End Type

Type TArrayType Extends TType
	Field elemType:TType
	Field dims:Int
	
	Method Create:TArrayType( elemType:TType, dims:Int = 1 )
		Self.elemType=elemType
		Self.dims = dims
		Return Self
	End Method
	
	Method ActualType:TType()
		Local ty:TType=elemType.ActualType()
		If ty=elemType Return Self
		Return New TArrayType.Create( ty )
	End Method
		
	Method EqualsType:Int( ty:TType )
		Local arrayType:TArrayType=TArrayType( ty )
		Return arrayType And elemType.EqualsType( arrayType.elemType )
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Local arrayType:TArrayType=TArrayType( ty )
		Return (arrayType And ( TVoidType( elemType ) Or elemType.EqualsType( arrayType.elemType ) Or elemType.ExtendsType( arrayType.elemType ) )) Or IsPointerType(ty, 0, TType.T_POINTER) <> Null Or (TObjectType( ty ) And TObjectType( ty ).classDecl.ident="Object")
	End Method
	
	Method Semant:TType(option:Int = False)
		Local ty:TType=elemType.Semant()
		If ty<>elemType Return New TArrayType.Create( ty, dims )
		Return Self
	End Method
	
	Method GetClass:TClassDecl()
		'Return _env.FindClassDecl( "array" )
		Return TClassDecl( _env.FindDecl( "___array" ) )
	End Method

	Method OnCopy:TType()
		Local ty:TArrayType = New TArrayType
		ty.elemType = elemType
		ty.dims = dims
		Return ty
	End Method

	Method ToString$()
		Return elemType.ToString()+"[]"
	End Method
End Type

Type TObjectType Extends TType
	Field classDecl:TClassDecl
	
	Method Create:TObjectType( classDecl:TClassDecl )
		Self.classDecl=classDecl
		Return Self
	End Method
	
	Method ActualType:TType()
		If classDecl.actual=classDecl Return Self
		Return New TObjectType.Create( TClassDecl(classDecl.actual) )
	End Method
	
	Method EqualsType:Int( ty:TType )
		Local objty:TObjectType=TObjectType( ty )
		Return TNullDecl(classDecl) <> Null Or (objty And (classDecl=objty.classDecl Or classDecl.ExtendsClass( objty.classDecl ))) 'Or TObjectVarPtrType(ty) <> Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Local objty:TObjectType=TObjectType( ty )
		If objty Return classDecl.ExtendsClass( objty.classDecl )
		If IsPointerType( ty, T_BYTE ) Return True
		' we only "extend" String if we are a basic Object
		If TStringType( ty ) And classDecl.ident="Object" Then
			Return True
		Else
			Return False
		EndIf
	End Method
	
	Method GetClass:TClassDecl()
		Return classDecl
	End Method
	
	Method ToString$()
		Return classDecl.ToString()
	End Method

	Method OnCopy:TType()
		Local ty:TObjectType = New TObjectType
		ty.classDecl = classDecl
		Return ty
	End Method

End Type

Type TIdentType Extends TType
	Field ident$
	Field args:TType[]
	
	Method Create:TIdentType( ident$,args:TType[] = Null )
		Self.ident=ident
		If args = Null Then
			Self.args = New TType[0]
		Else
			Self.args=args
		End If
		Return Self
	End Method
	
	Method CopyToDest:TIdentType(dst:TIdentType)
		dst.ident = ident
		dst.args = args
		Return dst
	End Method

	Method CopyToPointer:TIdentType(dst:TIdentType)
		dst = TIdentType(MapToPointerType(dst))
		dst.ident = ident
		dst.args = args
		Return dst
	End Method
	
	Method ActualType:TType()
		InternalErr
	End Method
	
	Method EqualsType:Int( ty:TType )
		InternalErr
	End Method
	
	Method ExtendsType:Int( ty:TType )
		InternalErr
	End Method
	
	'Method Semant:TType()
	'	If ident Return New TObjectType.Create( FindClass() )
	'	Return New TObjectType.Create( TClassDecl.nullObjectClass )
	'End Method
	
	
	Method Semant:TType(ignoreNotFoundError:Int = 0)
'If ident="obj" DebugStop
		If Not ident Return TType.nullObjectType

		Local targs:TType[args.Length]
		For Local i:Int=0 Until args.Length
			targs[i]=args[i].Semant()
		Next
		
		Local tyid$,ty:TType
		Local i:Int=ident.FindLast( "." )
		
		If i=-1
			tyid=ident
			ty=_env.FindType( tyid,targs )

			' finally scan all modules for it
			If Not ty Then
				For Local mdecl:TModuleDecl = EachIn _appInstance.globalImports.Values()
					ty=mdecl.FindType( tyid,targs )
					If ty Exit
				Next
			End If
		Else
			i = ident.Find( "." )
						
			' try scope search first
			tyid=ident[..i]
			ty=_env.FindType( tyid,targs )
			
			If Not ty Then
				i = ident.FindLast( "." )
		
				' try scope search first
				tyid=ident[..i]
				ty=_env.FindType( tyid,targs )				

				If Not ty Then
					' no? now try module search
					Local modid$=ident[..i]
					Local mdecl:TModuleDecl=_env.FindModuleDecl( modid )
					If Not mdecl Err "Module '"+modid+"' not found"
					tyid=ident[i+1..]
					ty=mdecl.FindType( tyid,targs )
				End If
			End If
		EndIf
		If Not ty Then
			If ignoreNotFoundError Then
				Return Null
			End If
			Err "Type '"+tyid+"' not found"
		End If
		
		If (_flags & T_VAR) And TObjectType(ty) Then
			ty = New TObjectType.Create(TObjectType(ty).classDecl)
			ty._flags :| T_VAR
		End If

		If (_flags & T_POINTER) And TObjectType(ty) Then
			ty = New TObjectType.Create(TObjectType(ty).classDecl)
			ty._flags :| (_flags & T_POINTER)
		End If
		
		Return ty
	End Method

	Method SemantClass:TClassDecl()
		Local ty:TObjectType=TObjectType( Semant() )
		If Not ty Err "Type is not a class"
		Return ty.classDecl
	End Method

	Method ToString$()
		Local t$
		For Local arg:TIdentType=EachIn args
			If t t:+","
			t:+arg.ToString()
		Next
		If t Return "$"+ident+"<"+t.Replace("$","")+">"
		Return "$"+ident
	End Method

	Method OnCopy:TType()
		Local ty:TIdentType = New TIdentType
		ty.ident = ident
		ty.args = args
		Return ty
	End Method

End Type

Type TExternObjectType Extends TType
	Field classDecl:TClassDecl
	
	Method Create:TExternObjectType( classDecl:TClassDecl )
		Self.classDecl=classDecl
		Return Self
	End Method
	
	Method ActualType:TType()
		If classDecl.actual=classDecl Return Self
		Return New TExternObjectType.Create( TClassDecl(classDecl.actual) )
	End Method
	
	Method EqualsType:Int( ty:TType )
		Local objty:TObjectType=TObjectType( ty )
		Return TNullDecl(classDecl) <> Null Or (objty And (classDecl=objty.classDecl Or classDecl.ExtendsClass( objty.classDecl ))) Or TObjectType(ty)
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Local objty:TObjectType=TObjectType( ty )
		If objty Return classDecl.ExtendsClass( objty.classDecl )
		If IsPointerType( ty, T_BYTE ) Return True
		Local op$
		If TBoolType( ty )
			op="ToBool"
		Else If TIntType( ty ) 
			op="ToInt"
		Else If TFloatType( ty )
			op="ToFloat"
		Else If TStringType( ty )
			op="ToString"
		Else If TLongType( ty ) ' BaH Long
			op="ToLong"
		Else
			Return False
		EndIf
		Local fdecl:TFuncDecl=GetClass().FindFuncDecl( op,Null,True )
		Return fdecl And fdecl.IsMethod() And fdecl.retType.EqualsType( ty )
	End Method
	
	Method GetClass:TClassDecl()
		Return classDecl
	End Method
	
	Method ToString$()
		Return classDecl.ToString()
	End Method

	Method OnCopy:TType()
		Local ty:TExternObjectType = New TExternObjectType
		ty.classDecl = classDecl
		Return ty
	End Method
End Type

Type TFunctionPtrType Extends TType

	Field func:TFuncDecl

	Method EqualsType:Int( ty:TType )
' TODO : compare function decl
		Return TFunctionPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return IsPointerType( ty, 0, T_POINTER )<>Null
	End Method
	
	Method equalsDecl:Int(fdecl:TFuncDecl)
		func.Semant
		fdecl.Semant
	
		' same number of args?
		If func.argDecls.length <> fdecl.argDecls.length Then
			Return False
		End If
		
		' same arg types?
		For Local i:Int = 0 Until func.argDecls.length
			If Not func.argDecls[i].ty.equalsType(fdecl.argDecls[i].ty) Return False
		Next
		
		' same return type?
		If Not func.retType.equalsType(fdecl.retType) Then
			' if function pointer specifies Int return type, our function can specify void...
			If TIntType(func.retType) And TVoidType(fdecl.retType) Then
				Return True
			End If
			Return False
		End If
		
		Return True
	End Method
	
	Method ToString$()
		Return "Function Ptr"
	End Method

	Method OnCopy:TType()
		Local ty:TFunctionPtrType = New TFunctionPtrType
		ty.func = func
		Return ty
	End Method

	Method Semant:TType(option:Int = False)
		func.Semant()
		Return Self
	End Method

End Type

' a holder during parsing which becomes the "real" var ptr type during semanting
Type TVarPtrType Extends TType
	Method OnCopy:TType()
		Return New TVarPtrType
	End Method
End Type
