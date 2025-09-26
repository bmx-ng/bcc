' Copyright (c) 2013-2025 Bruce A Henderson
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
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		Return EqualsType( ty )
	End Method
	
	Method WidensToType:Int( ty:TType )
		Return False
	End Method
	
	Method DistanceToType:Int(ty:TType)
		Return T_MAX_DISTANCE
	End Method
	
	Method Semant:TType(option:Int = False, callback:TCallback = Null)
		Return Self
	End Method

	Method GetClass:TClassDecl()
		Return Null
	End Method
	
	Method GetClassScope:TScopeDecl()
		Return GetClass()
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

	Rem
	bbdoc: map a var pointer to it's pointer equivalent (strip out var)
	End Rem
	Function MapVarPointerToPointerType:TType(ty:TType)
		If ty = stringType Then
			ty = ty.Copy()
		End If

		If (ty._flags & T_VARPTR) Then
			ty._flags :~ T_VARPTR
			
			Return MapToPointerType(ty)
		End If

		Return ty
	End Function
	
	Function MapPointerToPrim:TType(ty:TNumericType)
		Return MapFromPointer(ty)
	End Function

	Function MapFromPointer:TType(ty:TType)
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

	Const T_BYTE:Int        =  $001
	Const T_SHORT:Int       =  $002
	Const T_INT:Int         =  $004
	Const T_LONG:Int        =  $008
	Const T_FLOAT:Int       =  $010
	Const T_DOUBLE:Int      =  $020
	Const T_STRING:Int      =  $040
	Const T_ARRAY:Int       =  $080
	Const T_FUNCTIONPTR:Int =  $100
	Const T_SIZET:Int       =  $200
	Const T_UINT:Int        =  $400
	Const T_ULONG:Int       =  $800
	Const T_FLOAT64:Int     = $1000
	Const T_INT128:Int      = $2000
	Const T_FLOAT128:Int    = $4000
	Const T_DOUBLE128:Int   = $8000
	Const T_LPARAM:Int      =$10000
	Const T_WPARAM:Int      =$20000
	Const T_ENUM:Int        =$40000
	Const T_LONGINT:Int     =$80000
	Const T_ULONGINT:Int    =$100000

	Const T_MAX_DISTANCE:Int = $FFFF

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

		If _flags & T_VARPTR Then
			s:+ " VarPtr"
		End If
	
		Return s
	End Method
	
	Method IsFlagEquivalent:Int(ty:TType)
		Return _flags = ty._flags Or ..
			(Not IsPointerType(Self, 0, TType.T_POINTER) And (ty._flags & T_VAR)) Or ..
			(Not IsPointerType(ty, 0, TType.T_POINTER) And (_flags & T_VAR)) Or ..
			(IsPointerType(Self, 0, TType.T_POINTER) And (ty._flags & T_VARPTR)) Or ..
			(IsPointerType(ty, 0, TType.T_POINTER) And (_flags & T_VARPTR))
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
		Case TType.T_UINT
			ty = New TUIntType
		Case TType.T_LONG
			ty = New TLongType
		Case TType.T_ULONG
			ty = New TULongType
		Case TType.T_SIZET
			ty = New TSizeTType
		Case TType.T_INT128
			ty = New TInt128Type
		Case TType.T_FLOAT
			ty = New TFloatType
		Case TType.T_DOUBLE
			ty = New TDoubleType
		Case TType.T_FLOAT64
			ty = New TFloat64Type
		Case TType.T_FLOAT128
			ty = New TFloat128Type
		Case TType.T_DOUBLE128
			ty = New TDouble128Type
		Case TType.T_STRING
			ty = New TStringType
		Case TType.T_ARRAY
			ty = New TArrayType
		Case TType.T_FUNCTIONPTR
			ty = New TFunctionPtrType
		Case TType.T_ENUM
			ty = New TEnumType
		Case TType.T_LONGINT
			ty = New TLongIntType
		Case TType.T_ULONGINT
			ty = New TULongIntType
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
		Case TType.T_UINT
			Return TUIntType(ty) <> Null
		Case TType.T_LONG
			Return TLongType(ty) <> Null
		Case TType.T_ULONG
			Return TULongType(ty) <> Null
		Case TType.T_SIZET
			Return TSizeTType(ty) <> Null
		Case TType.T_INT128
			Return TInt128Type(ty) <> Null
		Case TType.T_FLOAT
			Return TFloatType(ty) <> Null
		Case TType.T_DOUBLE
			Return TDoubleType(ty) <> Null
		Case TType.T_FLOAT64
			Return TFloat64Type(ty) <> Null
		Case TType.T_FLOAT128
			Return TFloat128Type(ty) <> Null
		Case TType.T_DOUBLE128
			Return TDouble128Type(ty) <> Null
		Case TType.T_STRING
			Return TStringType(ty) <> Null
		Case TType.T_ARRAY
			Return TArrayType(ty) <> Null
		Case TType.T_FUNCTIONPTR
			Return TFunctionPtrType(ty) <> Null
		Case TType.T_ENUM
			Return TEnumType(ty) <> Null
		Case TType.T_LONGINT
			Return TLongIntType(ty) <> Null
		Case TType.T_ULONGINT
			Return TULongIntType(ty) <> Null
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
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
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
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or TBoolType( ty )<>Null Or (Not noExtendString And TStringType( ty )<>Null)
	End Method

	Method WidensToType:Int( ty:TType )
		Return IsNumericType(ty)
	End Method

	Method DistanceToType:Int(ty:TType)

		If TBoolType(ty)<>Null Then
			Return 0
		End If
		
		If TIntType(ty)<>Null Then
			Return 1
		End If
		
		Return 4
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

Type TIntegralType Extends TNumericType
End Type

Type TIntType Extends TIntegralType
	
	Method EqualsType:Int( ty:TType )
		Return TIntType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True,,,,SCOPE_CLASS_HEIRARCHY )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If _flags & T_VARPTR And (TIntType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) Or (WORD_SIZE=4 And TLParamType(ty)<>Null)
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TIntType(ty)<>Null And (ty._flags & T_VAR)) Or TLongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or (WORD_SIZE=8 And TLParamType(ty)<>Null)
	End Method
	
	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TIntType(ty)<>Null Then
			Return 0
		End If
		
		If WORD_SIZE = 4 And TLParamType(ty)<>Null Then
			Return 1
		End If
		
		If TLongType(ty)<>Null Then
			Return 2
		End If

		If WORD_SIZE = 8 And TLParamType(ty)<>Null Then
			Return 3
		End If
		
		If TFloatType(ty)<>Null Then
			Return 4
		End If

		If TDoubleType(ty)<>Null Then
			Return 6
		End If
		
		Return T_MAX_DISTANCE
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

Type TUIntType Extends TIntegralType
	
	Method EqualsType:Int( ty:TType )
		Return TUIntType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If _flags & T_VARPTR And (TUIntType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) Or (WORD_SIZE=4 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null))
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TUIntType(ty)<>Null And (ty._flags & T_VAR)) Or TIntType(ty)<> Null Or TLongType(ty)<>Null Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or (WORD_SIZE=8 And TWParamType(ty)<>Null)
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TUIntType(ty)<>Null Then
			Return 0
		End If

		If WORD_SIZE = 4 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null) Then
			Return 1
		End If
		
		If TIntType(ty)<>Null Then
			Return 2
		End If
		
		If WORD_SIZE = 8 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null) Then
			Return 3
		End If
		
		If TULongType(ty)<>Null Then
			Return 3
		End If

		If TLongType(ty)<>Null Then
			Return 4
		End If

		If TFloatType(ty)<>Null Then
			Return 5
		End If

		If TDoubleType(ty)<>Null Then
			Return 6
		End If
		
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TUIntType
	End Method

	Method ToString$()
		Return "UInt" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return 4
	End Method

End Type

Type TSizeTType Extends TIntegralType
	
	Method EqualsType:Int( ty:TType )
		Return TSizeTType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If _flags & T_VARPTR And (TSizeTType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) Or (WORD_SIZE=4 And TUIntType(ty)<>Null) Or (WORD_SIZE=8 And TULongType(ty)<>Null)
	End Method

	Method WidensToType:Int( ty:TType )
		If WORD_SIZE = 4 Then
			Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or ((TSizeTType(ty)<>Null Or TUIntType(ty)<>Null) And (ty._flags & T_VAR)) Or TIntType(ty)<>Null Or TUIntType(ty)<>Null Or TLongType(ty)<>Null Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TWParamType(ty)<>Null Or TLParamType(ty)<>Null
		Else
			Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or ((TSizeTType(ty)<>Null Or TULongType(ty)<>Null) And (ty._flags & T_VAR)) Or TLongType(ty)<>Null Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TFloat64Type(ty)<>Null Or TWParamType(ty)<>Null Or TLParamType(ty)<>Null
		End If
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TSizeTType(ty)<>Null Then
			Return 0
		End If

		If TWParamType(ty)<>Null Then
			Return 1
		End If

		If WORD_SIZE = 4 Then
			If TUIntType(ty)<>Null Then
				Return 1
			End If

			If TIntType(ty)<>Null Then
				Return 2
			End If
			
			If TLParamType(ty)<>Null Then
				Return 2
			End If

			If TULongType(ty)<>Null Then
				Return 3
			End If

			If TLongType(ty)<>Null Then
				Return 4
			End If

			If TFloatType(ty)<>Null Then
				Return 5
			End If
	
			If TDoubleType(ty)<>Null Then
				Return 6
			End If
			
		Else
			If TULongType(ty)<>Null Then
				Return 1
			End If

			If TLongType(ty)<>Null Then
				Return 2
			End If

			If TLParamType(ty)<>Null Then
				Return 2
			End If

			If TFloatType(ty)<>Null Then
				Return 4
			End If
	
			If TDoubleType(ty)<>Null Then
				Return 6
			End If

			If TFloat64Type(ty)<>Null Then
				Return 8
			End If

		End If
	
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TSizeTType
	End Method

	Method ToString$()
		Return "size_t" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return WORD_SIZE
	End Method

End Type

Type TByteType Extends TIntegralType
	
	Method EqualsType:Int( ty:TType )
		Return TByteType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If (_flags & T_VARPTR) And (TByteType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TByteVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TByteType(ty)<>Null And (ty._flags & T_VAR)) Or TShortType(ty)<>Null Or TIntType(ty)<>Null Or TUIntType(ty)<>Null Or TLongType(ty)<>Null Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TWParamType(ty)<>Null Or TLParamType(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TByteType(ty)<>Null Then
			Return 0
		End If

		If TShortType(ty)<>Null Then
			Return 2
		End If

		If WORD_SIZE = 4 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null) Then
			Return 4
		End If
		
		If TUIntType(ty)<>Null Then
			Return 4
		End If

		If TIntType(ty)<>Null Then
			Return 5
		End If

		If WORD_SIZE = 4 And TLParamType(ty)<>Null Then
			Return 5
		End If
		
		If WORD_SIZE = 8 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null) Then
			Return 6
		End If
		
		If TULongType(ty)<>Null Then
			Return 6
		End If

		If TLongType(ty)<>Null Then
			Return 7
		End If

		If WORD_SIZE = 8 And TLParamType(ty)<>Null Then
			Return 7
		End If

		If TFloatType(ty)<>Null Then
			Return 8
		End If

		If TDoubleType(ty)<>Null Then
			Return 10
		End If
		
		Return T_MAX_DISTANCE
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

Type TShortType Extends TIntegralType

	Method EqualsType:Int( ty:TType )
		Return TShortType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If _flags & T_VARPTR And (TShortType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TShortVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TShortType(ty)<>Null And (ty._flags & T_VAR)) Or TIntType(ty)<>Null Or TUIntType(ty)<>Null Or TLongType(ty)<>Null Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TWParamType(ty)<>Null Or TLParamType(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TShortType(ty)<>Null Then
			Return 0
		End If

		If WORD_SIZE = 4 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null) Then
			Return 2
		End If
		
		If TUIntType(ty)<>Null Then
			Return 2
		End If

		If TIntType(ty)<>Null Then
			Return 3
		End If

		If WORD_SIZE = 4 And TLParamType(ty)<>Null Then
			Return 3
		End If
		
		If WORD_SIZE = 8 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null) Then
			Return 4
		End If

		If TULongType(ty)<>Null Then
			Return 4
		End If

		If TLongType(ty)<>Null Then
			Return 5
		End If

		If WORD_SIZE = 8 And TLParamType(ty)<>Null Then
			Return 5
		End If

		If TFloatType(ty)<>Null Then
			Return 6
		End If

		If TDoubleType(ty)<>Null Then
			Return 8
		End If
		
		Return T_MAX_DISTANCE
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

Type TLongType Extends TIntegralType ' BaH Long
	
	Method EqualsType:Int( ty:TType )
		Return TLongType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If _flags & T_VARPTR And (TLongType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TLongVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TLongType(ty)<>Null And (ty._flags & T_VAR)) Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TFloat64Type(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TLongType(ty)<>Null Then
			Return 0
		End If
		
		If WORD_SIZE = 8 And TLParamType(ty)<>Null Then
			Return 1
		End If

		If TFloatType(ty)<>Null Then
			Return 2
		End If

		If TDoubleType(ty)<>Null Then
			Return 4
		End If

		If TFloat64Type(ty)<>Null Then
			Return 6
		End If
		
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TLongType
	End Method

	Method ToString$()
		Return "Long" + ToStringParts()
	End Method
End Type

Type TULongType Extends TIntegralType
	
	Method EqualsType:Int( ty:TType )
		Return TULongType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If _flags & T_VARPTR And (TULongType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TLongVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TULongType(ty)<>Null And (ty._flags & T_VAR)) Or TDoubleType(ty)<>Null Or TFloat64Type(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TULongType(ty)<>Null Then
			Return 0
		End If

		If WORD_SIZE = 8 And (TSizeTType(ty)<>Null Or TWParamType(ty)<>Null) Then
			Return 1
		End If
		
		If TLongType(ty)<>Null Then
			Return 2
		End If

		If TFloatType(ty)<>Null Then
			Return 3
		End If

		If TDoubleType(ty)<>Null Then
			Return 4
		End If

		If TFloat64Type(ty)<>Null Then
			Return 6
		End If

		Return T_MAX_DISTANCE
	End Method

	Method OnCopy:TType()
		Return New TULongType
	End Method

	Method ToString$()
		Return "ULong" + ToStringParts()
	End Method
End Type

Type TLongIntType Extends TIntegralType
	
	Method EqualsType:Int( ty:TType )
		Return TLongIntType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		If _flags & T_VARPTR And (TLongIntType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) Or (WORD_SIZE=4 And TLParamType(ty)<>Null)
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TLongIntType(ty)<>Null And (ty._flags & T_VAR)) Or TLongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or (WORD_SIZE=8 And TLParamType(ty)<>Null)
	End Method
	
	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TLongIntType(ty)<>Null Then
			Return 0
		End If

		Local longIntSize:Int = GetSize()

		If longIntSize = 4 And TIntType(ty)<>Null Then
			Return 1
		End If

		If longIntSize = 4 And TLParamType(ty)<>Null Then
			Return 1
		End If
		
		If longIntSize = 4 And TLongType(ty)<>Null Then
			Return 2
		End If

		If longIntSize = 8 And TLParamType(ty)<>Null Then
			Return 3
		End If

		If longIntSize = 8 And TIntType(ty)<>Null Then
			Return 2
		End If

		If longIntSize = 8 And TLongType(ty)<>Null Then
			Return 1
		End If

		If TFloatType(ty)<>Null Then
			Return 4
		End If

		If TDoubleType(ty)<>Null Then
			Return 6
		End If
		
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TLongIntType
	End Method

	Method ToString$()
		Return "LongInt" + ToStringParts()
	End Method

	Method GetSize:Int()
		If WORD_SIZE = 4 Then
			Return 4
		Else
			If opt_platform = "linux" or opt_platform = "macos" Then
				Return 8
			Else
				Return 4
			End If
		End If
	End Method

End Type

Type TULongIntType Extends TIntegralType
	
	Method EqualsType:Int( ty:TType )
		Return TULongIntType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		If _flags & T_VARPTR And (TULongIntType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) Or (WORD_SIZE=4 And TLParamType(ty)<>Null)
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TULongIntType(ty)<>Null And (ty._flags & T_VAR)) Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or (WORD_SIZE=8 And TLParamType(ty)<>Null)
	End Method
	
	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TULongIntType(ty)<>Null Then
			Return 0
		End If

		Local longIntSize:Int = GetSize()

		If longIntSize = 4 And TIntType(ty)<>Null Then
			Return 2
		End If

		If longIntSize = 4 And TUIntType(ty)<>Null Then
			Return 1
		End If

		If longIntSize = 4 And TLParamType(ty)<>Null Then
			Return 3
		End If
		
		If longIntSize = 4 And TLongType(ty)<>Null Then
			Return 4
		End If

		If longIntSize = 8 And TLParamType(ty)<>Null Then
			Return 3
		End If

		If longIntSize = 8 And TIntType(ty)<>Null Then
			Return 4
		End If

		If longIntSize = 8 And TUIntType(ty)<>Null Then
			Return 3
		End If

		If longIntSize = 8 And TLongType(ty)<>Null Then
			Return 1
		End If

		If longIntSize = 8 And TULongType(ty)<>Null Then
			Return 2
		End If

		If TFloatType(ty)<>Null Then
			Return 5
		End If

		If TDoubleType(ty)<>Null Then
			Return 6
		End If
		
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TULongIntType
	End Method

	Method ToString$()
		Return "ULongInt" + ToStringParts()
	End Method

	Method GetSize:Int()
		If WORD_SIZE = 4 Then
			Return 4
		Else
			If opt_platform = "linux" or opt_platform = "macos" Then
				Return 8
			Else
				Return 4
			End If
		End If
	End Method

End Type

Type TDecimalType Extends TNumericType
End Type

Type TFloatType Extends TDecimalType
	
	Method EqualsType:Int( ty:TType )
		Return TFloatType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf	
		If _flags & T_VARPTR And (TFloatType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TFloatVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TFloatType(ty)<>Null And (ty._flags & T_VAR)) Or TDoubleType(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TFloatType(ty)<>Null Then
			Return 0
		End If

		If TDoubleType(ty)<>Null Then
			Return 2
		End If
		
		Return T_MAX_DISTANCE
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
		Return TDoubleType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf	
		If _flags & T_VARPTR And (TDoubleType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TDoubleVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TDoubleType(ty)<>Null And (ty._flags & T_VAR))
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TDoubleType(ty)<>Null Then
			Return 0
		End If
		
		Return T_MAX_DISTANCE
	End Method

	Method OnCopy:TType()
		Return New TDoubleType
	End Method

	Method ToString$()
		Return "Double" + ToStringParts()
	End Method

End Type

Type TIntrinsicType Extends TNumericType
End Type

Type TInt128Type Extends TIntrinsicType
	
	Method EqualsType:Int( ty:TType )
		Return TInt128Type( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf
		If _flags & T_VARPTR And (TLongType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TLongVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TInt128Type(ty)<>Null And (ty._flags & T_VAR)) Or TFloat128Type(ty)<>Null Or TDouble128Type(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TInt128Type(ty)<>Null Then
			Return 0
		End If

		If TFloat128Type(ty)<>Null Then
			Return 2
		End If

		If TDouble128Type(ty)<>Null Then
			Return 4
		End If
		
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TInt128Type
	End Method

	Method ToString$()
		Return "Int128" + ToStringParts()
	End Method
End Type

Type TFloat64Type Extends TIntrinsicType
	
	Method EqualsType:Int( ty:TType )
		Return TFloat64Type( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf	
		If _flags & T_VARPTR And (TFloat64Type(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TDoubleVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TFloat64Type(ty)<>Null And (ty._flags & T_VAR))
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TFloat64Type(ty)<>Null Then
			Return 0
		End If
		
		Return T_MAX_DISTANCE
	End Method

	Method OnCopy:TType()
		Return New TFloat64Type
	End Method

	Method ToString$()
		Return "Float64" + ToStringParts()
	End Method

End Type

Type TFloat128Type Extends TIntrinsicType
	
	Method EqualsType:Int( ty:TType )
		Return TFloat128Type( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf	
		If _flags & T_VARPTR And (TFloat128Type(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TDoubleVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TFloat128Type(ty)<>Null And (ty._flags & T_VAR)) Or TInt128Type(ty)<>Null Or TDouble128Type(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TFloat128Type(ty)<>Null Then
			Return 0
		End If

		If TDouble128Type(ty)<>Null Then
			Return 2
		End If
		
		If TInt128Type(ty)<>Null Then
			Return 4
		End If
	
		Return T_MAX_DISTANCE
	End Method

	Method OnCopy:TType()
		Return New TFloat128Type
	End Method

	Method ToString$()
		Return "Float128" + ToStringParts()
	End Method

End Type

Type TDouble128Type Extends TIntrinsicType
	
	Method EqualsType:Int( ty:TType )
		Return TDouble128Type( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		'If TObjectType( ty )
		'	Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
		'	Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
		'	Return ctor And ctor.IsCtor()
		'EndIf	
		If _flags & T_VARPTR And (TDouble128Type(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TDoubleVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TDouble128Type(ty)<>Null And (ty._flags & T_VAR)) Or TInt128Type(ty)<>Null Or TFloat128Type(ty)<>Null
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TDouble128Type(ty)<>Null Then
			Return 0
		End If

		If TFloat128Type(ty)<>Null Then
			Return 2
		End If

		If TInt128Type(ty)<>Null Then
			Return 4
		End If
		
		Return T_MAX_DISTANCE
	End Method

	Method OnCopy:TType()
		Return New TDouble128Type
	End Method

	Method ToString$()
		Return "Double128" + ToStringParts()
	End Method

End Type

Type TStringType Extends TType

	Field cdecl:TClassDecl

	Method EqualsType:Int( ty:TType )
		Return TStringType( ty )<>Null And (_flags = ty._flags Or (_flags & T_VAR))
	End Method

	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )	
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
	
	Method Semant:TType(option:Int = 0, callback:TCallback = Null)
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
	
	Method DistanceToType:Int(ty:TType)
		If TStringType(ty) Then
			Return 0
		End If
		
		' prefer Object
		If TObjectType(ty)
			If TObjectType(ty).classDecl.ident = "Object" Then
				Return $F
			End If
		End If

		Return T_MAX_DISTANCE
	End Method
	
End Type

Type TArrayType Extends TType
	Field elemType:TType
	Field dims:Int
	Field isStatic:Int
	Field length:String
	
	Method Create:TArrayType( elemType:TType, dims:Int = 1, flags:Int = 0, isStatic:Int = False, length:Int = 0 )
		Self.elemType=elemType
		Self.dims = dims
		Self._flags = flags
		Self.isStatic = isStatic
		Self.length = length
		Return Self
	End Method
	
	Method ActualType:TType()
		Local ty:TType=elemType.ActualType()
		If ty=elemType Return Self
		Return New TArrayType.Create( ty )
	End Method
		
	Method EqualsType:Int( ty:TType )
		Local arrayType:TArrayType=TArrayType( ty )
		Return arrayType And elemType.EqualsType( arrayType.elemType ) And dims = arrayType.dims And arrayType.isStatic = isStatic And arrayType.length = length
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		Local arrayType:TArrayType=TArrayType( ty )
		Return (arrayType And dims = arrayType.dims And ..
			(arrayType.isStatic = isStatic And arrayType.length = length) And ..
			( TVoidType( elemType ) ..
				Or elemType.EqualsType( arrayType.elemType ) ..
				Or ((TObjectType(elemType) Or TStringType(elemType) Or TArrayType(elemType)) And ..
					(elemType.ExtendsType( arrayType.elemType ) ..
						Or (TObjectType(arrayType.elemType) And TObjectType( arrayType.elemType ).classDecl.ident="Object") ..
					))) ..
					) ..
				Or IsPointerType(ty, 0, TType.T_POINTER) <> Null Or (TObjectType( ty ) And TObjectType( ty ).classDecl.ident="Object")
	End Method
	
	Method Semant:TType(option:Int = False, callback:TCallback = Null)
		Local ty:TType=elemType.Semant(option, callback)
		If ty<>elemType Return New TArrayType.Create( ty, dims, _flags, isStatic, Int(length) )
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
		ty.isStatic = isStatic
		ty.length = length
		Return ty
	End Method

	Method ToString$()
		Local t:String = elemType.ToString()
		If isStatic Then
			t = "Static " + t + " Array[" + length + "]"
		Else
			t :+ " Array"
		End If
		Return t
	End Method
	
	Method DistanceToType:Int(ty:TType)
		If TArrayType(ty) Then
			Return 0
		End If
		
		' prefer Object
		If TObjectType(ty)
			If TObjectType(ty).classDecl.ident = "Object" Then
				Return $F
			End If
		End If

		Return T_MAX_DISTANCE
	End Method

End Type

Type TObjectType Extends TType
	Field classDecl:TClassDecl
	Field instance:Int
	
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
		Return TNullDecl(classDecl) <> Null Or (objty And (classDecl=objty.classDecl) And (Not classDecl.IsStruct() Or IsFlagEquivalent(ty)))' Or classDecl.ExtendsClass( objty.classDecl ))) 'Or TObjectVarPtrType(ty) <> Null
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		If classDecl.IsStruct() Then
			If (_flags & T_VARPTR Or IsPointerType(Self, 0, T_POINTER)) And (TNumericType(ty) <> Null) And IsPointerType(ty, 0, T_POINTER) Then
				Return True
			End If
			Return False
		End If
		Local objty:TObjectType=TObjectType( ty )
		If objty Return classDecl.ExtendsClass( objty.classDecl )
		If IsPointerType( ty, T_BYTE ) Return True
	End Method
	
	Method GetClass:TClassDecl()
		Return classDecl
	End Method
	
	Method ToString$()
		Return classDecl.ToTypeString() + ToStringParts()
	End Method

	Method OnCopy:TType()
		Local ty:TObjectType = New TObjectType
		ty.classDecl = classDecl
		ty.instance = instance
		Return ty
	End Method

	Method DistanceToType:Int(ty:TType)
		If TObjectType(ty) Then
			If classDecl = TObjectType(ty).classDecl Then
				Return 0
			End If
			
			If classDecl.ExtendsClass(TObjectType(ty).classDecl) Then
				Return $F
			End If
		End If
		
		If classDecl.IsStruct() Then
			If (_flags & T_VARPTR Or IsPointerType(Self, 0, T_POINTER)) And (TNumericType(ty) <> Null) And IsPointerType(ty, 0, T_POINTER) Then
				Return $10
			End If
		End If

		Return T_MAX_DISTANCE
	End Method

End Type

Type TClassType Extends TType

	Field classDecl:TClassDecl
	Field instance:Int
	
	Method Create:TClassType( classDecl:TClassDecl )
		Self.classDecl=classDecl
		Return Self
	End Method

	Method GetClass:TClassDecl()
		Return classDecl
	End Method

	Method OnCopy:TType()
		Local ty:TClassType = New TClassType
		ty.classDecl = classDecl
		ty.instance = instance
		Return ty
	End Method

	Method ToString:String()
		Return "Type"
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
		InternalErr "TIdentType.ActualType"
	End Method
	
	Method EqualsType:Int( ty:TType )
		InternalErr "TIdentType.EqualsType"
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		InternalErr "TIdentType.ExtendsType"
	End Method
	
	'Method Semant:TType()
	'	If ident Return New TObjectType.Create( FindClass() )
	'	Return New TObjectType.Create( TClassDecl.nullObjectClass )
	'End Method
	
	
	Method Semant:TType(ignoreNotFoundError:Int = 0, callback:TCallback = Null)
'If ident="IPair" DebugStop
		If Not ident Return TType.nullObjectType

		Local targs:TType[args.Length]
		For Local i:Int=0 Until args.Length
			targs[i]=args[i].Semant(ignoreNotFoundError, callback)
		Next
		
		Local tyid$,ty:TType
		Local i:Int=ident.FindLast( "." )
		
		If i=-1
			tyid=ident.ToLower()

			If tyid = "self" Then
				' find owning class
				Local scope:TClassDecl = _env.ClassScope()
				If scope Then
					tyid = scope.ident
					ty = New TClassType.Create(scope)
					
					' test for method scope - self is already an instance
					Local funcScope:TFuncDecl = _env.FuncScope()
					If funcScope.IsAnyMethod() Then
						TClassType(ty).instance = True
					End If
				Else
					Err "'Self' can only be used within methods."
				End If
			End If
			
			If Not ty Then
				ty=_env.FindType( tyid,targs, callback )
			End If

			' finally scan all modules for it
			If Not ty Then
				For Local mdecl:TModuleDecl = EachIn _appInstance.globalImports.Values()
					ty=mdecl.FindType( tyid,targs, callback )
					If ty Exit
				Next
			Else If TIdentType(ty) Then
				ty = ty.Semant()
			End If
		Else
			Local id:String = ident.ToLower()
			i = id.Find( "." )
						
			' try scope search first
			tyid=id[..i]
			
			If tyid = "self" Then
				' find owning class
				Local scope:TClassDecl = _env.ClassScope()
				If scope Then
					tyid = scope.ident
					ty = New TClassType.Create(scope)
					
					' test for method scope - self is already an instance
					Local funcScope:TFuncDecl = _env.FuncScope()
					If funcScope.IsAnyMethod() Then
						TClassType(ty).instance = True
					End If
				Else
					Err "'Self' can only be used within methods."
				End If
			End If
			
			If Not ty Then
				ty=_env.FindType( tyid,targs, callback )
			End If
			
			If Not ty Then
				i = id.FindLast( "." )
		
				' try scope search first
				tyid=id[..i]
				ty=_env.FindType( tyid,targs, callback )				

				If Not ty Then
					' no? now try module search
					Local modid$=id[..i]
					Local mdecl:TModuleDecl=_env.FindModuleDecl( modid )
					If Not mdecl Err "Module '"+modid+"' not found"
					tyid=id[i+1..]
					ty=mdecl.FindType( tyid,targs, callback )
				End If
			End If
		EndIf
		If Not ty Then
			If ignoreNotFoundError Then
				Return Null
			End If
			Err "Type '"+tyid+"' not found"
		End If
		
		If (_flags & T_VAR) Then
			If TObjectType(ty) Then
				ty = New TObjectType.Create(TObjectType(ty).classDecl)
				ty._flags :| T_VAR
			Else If TEnumType(ty) Then
				ty = New TEnumType.Create(TEnumType(ty).decl)
				ty._flags :| T_VAR
			Else
				ty = ty.Copy()
				ty._flags :| T_VAR
			End If
		End If

		If (_flags & T_POINTER) And TObjectType(ty) Then
			' FIXME #200
			'If Not TObjectType(ty).classDecl.IsExtern() Then
			'	Err "Invalid Pointer type."
			'End If
			ty = New TObjectType.Create(TObjectType(ty).classDecl)
			ty._flags :| (_flags & T_POINTER)
		End If
		
		Return ty
	End Method

	Method SemantClass:TClassDecl(callback:TCallback = Null)
		Local ty:TObjectType=TObjectType( Semant(False, callback) )
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
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
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
		Local fdecl:TFuncDecl=GetClass().FindFuncDecl( op,Null,True,,,,SCOPE_CLASS_HEIRARCHY )
		Return fdecl And fdecl.IsMethod() And fdecl.retType.EqualsType( ty )
	End Method
	
	Method GetClass:TClassDecl()
		Return classDecl
	End Method
	
	Method ToString$()
		Return classDecl.ToTypeString()
	End Method

	Method OnCopy:TType()
		Local ty:TExternObjectType = New TExternObjectType
		ty.classDecl = classDecl
		Return ty
	End Method
End Type

Type TFunctionPtrType Extends TType

	Field func:TFuncDecl
	
	Method Create:TFunctionPtrType(func:TFuncDecl)
		Self.func = func
		Return Self
	End Method

	Method EqualsType:Int( ty:TType )
		If Not TFunctionPtrType(ty) Then Return False
		' declared function pointer
		Local tyfunc:TFuncDecl = TFunctionPtrType(ty).func
		If Not tyfunc.retType.EqualsType(func.retType) Then Return False
		If Not (tyfunc.argDecls.Length = func.argDecls.Length) Then Return False
		For Local a:Int = 0 Until func.argDecls.Length
			' does our arg equal declared arg?
			If Not func.argDecls[a].ty.EqualsType(tyfunc.argDecls[a].ty) Then Return False
		Next
		Return True
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		If TFunctionPtrType( ty )
			' declared function pointer
			Local tyfunc:TFuncDecl = TFunctionPtrType(ty).func
			If Not func.retType.ExtendsType(tyfunc.retType) Then Return False
			If Not (func.argDecls.Length = tyfunc.argDecls.Length) Then Return False
			For Local a:Int = 0 Until func.argDecls.Length
				' does declared arg extend our arg?
				If Not tyfunc.argDecls[a].ty.ExtendsType(func.argDecls[a].ty) Then Return False
			Next
			Return True
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
		Return func.ToTypeString()
	End Method

	Method OnCopy:TType()
		Local ty:TFunctionPtrType = New TFunctionPtrType
		ty.func = func
		Return ty
	End Method

	Method Semant:TType(option:Int = False, callback:TCallback = Null)
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

Type TParamType Extends TIntegralType
End Type

Type TWParamType Extends TParamType

	Method EqualsType:Int( ty:TType )
		Return TWParamType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		If _flags & T_VARPTR And (TWParamType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TIntVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		If WORD_SIZE = 4 Then
			Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or ((TWParamType(ty)<>Null Or TSizeTType(ty)<>Null Or TUIntType(ty)<>Null) And (ty._flags & T_VAR)) Or TIntType(ty)<>Null Or TUIntType(ty)<>Null Or TLongType(ty)<>Null Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null
		Else
			Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or ((TWParamType(ty)<>Null Or TSizeTType(ty)<>Null Or TULongType(ty)<>Null) And (ty._flags & T_VAR)) Or TLongType(ty)<>Null Or TULongType(ty)<>Null Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TFloat64Type(ty)<>Null
		End If
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TWParamType(ty)<>Null Then
			Return 0
		End If

		If TSizeTType(ty)<>Null Then
			Return 0
		End If

		If WORD_SIZE = 4 Then
			If TUIntType(ty)<>Null Then
				Return 0
			End If

			If TIntType(ty)<>Null Then
				Return 2
			End If

			If TULongType(ty)<>Null Then
				Return 3
			End If

			If TLongType(ty)<>Null Then
				Return 4
			End If

			If TFloatType(ty)<>Null Then
				Return 5
			End If
	
			If TDoubleType(ty)<>Null Then
				Return 6
			End If
			
		Else
			If TULongType(ty)<>Null Then
				Return 0
			End If

			If TLongType(ty)<>Null Then
				Return 2
			End If

			If TFloatType(ty)<>Null Then
				Return 4
			End If
	
			If TDoubleType(ty)<>Null Then
				Return 6
			End If

			If TFloat64Type(ty)<>Null Then
				Return 8
			End If

		End If
	
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TWParamType
	End Method

	Method ToString$()
		Return "WPARAM" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return WORD_SIZE
	End Method

End Type

Type TLParamType Extends TParamType

	Method EqualsType:Int( ty:TType )
		Return TLParamType( ty )<>Null And (_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method
	
	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		If _flags & T_VARPTR And (TLParamType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty)) Or (Not widensTest And TNumericType( ty )<>Null) Or (Not noExtendString And TStringType( ty )<>Null) 'Or TIntVarPtrType( ty )<> Null
	End Method

	Method WidensToType:Int( ty:TType )
		If WORD_SIZE = 4 Then
			Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or ((TIntType(ty)<>Null Or TLParamType(ty)<>Null) And (ty._flags & T_VAR)) Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TFloat64Type(ty)<>Null
		Else
			Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or ((TLongType(ty)<>Null Or TLParamType(ty)<>Null) And (ty._flags & T_VAR)) Or TFloatType(ty)<>Null Or TDoubleType(ty)<>Null Or TFloat64Type(ty)<>Null
		End If
	End Method

	Method DistanceToType:Int(ty:TType)
		If IsPointerType(ty, 0, T_POINTER) Then
			If IsPointerType(Self, 0, T_POINTER) Then
				Return 0
			Else
				Return T_MAX_DISTANCE
			End If
		End If

		If TLParamType(ty)<>Null Then
			Return 0
		End If

		If WORD_SIZE = 4 Then
		
			If TIntType(ty)<>Null Then
				Return 0
			End If
			
			If TLongType(ty)<>Null Then
				Return 2
			End If
			
			If TFloatType(ty)<>Null Then
				Return 4
			End If
			
			If TDoubleType(ty)<>Null Then
				Return 6
			End If
			
		Else
			If TLongType(ty)<>Null Then
				Return 0
			End If
	
			If TFloatType(ty)<>Null Then
				Return 2
			End If
	
			If TDoubleType(ty)<>Null Then
				Return 4
			End If
	
			If TFloat64Type(ty)<>Null Then
				Return 6
			End If

		End If
	
		Return T_MAX_DISTANCE
	End Method
	
	Method OnCopy:TType()
		Return New TLParamType
	End Method

	Method ToString$()
		Return "LPARAM" + ToStringParts()
	End Method

	Method GetSize:Int()
		Return WORD_SIZE
	End Method
	
End Type

Type TEnumType Extends TType

	Field decl:TEnumDecl
	
	Method Create:TEnumType(decl:TEnumDecl)
		Self.decl = decl
		Return Self
	End Method

	Method EqualsType:Int( ty:TType )
		Local ety:TEnumType = TEnumType(ty)
		Return ety And decl = ety.decl And(_flags = ty._flags Or ..
			(_flags & T_VARPTR And ty._flags & T_PTR) Or (ty._flags & T_VARPTR And _flags & T_PTR) Or (_flags & T_VAR))
	End Method

	Method ExtendsType:Int( ty:TType, noExtendString:Int = False, widensTest:Int = False )
		If _flags & T_VARPTR And (TEnumType(ty) <> Null Or IsPointerType(ty, 0, T_POINTER)) Return True
		Return (widensTest And WidensToType(ty))
	End Method

	Method WidensToType:Int( ty:TType )
		Return (IsPointerType(ty, 0, T_POINTER) And IsPointerType(Self, 0, T_POINTER)) Or (TEnumType(ty)<>Null And (ty._flags & T_VAR))
	End Method
	
	Method OnCopy:TType()
		Local ty:TEnumType = New TEnumType
		ty.decl = decl
		Return ty
	End Method
	
	Method IsFlags:Int()
		Return decl.isFlags
	End Method
	
	Method Value:String(ordinal:Int)
		Return decl.values[ordinal].Value()
	End Method

	Method ToString$()
		Return "Enum " + decl.ident + " " + ToStringParts()
	End Method

	Method GetClassScope:TScopeDecl()
		Return decl
	End Method

End Type

Type TTemplateArg
	Field ident:String
	Field superTy:TType[]
	
	Method ExtendsType(ty:TType)
		If Not superTy Then
			superTy = New TType[0]
		End If
		
		superTy :+ [ty]
	End Method
	
	Method ToString:String()
		Local s:String = ident
		If superTy Then
			s :+ " Extends "
			For Local i:Int = 0 Until superTy.length
				If i Then
					s:+ " And "
				End If
				s :+ superTy[i].ToString()
			Next
		End If
	End Method
End Type

