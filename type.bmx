' Copyright (c) 2013-2014 Bruce A Henderson
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
	
	Method Semant:TType()
		Return Self
	End Method

	Method GetClass:TClassDecl()
		Return Null
	End Method
	
	Method ToString$()
		Return "??Type??"
	End Method

	Method ArrayOf:TArrayType()
		If Not _arrayOf Then
			_arrayOf=New TArrayType.Create( Self )
		End If
		Return _arrayOf
	End Method
	
	Global voidType:TVoidType=New TVoidType
	Global boolType:TBoolType=New TBoolType
	Global intType:TIntType=New TIntType
	Global shortType:TShortType=New TShortType
	Global floatType:TFloatType=New TFloatType
	Global stringType:TStringType=New TStringType
	Global emptyArrayType:TArrayType=New TArrayType.Create( voidType )
	Global objectType:TIdentType=New TIdentType.Create( "brl.classes.object" )
	Global nullObjectType:TIdentType=New TIdentType.Create( "" )
	Global longType:TLongType=New TLongType ' BaH Long
	Global doubleType:TDoubleType=New TDoubleType
	Global byteType:TByteType=New TByteType
	Global pointerType:TPointerType=New TPointerType
	Global bytePointerType:TBytePtrType=New TBytePtrType
	Global intPointerType:TIntPtrType=New TIntPtrType
	Global shortPointerType:TShortPtrType=New TShortPtrType
	Global floatPointerType:TFloatPtrType=New TFloatPtrType
	Global doublePointerType:TDoublePtrType=New TDoublePtrType
	Global longPointerType:TLongPtrType=New TLongPtrType

	Global stringPointerType:TStringPtrType=New TStringPtrType
	Global varPointerType:TVarPtrType=New TVarPtrType

	Global byteVarPointerType:TByteVarPtrType=New TByteVarPtrType
	Global intVarPointerType:TIntVarPtrType=New TIntVarPtrType
	Global shortVarPointerType:TShortVarPtrType=New TShortVarPtrType
	Global floatVarPointerType:TFloatVarPtrType=New TFloatVarPtrType
	Global doubleVarPointerType:TDoubleVarPtrType=New TDoubleVarPtrType
	Global longVarPointerType:TLongVarPtrType=New TLongVarPtrType
	Global stringVarPointerType:TStringVarPtrType=New TStringVarPtrType
	Global objectVarPointerType:TObjectVarPtrType=New TObjectVarPtrType
	
	Global functionPointerType:TFunctionPtrType=New TFunctionPtrType
	
	Global pointerPointerType:TPointerPtrType=New TPointerPtrType
	Global bytePointerPtrType:TBytePtrPtrType=New TBytePtrPtrType
	Global intPointerPtrType:TIntPtrPtrType=New TIntPtrPtrType
	Global shortPointerPtrType:TShortPtrPtrType=New TShortPtrPtrType
	Global floatPointerPtrType:TFloatPtrPtrType=New TFloatPtrPtrType
	Global doublePointerPtrType:TDoublePtrPtrType=New TDoublePtrPtrType
	Global longPointerPtrType:TLongPtrPtrType=New TLongPtrPtrType

	Global byteVarPointerPtrType:TByteVarPtrPtrType=New TByteVarPtrPtrType
	Global intVarPointerPtrType:TIntVarPtrPtrType=New TIntVarPtrPtrType
	Global shortVarPointerPtrType:TShortVarPtrPtrType=New TShortVarPtrPtrType
	Global floatVarPointerPtrType:TFloatVarPtrPtrType=New TFloatVarPtrPtrType
	Global doubleVarPointerPtrType:TDoubleVarPtrPtrType=New TDoubleVarPtrPtrType
	Global longVarPointerPtrType:TLongVarPtrPtrType=New TLongVarPtrPtrType

	' these represent $z and $w respectively.
	Global stringToCharPointerType:TStringCharPtrType=New TStringCharPtrType
	Global stringToShortPointerType:TStringShortPtrType=New TStringShortPtrType

	Rem
	bbdoc: map to a pointer type
	End Rem
	Function MapToPointerType:TPointerType(ty:TType)
		If TByteType(ty) Return bytePointerType
		If TIntType(ty) Return intPointerType
		If TShortType(ty) Return shortPointerType
		If TFloatType(ty) Return floatPointerType
		If TDoubleType(ty) Return doublePointerType
		If TLongType(ty) Return longPointerType

		If TStringType(ty) Return stringPointerType
		
		' pointer pointer
		If TBytePtrType(ty) Return bytePointerPtrType
		If TIntPtrType(ty) Return intPointerPtrType
		If TShortPtrType(ty) Return shortPointerPtrType
		If TFloatPtrType(ty) Return floatPointerPtrType
		If TDoublePtrType(ty) Return doublePointerPtrType
		If TLongPtrType(ty) Return longPointerPtrType
		
		Return Null
	End Function

	Rem
	bbdoc: map to a var pointer type
	End Rem
	Function MapToVarPointerType:TType(ty:TType)
		If TByteType(ty) Return byteVarPointerType
		If TIntType(ty) Return intVarPointerType
		If TShortType(ty) Return shortVarPointerType
		If TFloatType(ty) Return floatVarPointerType
		If TDoubleType(ty) Return doubleVarPointerType
		If TLongType(ty) Return longVarPointerType

		If TStringType(ty) Return stringVarPointerType
		If TObjectType(ty) Return objectVarPointerType
		
		If TIdentType(ty) Return TIdentType(ty).CopyToDest(New TIdentVarPtrType)
		
		' pointer pointer
		If TBytePtrType(ty) Return byteVarPointerPtrType
		If TIntPtrType(ty) Return intVarPointerPtrType
		If TShortPtrType(ty) Return shortVarPointerPtrType
		If TFloatPtrType(ty) Return floatVarPointerPtrType
		If TDoublePtrType(ty) Return doubleVarPointerPtrType
		If TLongPtrType(ty) Return longVarPointerPtrType
		
		Return Null
	End Function
	
	Function MapPointerToPrim:TType(ty:TType)
		If TBytePtrType(ty) Return byteType
		If TIntPtrType(ty) Return intType
		If TShortPtrType(ty) Return shortType
		If TFloatPtrType(ty) Return floatType
		If TDoublePtrType(ty) Return doubleType
		If TLongPtrType(ty) Return longType

		If TBytePtrPtrType(ty) Return bytePointerType
		If TIntPtrPtrType(ty) Return intPointerType
		If TShortPtrPtrType(ty) Return shortPointerType
		If TFloatPtrPtrType(ty) Return floatPointerType
		If TDoublePtrPtrType(ty) Return doublePointerType
		If TLongPtrPtrType(ty) Return longPointerType
		
	End Function

	Function MapVarPointerToPrim:TType(ty:TType)
		If TByteVarPtrType(ty) Return byteType
		If TIntVarPtrType(ty) Return intType
		If TShortVarPtrType(ty) Return shortType
		If TFloatVarPtrType(ty) Return floatType
		If TDoubleVarPtrType(ty) Return doubleType
		If TLongVarPtrType(ty) Return longType
		If TStringVarPtrType(ty) Return stringType
	End Function
	
	Field _arrayOf:TArrayType
	
End Type

Type TVoidType Extends TType

	Method EqualsType:Int( ty:TType )
		Return TVoidType( ty )<>Null Or TIntType( ty) <> Null ' TODO : Void and int are interchangable...
	End Method
	
	Method ToString$()
		Return "Void"
	End Method
End Type

Type TBoolType Extends TType

	Method EqualsType:Int( ty:TType )
		Return TBoolType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Return TIntType( ty )<>Null Or TBoolType( ty )<>Null Or TStringType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Bool"
	End Method

End Type

Type TNumericType Extends TType

	Method ToPointer:TPointerType() Abstract
	
End Type

Type TIntType Extends TNumericType
	
	Method EqualsType:Int( ty:TType )
		Return TIntType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null Or TIntVarPtrType( ty )<> Null
	End Method
	
	Method ToPointer:TPointerType()
		Return intPointerType
	End Method

	Method ToString$()
		Return "Int"
	End Method
End Type

Type TByteType Extends TNumericType
	
	Method EqualsType:Int( ty:TType )
		Return TByteType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null Or TByteVarPtrType( ty )<> Null
	End Method

	Method ToPointer:TPointerType()
		Return bytePointerType
	End Method

	Method ToString$()
		Return "Byte"
	End Method
End Type

Type TShortType Extends TNumericType

	Method EqualsType:Int( ty:TType )
		Return TShortType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null Or TShortVarPtrType( ty )<> Null
	End Method
	
	Method ToPointer:TPointerType()
		Return shortPointerType
	End Method

	Method ToString$()
		Return "Short"
	End Method
End Type

Type TLongType Extends TNumericType ' BaH Long
	
	Method EqualsType:Int( ty:TType )
		Return TLongType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null Or TLongVarPtrType( ty )<> Null
	End Method
	
	Method ToPointer:TPointerType()
		Return longPointerType
	End Method

	Method ToString$()
		Return "Long"
	End Method
End Type

Type TFloatType Extends TNumericType
	
	Method EqualsType:Int( ty:TType )
		Return TFloatType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf	
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null Or TFloatVarPtrType( ty )<> Null
	End Method

	Method ToPointer:TPointerType()
		Return floatPointerType
	End Method
	
	Method ToString$()
		Return "Float"
	End Method

End Type

Type TDoubleType Extends TNumericType
	
	Method EqualsType:Int( ty:TType )
		Return TDoubleType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf	
		Return TNumericType( ty )<>Null Or TStringType( ty )<>Null Or TDoubleVarPtrType( ty )<> Null
	End Method

	Method ToPointer:TPointerType()
		Return doublePointerType
	End Method

	Method ToString$()
		Return "Double"
	End Method

End Type

Type TStringType Extends TType

	Field cdecl:TClassDecl

	Method EqualsType:Int( ty:TType )
		Return TStringType( ty )<>Null
	End Method

	Method ExtendsType:Int( ty:TType )	
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return EqualsType( ty ) Or TStringVarPtrType( ty ) <> Null Or TStringCharPtrType(ty) <> Null Or TBytePtrType(ty) <> Null
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
	
	Method Semant:TType()
		GetClass()
		Return Self
	End Method
	
	Method ToString$()
		Return "String"
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
		Return (arrayType And ( TVoidType( elemType ) Or elemType.EqualsType( arrayType.elemType ) )) Or TPointerType(ty) <> Null Or TObjectType(ty) <> Null
	End Method
	
	Method Semant:TType()
		Local ty:TType=elemType.Semant()
		If ty<>elemType Return New TArrayType.Create( ty, dims )
		Return Self
	End Method
	
	Method GetClass:TClassDecl()
		'Return _env.FindClassDecl( "array" )
		Return TClassDecl( _env.FindDecl( "array" ) )
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
		Return TNullDecl(classDecl) <> Null Or (objty And (classDecl=objty.classDecl Or classDecl.ExtendsClass( objty.classDecl ))) Or TObjectVarPtrType(ty) <> Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Local objty:TObjectType=TObjectType( ty )
		If objty Return classDecl.ExtendsClass( objty.classDecl )
		If TBytePtrType( ty ) Return True
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
	
	
	Method Semant:TType()
		If Not ident Return TClassDecl.nullObjectClass.objectType

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
		EndIf
		If Not ty Err "Type '"+tyid+"' not found"
		
		If TIdentVarPtrType(Self) And TObjectType(ty) Then
			ty = New TObjectVarPtrType.Create(TObjectType(ty).classDecl)
		End If
		
		Return ty
	End Method
	
	
	
Rem
	Method FindClass:TClassDecl()
	
		Local argClasses:TClassDecl[args.Length]

		For Local i:Int=0 Until args.Length
			argClasses[i]=args[i].FindClass()
		Next
		
		Local clsid$
		Local cdecl:TClassDecl
		Local i:Int=ident.FindLast( "." )
		If i=-1
			clsid=ident
			cdecl=_env.FindClassDecl( clsid,argClasses )
		Else
			Local modid$=ident[..i]
			Local mdecl:TModuleDecl=_env.FindModuleDecl( modid )
			If Not mdecl Err "Module '"+modid+"' not found"
			clsid=ident[i+1..] ' BaH
'DebugStop
			cdecl=mdecl.FindClassDecl( clsid,argClasses )
		EndIf
		If Not cdecl Err "Class '"+clsid+"' not found"
		Return cdecl
	End Method
End Rem

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
End Type

Type TPointerType Extends TType

End Type

Type TPointerPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TPointerPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Pointer Ptr"
	End Method

End Type

Type TVarPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "VarPtr"
	End Method

End Type

Type TIdentVarPtrType Extends TIdentType
End Type

Type TBytePtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TBytePtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TByteVarPtrPtrType( ty )<> Null
	End Method
	
	Method ToString$()
		Return "Byte Ptr"
	End Method

End Type

Type TByteVarPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TByteVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TStringType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Byte Var"
	End Method

End Type

Type TByteVarPtrPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TByteVarPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Byte Ptr Var"
	End Method

End Type

Type TShortPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TShortPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TShortVarPtrPtrType( ty )<> Null
	End Method
	
	Method ToString$()
		Return "Short Ptr"
	End Method

End Type

Type TShortVarPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TShortVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TStringType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Short Var"
	End Method

End Type

Type TShortVarPtrPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TShortVarPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Short Ptr Var"
	End Method

End Type

Type TIntPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TIntPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TShortVarPtrPtrType( ty )<> Null
	End Method
	
	Method ToString$()
		Return "Int Ptr"
	End Method

End Type

Type TIntVarPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TIntVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TStringType( ty )<>Null Or TIntType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Int Var"
	End Method

End Type

Type TIntVarPtrPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TIntVarPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Int Ptr Var"
	End Method

End Type

Type TFloatPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TFloatPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TFloatVarPtrPtrType( ty )<> Null
	End Method
	
	Method ToString$()
		Return "Float Ptr"
	End Method

End Type

Type TFloatVarPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TFloatVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TStringType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Float Var"
	End Method

End Type

Type TFloatVarPtrPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TFloatVarPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TDoubleVarPtrPtrType( ty )<> Null
	End Method
	
	Method ToString$()
		Return "Float Ptr Var"
	End Method

End Type

Type TDoublePtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TDoublePtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Byte Ptr"
	End Method

End Type

Type TDoubleVarPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TDoubleVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TStringType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Double Var"
	End Method

End Type

Type TDoubleVarPtrPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TDoubleVarPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TLongVarPtrPtrType( ty )<> Null
	End Method
	
	Method ToString$()
		Return "Double Ptr Var"
	End Method

End Type

Type TLongPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TLongPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Long Ptr"
	End Method

End Type

Type TLongVarPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TLongVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TStringType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Long Var"
	End Method

End Type

Type TLongVarPtrPtrType Extends TVarPtrType

	Method EqualsType:Int( ty:TType )
		Return TLongVarPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Long Ptr Var"
	End Method

End Type

Type TStringPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TStringPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "String Ptr"
	End Method

End Type

Type TStringVarPtrType Extends TVarPtrType

	Field cdecl:TClassDecl

	Method EqualsType:Int( ty:TType )
		Return TStringVarPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null Or TStringType( ty )<>Null
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
	
	Method ToString$()
		Return "String Var"
	End Method

End Type

Type TStringCharPtrType Extends TPointerType

	Field cdecl:TClassDecl

	Method EqualsType:Int( ty:TType )
		Return TStringCharPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
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
	
	Method ToString$()
		Return "$z"
	End Method

End Type

Type TStringShortPtrType Extends TPointerType

	Method EqualsType:Int( ty:TType )
		Return TStringShortPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "$w"
	End Method

End Type

Type TObjectVarPtrType Extends TVarPtrType
	Field classDecl:TClassDecl
	
	Method Create:TObjectVarPtrType( classDecl:TClassDecl )
		Self.classDecl=classDecl
		Return Self
	End Method
	
	Method ActualType:TType()
		If classDecl.actual=classDecl Return Self
		Return New TObjectVarPtrType.Create( TClassDecl(classDecl.actual) )
	End Method
	
	Method EqualsType:Int( ty:TType )
		Local objty:TObjectType=TObjectType( ty )
		Return TNullDecl(classDecl) <> Null Or (objty And (classDecl=objty.classDecl Or classDecl.ExtendsClass( objty.classDecl ))) Or TPointerType( ty )<>Null Or TObjectType(ty)
	End Method
	
	Method ExtendsType:Int( ty:TType )
		Local objty:TObjectType=TObjectType( ty )
		If objty Return classDecl.ExtendsClass( objty.classDecl )
		If TBytePtrType( ty ) Return True
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
End Type

Type TFunctionPtrType Extends TPointerType

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
		Return TPointerType( ty )<>Null
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
		If Not func.retType.equalsType(fdecl.retType) Return False
		
		Return True
	End Method
	
	Method ToString$()
		Return "Function Ptr"
	End Method

End Type

Type TBytePtrPtrType Extends TPointerPtrType

	Method EqualsType:Int( ty:TType )
		Return TBytePtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Byte Ptr Ptr"
	End Method

End Type

Type TShortPtrPtrType Extends TPointerPtrType

	Method EqualsType:Int( ty:TType )
		Return TShortPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Short Ptr Ptr"
	End Method

End Type

Type TIntPtrPtrType Extends TPointerPtrType

	Method EqualsType:Int( ty:TType )
		Return TIntPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Int Ptr Ptr"
	End Method

End Type

Type TFloatPtrPtrType Extends TPointerPtrType

	Method EqualsType:Int( ty:TType )
		Return TFloatPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Float Ptr Ptr"
	End Method

End Type

Type TDoublePtrPtrType Extends TPointerPtrType

	Method EqualsType:Int( ty:TType )
		Return TDoublePtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Double Ptr Ptr"
	End Method

End Type

Type TLongPtrPtrType Extends TPointerPtrType

	Method EqualsType:Int( ty:TType )
		Return TLongPtrPtrType( ty )<>Null
	End Method
	
	Method ExtendsType:Int( ty:TType )
		If TObjectType( ty )
			Local expr:TExpr=New TConstExpr.Create( Self,"" ).Semant()
			Local ctor:TFuncDecl=ty.GetClass().FindFuncDecl( "new",[expr],True )
			Return ctor And ctor.IsCtor()
		EndIf
		Return TPointerType( ty )<>Null
	End Method
	
	Method ToString$()
		Return "Long Ptr Ptr"
	End Method

End Type
