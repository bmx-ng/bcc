Rem
	===========================================================
	BASIC CONFIGURATION MAP
	===========================================================

	Code contains an class to hold certain key->value pairs.

ENDREM
SuperStrict
Import BRL.Map
Import BRL.FileSystem

Type TConfigMap
	Field values:TMap = CreateMap()
	Field fileUri:String = ""


	Method Init:TConfigMap( configFile:String="" )
		If configFile <> "" Then LoadFromFile(configFile)

		Return Self
	End Method


	'clear all key->value pairs
	Method Reset:Int()
		values.Clear()
		Return True
	End Method


	'create another configMap with the same values
	Method Copy:TConfigMap()
		Local copyObj:TConfigMap = New TConfigMap

		'copy values
		For Local key:String = EachIn values.Keys()
			copyObj.Add(key, Get(key))
		Next
		Return copyObj
	End Method


	'create a merged configMap of all given configurations (eg. base + extension)
	Function CreateMerged:TConfigMap( configs:TConfigMap[], reversed:Int = False )
		If configs.length = 0 Then Return Null

		If reversed
			Local newConfigs:TConfigMap[]
			For Local i:Int = 1 To configs.length
				newConfigs :+ [configs[configs.length - i]]
			Next
			configs = newConfigs
		EndIf


		Local result:TConfigMap = configs[0].copy()
		For Local i:Int = 1 To configs.length-1
			'overwrite values or add new if not existing
			For Local key:String = EachIn configs[i].values.Keys()
				Local value:Object = configs[i].Get(key)
				If value Then result.Add(key, value)
			Next
		Next
		Return result
	End Function

	'try to load the configuration from a file
	Method LoadFromFile:Int( fileUri:String )
		'skip resetting and loading if the file is not existing
		If FileSize(fileUri) < 0 Then Return False

		Self.fileUri = fileUri

		'remove old values
		Reset()

		Local file:TStream = ReadFile(fileUri)
		If Not file
			'RuntimeError("ERROR: could not open file ~q"+fileUri+"~q for reading.")
			Print "ERROR: could not open file ~q"+fileUri+"~q for reading."
			Return False
		EndIf

		Local line:String = ""
		Local splitPos:Int = 0
		Local key:String, value:String
		While Not Eof(file)
			line = ReadLine(file)

			'skip #comments
			If line.Trim().Find("#") = 0 Then Continue

			'find first "=" (later ones could come from arguments/params)
			splitPos = line.Find("=")
			'no splitter means no assignment
			If splitPos < 0 Then Continue

			key = line[..splitPos].Trim()
			value = line[splitPos+1..].Trim()

			Add(key, value)
		Wend

		file.Close()
		Return True
	End Method


	Method ToString:String()
		Local result:String = "TConfigMap"+"~n"
		result :+ "-> file: "+Self.fileUri+"~n"
		result :+ "-> keys:"+"~n"
		For Local key:String = EachIn values.Keys()
			result :+ "  -> "+key+" : "+String(values.ValueForKey(key))+"~n"
		Next
		Return result
	End Method


	Method Add:TConfigMap( key:String, data:Object )
		values.insert(key, data)
		Return Self
	End Method


	Method AddString:TConfigMap( key:String, data:String )
		Add(key, Object(data))
		Return Self
	End Method


	Method AddNumber:TConfigMap( key:String, data:Float )
		Add( key, Object( String(data) ) )
		Return Self
	End Method


	Method Get:Object( key:String, defaultValue:Object=Null )
		Local result:Object = values.ValueForKey(key)
		If result Then Return result
		Return defaultValue
	End Method


	Method GetString:String( key:String, defaultValue:String=Null )
		Local result:Object = Get(key)
		If result Then Return String( result )
		Return defaultValue
	End Method


	Method GetInt:Int( key:String, defaultValue:Int = Null )
		Local result:Object = Get(key)
		If result Then Return Int( Float( String( result ) ) )
		Return defaultValue
	End Method
End Type