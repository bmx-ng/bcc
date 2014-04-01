Rem
	This test checks:
	- play sound
End Rem
SuperStrict
Framework BRL.StandardIO
Import BRL.Freeaudioaudio
Import BRL.Audio
Import BRL.WavLoader
Import BRL.OggLoader
Import BRL.Timer
'SetAudioDriver("FreeAudio PulseAudio System")

local timer:TTimer = CreateTimer(50)

For local driver:string = eachin AudioDrivers()
	print "available driver: "+driver
Next

local sound:TSound

'try WAV
sound = LoadSound("audio_01.wav")
if sound
	For local i:int = 0 to 2
		WaitTimer(timer)
		PlaySound(sound)
	Next
	print "wav sound played"
endif

'try OGG
sound = LoadSound("audio_01.ogg")
if sound
	For local i:int = 0 to 2
		WaitTimer(timer)
		PlaySound(sound)
	Next
	print "ogg sound played"
endif