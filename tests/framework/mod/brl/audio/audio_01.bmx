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

local soundFiles:string[] = ["audio_01.wav", "audio_01.ogg"]
local channel:TChannel
local sound:TSound


'loop through drivers
For local driver:string = eachin AudioDrivers()
	'default blitzmax modules have "null" driver which segfaults
	'-> skip it
	if driver.ToLower() = "null" then continue

	print "Trying Audio Driver: "+driver
	SetAudioDriver(driver)

	For local f:string = EachIn soundFiles
		sound = LoadSound(f)
		if not sound
			print " loading of ~q"+f+"~q failed."
			continue
		endif
		print "playing: "+f

		channel = PlaySound(sound)
		While channel.Playing()
			delay(10)
		Wend
		print "stopped: "+f
	Next
Next


