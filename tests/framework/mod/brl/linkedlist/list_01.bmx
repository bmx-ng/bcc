SuperStrict

Framework BRL.LinkedList
Import BRL.StandardIO

Local planets:String[] = ["Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"]

Local list:TList = CreateList()

For Local planet:String = EachIn planets
	list.AddLast(planet)
Next

Print "IsEmpty = " + list.IsEmpty()
Print "Count   = " + list.Count()
Print "First   = " + String(list.First())
Print "Last    = " + String(list.Last())
Print "Contains(Pluto) = " + list.Contains("Pluto")
Print "Contains(Earth) = " + list.Contains("Earth")
Print "First Removed   = " + String(list.RemoveFirst())
Print "First   = " + String(list.First())
Print "Last Removed    = " + String(list.RemoveLast())
Print "Last    = " + String(list.Last())
Print "Count   = " + list.Count()
Print "ValueAtIndex(2) = " + String(list.ValueAtIndex(2))

