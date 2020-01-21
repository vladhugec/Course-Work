structure StringKey :> KEY where type key = string = 
struct
	type key = string

	fun eqKey(s1,s2) = s1 = s2

end