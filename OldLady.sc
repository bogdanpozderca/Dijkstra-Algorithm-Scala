/*
*	Author: Bogdan Pozderca
* Scala 2.11.6 Dijkstra's Algorithm Problem
* JavaSE-1.8
*
* This is an implementation of Dijkstra's Algorithm.
*	This is the challenge problem submission for interning at Blue Medora
*
*/



object OldLady {
	def shortestDistance(startingLocation:String, targetLocation:String, edges:List[Map[String,Any]]):Map[String,Any] = {
				
		val dist = collection.mutable.Map[String,Double](startingLocation -> 0)  // Stores the min distance from the
																											  										 // startingLocation to all locations.
		
		val prev = collection.mutable.Map[String,Any](startingLocation -> null)  // Stores the path for the min distance
																										 												 // from the starting Location to all locations.
		
		var existCheck = collection.mutable.Buffer[String]() // Buffer with names of all nodes in edges List
		
		def buildExistCheck(line:Map[String,Any]) = {
			existCheck += line("startLocation").toString
			existCheck += line("endLocation").toString
		}
		
		/* Initializes dist with default values of Infinity for all locations except startingLocation (0).
		   Initializes prev with default values of null. */
		def initialization(line:Map[String,Any]) = {
			if (line("startLocation") != startingLocation && !dist.contains(line("startLocation").toString)) {
				dist += line("startLocation").toString -> Double.PositiveInfinity
				prev += line("startLocation").toString -> null
			}
			if (line("endLocation") != startingLocation && !dist.contains(line("endLocation").toString)) {
				dist += line("endLocation").toString -> Double.PositiveInfinity
				prev += line("endLocation").toString -> null
			}
		}
		
		/* Outputs the node in notVisited with the min distance defined by the dist Buffer. */
		def getMin(notVisited:collection.mutable.Buffer[String]):String = {
			val tempDist = dist.clone()
			var theMin = ""
			var hi = 1
			while (theMin == "") {
				if (notVisited.contains(tempDist.minBy(_._2)._1.toString)) theMin = tempDist.minBy(_._2)._1 else tempDist -= tempDist.minBy(_._2)._1
			}
			theMin
		}
		
		/* Builds a buffer that contains all valid node Names. */
		edges foreach buildExistCheck
		
		/* Handles a a misspelled starting or target location, also an empty edges List */
		if (edges.size == 0) {
			println("The edges list does not exist.")
			Map("distance" -> 0, "path" -> "")
		}else if (!existCheck.contains(startingLocation)) {
			println("The starting location does not exist.")
			Map("distance" -> 0, "path" -> "")
		} else if (!existCheck.contains(targetLocation)) {
			println("The target location does not exist.")
			Map("distance" -> 0, "path" -> "")
		} else {
		
			/* Calls initialization on edges. */
		  edges foreach initialization
		  	 
		  val unvisited = dist.keys.toBuffer  // Creates a buffer of all unvisited nodes
		  
		 	/* While loop runs until there are no nodes unvisited. */
		  while (!unvisited.isEmpty) {
		  	var currentNode = getMin(unvisited)
		  	
		  	unvisited -= currentNode
		  	
		  	
		  	/* Calculates the shortest path from the currentNode to it's neighbors and adds it to the
		  	   distance of the currentNode from the starting Location */
		  	for (line <- edges) {
		  		if (line("startLocation") == currentNode) {
		  			var alt = dist(currentNode).toInt + line("distance").toString.toInt
		  			if (alt < dist(line("endLocation").toString)) {
		  				dist(line("endLocation").toString) = alt
		  				prev(line("endLocation").toString) = currentNode
		  			}
		  		}else if (line("endLocation") == currentNode) {
		  			var alt = dist(currentNode).toInt + line("distance").toString.toInt
		  			if (alt < dist(line("startLocation").toString)) {
		  				dist(line("startLocation").toString) = alt
		  				prev(line("startLocation").toString) = currentNode
		  			}
		  		}
		  	}
		  }
		  
		  
		  var path = collection.mutable.Buffer[String](targetLocation)
		  var pathString = ""
			var pathElement	 = targetLocation
		  
		  
		  /* Retrieves the shortest path between the startingLocation and the targetLocation. */
		  while (prev(pathElement) != null) {
		  	prev(pathElement).toString+=:path
		  	pathElement = prev(pathElement).toString
		  }
		  
		  /* Formats the retrieved path as a string. */
		  for (element <- path) {
		  	pathString += element + " => "
		  }
		  pathString = pathString.dropRight(4)
		  
		  Map("distance" -> dist(targetLocation).toInt, "path" -> pathString)
		
		}
	}                                         //> shortestDistance: (startingLocation: String, targetLocation: String, edges:
                                                  //|  List[Map[String,Any]])Map[String,Any]
	val theEdges =
		List(
		  Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Mark's crib", "distance" -> 9),
		  Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Greg's casa", "distance" -> 4),
		  Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Matt's pad", "distance" -> 18),
		  Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Brian's apartment", "distance" -> 8),
		  Map("startLocation" -> "Brian's apartment", "endLocation" -> "Wesley's condo", "distance" -> 7),
		  Map("startLocation" -> "Brian's apartment", "endLocation" -> "Cam's dwelling", "distance" -> 17),
		  Map("startLocation" -> "Greg's casa", "endLocation" -> "Cam's dwelling", "distance" -> 13),
		  Map("startLocation" -> "Greg's casa", "endLocation" -> "Mike's digs", "distance" -> 19),
		  Map("startLocation" -> "Greg's casa", "endLocation" -> "Matt's pad", "distance" -> 14),
		  Map("startLocation" -> "Wesley's condo", "endLocation" -> "Kirk's farm", "distance" -> 10),
		  Map("startLocation" -> "Wesley's condo", "endLocation" -> "Nathan's flat", "distance" -> 11),
		  Map("startLocation" -> "Wesley's condo", "endLocation" -> "Bryce's den", "distance" -> 6),
		  Map("startLocation" -> "Matt's pad", "endLocation" -> "Mark's crib", "distance" -> 19),
		  Map("startLocation" -> "Matt's pad", "endLocation" -> "Nathan's flat", "distance" -> 15),
		  Map("startLocation" -> "Matt's pad", "endLocation" -> "Craig's haunt", "distance" -> 14),
		  Map("startLocation" -> "Mark's crib", "endLocation" -> "Kirk's farm", "distance" -> 9),
		  Map("startLocation" -> "Mark's crib", "endLocation" -> "Nathan's flat", "distance" -> 12),
		  Map("startLocation" -> "Bryce's den", "endLocation" -> "Craig's haunt", "distance" -> 10),
		  Map("startLocation" -> "Bryce's den", "endLocation" -> "Mike's digs", "distance" -> 9),
		  Map("startLocation" -> "Mike's digs", "endLocation" -> "Cam's dwelling", "distance" -> 20),
		  Map("startLocation" -> "Mike's digs", "endLocation" -> "Nathan's flat", "distance" -> 12),
		  Map("startLocation" -> "Cam's dwelling", "endLocation" -> "Craig's haunt", "distance" -> 18),
		  Map("startLocation" -> "Nathan's flat", "endLocation" -> "Kirk's farm", "distance" -> 3),
		  Map("startLocation" -> "Kruthika's abode", "endLocation" -> "Bogdan's box", "distance" -> 15)
		)                                 //> theEdges  : List[scala.collection.immutable.Map[String,Any]] = List(Map(sta
                                                  //| rtLocation -> Kruthika's abode, endLocation -> Mark's crib, distance -> 9),
                                                  //|  Map(startLocation -> Kruthika's abode, endLocation -> Greg's casa, distanc
                                                  //| e -> 4), Map(startLocation -> Kruthika's abode, endLocation -> Matt's pad, 
                                                  //| distance -> 18), Map(startLocation -> Kruthika's abode, endLocation -> Bria
                                                  //| n's apartment, distance -> 8), Map(startLocation -> Brian's apartment, endL
                                                  //| ocation -> Wesley's condo, distance -> 7), Map(startLocation -> Brian's apa
                                                  //| rtment, endLocation -> Cam's dwelling, distance -> 17), Map(startLocation -
                                                  //| > Greg's casa, endLocation -> Cam's dwelling, distance -> 13), Map(startLoc
                                                  //| ation -> Greg's casa, endLocation -> Mike's digs, distance -> 19), Map(star
                                                  //| tLocation -> Greg's casa, endLocation -> Matt's pad, distance -> 14), Map(s
                                                  //| tartLocation -> Wesley's condo, endLocation -> Kirk's farm, distance -> 10)
                                                  //| , Map(startLocation -> Wesley's condo, endLocation -> Nathan's flat, distan
                                                  //| ce -> 11), Map(startLocation -> Wesley's condo, endLocation -> Bryce's den,
                                                  //|  distance -> 6), Map(startLocation -> Matt's pad, endLocation -> Mark's cri
                                                  //| b, distance -> 19), Map(startLocation -> Matt's pad, endLocation -> Nathan'
                                                  //| s flat, distance -> 15), Map(startLocation -> Matt's pad, endLocation -> Cr
                                                  //| aig's haunt, distance -> 14), Map(startLocation -> Mark's crib, endLocation
                                                  //|  -> Kirk's farm, distance -> 9), Map(startLocation -> Mark's crib, endLocat
                                                  //| ion -> Nathan's flat, distance -> 12), Map(startLocation -> Bryce's den, en
                                                  //| dLocation -> Craig's haunt, distance -> 10), Map(startLocation -> Bryce's d
                                                  //| en, endLocation -> Mike's digs, distance -> 9), Map(startLocation -> Mike's
                                                  //|  digs, endLocation -> Cam's dwelling, distance -> 20), Map(startLocation ->
                                                  //|  Mike's digs, endLocation -> Nathan's flat, distance -> 12), Map(startLocat
                                                  //| ion -> Cam's dwelling, endLocation -> Craig's haunt, distance -> 18), Map(s
                                                  //| tartLocation -> Nathan's flat, endLocation -> Kirk's farm, distance -> 3), 
                                                  //| Map(startLocation -> Kruthika's abode, endLocation -> Bogdan's box, distanc
                                                  //| e -> 15))
	
	
	// ShortestDistance(start,end,edges).
	// Known correct solution
	shortestDistance("Kruthika's abode","Craig's haunt", theEdges)
                                                  //> res0: Map[String,Any] = Map(distance -> 31, path -> Kruthika's abode => Bri
                                                  //| an's apartment => Wesley's condo => Bryce's den => Craig's haunt)
                                                  
  // Handles a misspelled Starting Location.
  shortestDistance("Brian's apdartment","Craig's haunt", theEdges)
                                                  //> The starting location does not exist.
                                                  //| res1: Map[String,Any] = Map(distance -> 0, path -> "")
                                                
	// Handles a misspelled target Location.
  shortestDistance("Brian's apartment","Craig's Boat", theEdges)
                                                  //> The target location does not exist.
                                                  //| res2: Map[String,Any] = Map(distance -> 0, path -> "")
                                                  
  // Handles a starting location that is not listed as startLocation in the provided Edges.
  	shortestDistance("Brian's apartment","Craig's Boat", theEdges)
                                                  //> The target location does not exist.
                                                  //| res3: Map[String,Any] = Map(distance -> 0, path -> "")
                                                  
  // Handles an empty edges list.
  shortestDistance("Brian's apartment","Craig's Boat", List())
                                                  //> The edges list does not exist.
                                                  //| res4: Map[String,Any] = Map(distance -> 0, path -> "")
  // Handles a one node target
  shortestDistance("Brian's apartment","Bogdan's box", theEdges)
                                                  //> res5: Map[String,Any] = Map(distance -> 23, path -> Brian's apartment => Kr
                                                  //| uthika's abode => Bogdan's box)

}