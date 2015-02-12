package rahul.montecarlo

/*
 * Author:  Rahul Chandran
 * 
 * Class For Nagel- Schreckenberg Traffic Modelling algorithm which is a simple Monte Carlo algorithm
 * 
 * ~~~~~~~~~~~
 * Description
 * ~~~~~~~~~~~
 * [a] Variables involved
 * ----------------------- 
 * (1) Roadway is divided into 'M' zones, each of which holds one vehicle
 * (2) There are 'N' vehicles in the road
 * (3) Time moves forward in discrete steps
 * (4) A vehicle with velocity 'V' moves ahead by 'v' zones, in the road way of next step
 * (5) There is a maximum speed Vmax which all vehicles obey
 * (6) For simpler case we take roadway as circular loop
 * 
 * [b] Rules
 * ----------
 * (1) First, if its velocity is below Vmax, it increases its velocity by one unit. 
 * (2) Second, it checks the distance to the car in front of it. If that distance is d spaces and the car has velocity v > d 
 *      then it reduces its velocity to d − 1 in order to avoid collision.
 * (3) If the velocity is positive, then with probability 'p' , it reduces velocity by 1 unit. This is the important step, 
 *      which introduces randomness in driver behaviour
 * (4) The car moves ahead by 'v' units to complete the stage. These four steps are taken in parellel for all 'N' vehicles
 * 
 */

class NagelSchkbrg(val numZones:Int, // represents 'M'
    val numVehicles:Int, // represents N
    val speedLimit: Double, // represents Vmax
    val probRandSlowing:Double // represents p
    ) {
   def generateTrafficVis = {
     
   }
}

object NSTest extends App{
  
}