

object RSA_Main {
  
  
  //to do:
  // recursive implementation of lcm multiplying out factors
  // Plain text to encoded text 
  // encoded text ot plain text
  // BigInt datatype for larger keys we want 1024 bit ints but the standard datatype 'Int' takes only 32 bit
  // BigInt implementation of the current functions 
  // extended Euclidian algorithm implementation for finding KeyD
  // catches for wrong data inputs
  
  
  def main (args: Array[String])  = {
    println("hello world")
   
    println("Generating Keys")
    
    // variables for the outputs of the function genkeys to later print
    var PublicPair : (Int,Int) = (0,0)
    var PrivatePair : (Int,Int) = (0,0)
    var BothKeys : ((Int,Int),(Int,Int)) = ((0,0),(0,0))
    
    
    //unpacking the keys
    BothKeys = genKeys(61,53)
    PublicPair = BothKeys._1
    PrivatePair = BothKeys._2
    
    println("Public Key : " ++ PublicPair.toString())
    println("Private Key : " ++ PrivatePair.toString())
    //seems to work 
    
    
    /*
    var TestInt1 : Int = 827569348
    
    var TestList1 = factorise(TestInt1)
    
    while(TestList1.isEmpty == false){
      
      println(TestList1.head)
      TestList1 = TestList1.tail
     
    }
    */
    // quick test for factorise 
     
    /* 
    var TestInt2 =  2345
    var TestInt3 =  1234
    
    println(lcm(TestInt2,TestInt3))
    
    println("Done")
    */
    //quick test for lcm
    
    
    
    
    
  }//end main
  
  
  
  
  
  
  
  // This function takes two prime numbers and returns a tuple containing public key (KeyN,KeyE) and a private key (KeyN, KeyD) 
  // this function generates both keys as the same initial primes are need to make sure the keys are bound to each other
  def genKeys(PrimeP : Int , PrimeQ : Int) : ((Int,Int),(Int,Int)) = {
    
    //defs for variables
    // keyN will be our generated public key and is actually generated here 
    var KeyN : Int = PrimeP * PrimeQ 
    // KeyE will be a set key for now but later we will give the option to generate or define it 
    var KeyE : Int = 65537
    //KeyD will be our generated private key
    var KeyD : Int = 0
    //CarmN is the LCM of  PrimeP -1 and PrimeQ -1
    //this is called CarmN as the RSA Algorithm used the Carmichael function 
    // we will use a specific case that also works and is easier to calculate 
    var CarmN : Int = lcm(PrimeP - 1,PrimeQ - 1)
    
   //to generate KeyD we need to calculate a value d where d*KeyE = 1 Mod CarmN 
   //so d is the modular multiplicative inverse of KeyE mod CarmN 
   // I will do this inefficiently with a brute force method but we can also use the extended Euclidean algorithm 
    
    KeyD = slowKeyD(KeyE,CarmN)
    
    //output the keys in the order stated at the start of the function 
    return ((KeyN,KeyE),(KeyN,KeyD))
    
  }//end genKeys
  
  
  // this will at the moment only work if there is a solution we know this has failed if it return the same value as CarmN
  //this is the brute force method for calculating KeyD
  //   1 < KeyD < CarmN 
  def slowKeyD (PassedKeyE : Int, PassedCarmN :Int ) : Int = {
    
    //Out will be our counter and our output
    var Out : Int = 2 
    
    //we iterate through every possible value of Out for KeyD
    while (((Out * PassedKeyE) % PassedCarmN != 1) && (Out < PassedCarmN)){
      Out +=1 
    }//end while
    
    return Out
  }//end slowKeyD
  
  
  
  
  
  
  
  // lcm function finds the lowest common multiple of two numbers
  // we will factorise the two numbers into a sorted list of factors and their frequency 
  // then we pop out the head of the lists and compare and multiply by the factor to the largest power 
  def lcm( Left : Int, Right : Int  ) : Int = {
    //output form the function 
    var Out : Int = 1
    
    
    // define and assign the lists of factors 
    var LeftFactors : List[(Int,Int)] = factorise(Left).reverse
    var RightFactors : List[(Int,Int)] = factorise(Right).reverse
    //println("factors found")
    
    
    //define the temp vars for the heads of the lists 
    var LeftHead : (Int, Int) = (1,1)
    var RightHead : (Int,Int) = (1,1) 
    // values are just place holders 
    
    //End condition for loop boolean
    var EndCon : Boolean = false
    
    // this while would probably be more clear using a recursive function instead 
    // list terminates when both lists are empty 
    while( !(EndCon)){
           
      // taking the heads of the lists
      if (!(LeftFactors.isEmpty)){LeftHead = LeftFactors.head}
      if (!(RightFactors.isEmpty)){RightHead = RightFactors.head}
      // we put the if statements so we dont get an error when executing head on an empty list 
      // todo: implement with type Option[(Int,Int)]
      
      
      
      //println(LeftHead.toString() ++ RightHead.toString())
      
      
      // a bit messy but works, could need cleaning up 
      // cases for if both or just one list is full 
      if(!(LeftFactors.isEmpty) && !(RightFactors.isEmpty)){
        
        //cases for finding the smallest factor currently at the head of the list 
        if (LeftHead._1 > RightHead._1){
          
          //If right factor is smaller then times out the right factor and frequency
          Out = Out * exp(RightHead._1 , RightHead._2)
          RightFactors = RightFactors.drop(1)
          //println("l>r")
          
        }else if(RightHead._1 > LeftHead._1){
          
          //If left factor is smaller then times out the left factor and frequency
          Out = Out * exp(LeftHead._1 , LeftHead._2)
          LeftFactors = LeftFactors.drop(1)
          //println("r>l")
          
        }else{
          //If both factors are thhe same size then times out by the factor and the highest frequency 
          Out = Out * exp(LeftHead._1 , (LeftHead._2.max(RightHead._2)))
          RightFactors = RightFactors.drop(1)
          LeftFactors = LeftFactors.drop(1)
          //println("l=r")
          
        }//end if 
        
      
      }else if(!(LeftFactors.isEmpty)){
        //this means the right list is empty now so we just times out all the left factors and their frequency 
        LeftHead = LeftFactors.head
        Out = Out * exp(LeftHead._1 , LeftHead._2)
        LeftFactors = LeftFactors.drop(1)
          //println("r=null")
        
      }else if(!(RightFactors.isEmpty)){
       //this means the left list is empty now so we just times out all the right factors and their frequency 
       RightHead = RightFactors.head
       Out = Out * exp(RightHead._1 , RightHead._2)
       RightFactors = RightFactors.drop(1)
          //println("l=null")
       
        
      }else{
        // if both lists are empty then we can end the loop
        EndCon = true
        
      }//end if 
      
      //println(Out)
      //println(LeftHead.toString() ++ RightHead.toString())
      
    }//end while
   
    return (Out)
  }//end LCM
  
  
  
  
  
  
  
  
  // this factorise function returns a list containing tuples of factors and their frequency 
  def factorise( Target : Int  ) :  List[(Int,Int)] = {
    
    // this is our output which contains a list of tubles (Factor, Frequency of factor )
    var FactorList : List[(Int,Int)] = List()
     // we define our counter variable it will also double up as the factor when filling in the output Factorlist 
    var Count : Int = 2
    // this is our variable for holding the temp frequency values 
    var Frequency : Int = 0 
    // This is our variable for keeping track of the product of the unfound factors
    var RestOf : Int = Target 
    
    
    
    
    //(Count ^ Frequency) * Restof * (all factors in FactorList) = Target   -should hold after any given iteration this loop
    while( Count <= RestOf){
      
      //Just for fun proof the loop terminates 
      
      //Variant(N) = RestOf(N) - Count(N) 
      // Let N be the iteration of the loop       
      //case 1    ll  RestOf(N+1) =  RestOf(N) / Count(N)    -> RestOf is strictly decreasing iff Count(a) > 1 for all a 
      //-> Variant is strictly decreasing iff Count(a) > 1 for all a 
      //case 2    ll  Count(N+1) = Count(N) +1               -> Count is strictly increasing -> Variant is strictly decreasing 
      //case 3    ll  Count(N+1) = Count(N) +1               -> "  -> " 
      
      // Count in all cases is strictly increasing or unchanged -> Count(N) is an increasing function 
      // Count(0) = 2  and count is increasing -> Count(a) > 1 for all a  -> case 1 variant is Strictly decreasing 
      // All cases strictly decease the variant -> The variant is strictly decreasing -> the loop will terminate 
      
      
      
      // the if checks if count is a factor of RestOf 
       if (RestOf % Count == 0) {
         // if Count is a factor then we divide and increment the frequency of that factor
         Frequency += 1 
         RestOf = RestOf / Count
       }else if(Frequency > 0){
         // if there are no more factors equal to count then we store these factors and move onto the next number to check
         // we check every number as if Count isn't a prime then Count's non prime factors should have already been factored out 
         FactorList = (Count, Frequency) :: FactorList 
         Count += 1
         //now freqency is stored we can reset it
         Frequency = 0 
       }else{
           // If the frequency is 0 then we just increment the count and store nothing in the list
         Count += 1
         
       }//end if 
      
    }//end while 
    
    //we append the last pair as the loop finished before we have chance to append it in this loop 
    FactorList =  (Count, Frequency) :: FactorList 
    
    
    
    return FactorList
  }//end factorsie 
  
  
  
  
  
  
  
  
// quick exponent function for integers  
def exp (x: Int, n: Int): Int = {
    if(n == 0) {
      //a^0 = 1
       return 1
    } else if(n == 1) {
      // a^1 = a
       return  x
    } else if(n%2 == 0) {
       // a^2n = (a^2)^n
       return exp(x*x, n/2)
       
    } else {
       //a^(2n+1) = a * (a^2)^n
       return x * exp(x*x, (n-1)/2)
    }//end if 
}//end exp
  
  
  
  
  
  
  
  
}//end RSA_Main


