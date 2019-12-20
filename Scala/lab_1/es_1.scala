/** is_palindrome: string → bool that checks if the string given as input is palindrome, a string is palindrome when the represented sentence can be read the same way in either directions in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...; **/

def clean(s: String) =
    for(c<-s if c.isLetter)
        yield c.toLower


def is_palindrome (s: String) = {
    var s1 = clean(s); 
    s1 == s1.reverse
}

/**is_an_anagram : string → string list → boolean that given a dictionary of strings, checks if the input string is an anagram of one or more of the strings in the dictionary;*/

def is_an_anagram(s: String, l: List[String]): Boolean = {
    for(str<-l)
        if(str.sorted == s.sorted)
            return true;
    return false
}
/**factors: int → int list that given a number calculates all its prime factors;*/

def factors(n:Int, div:Int = 2): List[Int] = {
    if(div <= n){
        if(n%div==0)
            return div :: factors(n/div, 2)
        else
            return factors(n, div+1)
    }
    return List()
}

/**squared_numbers that removes all non-numbers from a polymorphic list and returns the resulting list of squared numbers, e.g., squared_numbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5,7,'a') :: Nil) should return List(1, 10000, 9.8596, List(100), (25,49)). Note that it recursively applies to substructures.*/

def squared_numbers(l:List[Any]): List[Any] = {
    for(x<-l){
        if (x.isInstanceOf[Int])
            yield x.toInt * x.toInt
        else if (x.isInstanceOf[Double])
            yield x.toDouble * x.toDouble
    }
}