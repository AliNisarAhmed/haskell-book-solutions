module IsPalindrome where
  
  isPalindrom :: String -> Bool
  isPalindrom str = 
    str == rvrs
    where 
      rvrs = reverse str