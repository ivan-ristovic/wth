doubleEvery [] = []
doubleEvery l = foldr (\x acc -> x : x : acc) [] l
