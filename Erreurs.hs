planetes :: [(Double, String)]
planetes = [(0.39, "Mercure"), (0.72, "Venus"), (1.00, "Terre"),(1.52, "Mars"), (5.20, "Jupiter"),(9.54, "Saturne"), (19.2, "Uranus"),(30.1, "Neptune")]


cherche :: Eq b => [(a,b)] -> b -> Maybe a
cherche [] _ = Nothing
cherche  list@(x:xs)  target | snd x == target  =  Just (fst x)
                             | otherwise        = cherche xs target




