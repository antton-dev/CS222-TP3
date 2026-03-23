planetes :: [(Double, String)]
planetes = [(0.39, "Mercure"), (0.72, "Venus"), (1.00, "Terre"),(1.52, "Mars"), (5.20, "Jupiter"),(9.54, "Saturne"), (19.2, "Uranus"),(30.1, "Neptune")]


cherche :: Eq b => [(a,b)] -> b -> Maybe a
cherche [] _ = Nothing
cherche  list@(x:xs)  target | snd x == target  =  Just (fst x)
                             | otherwise        = cherche xs target




distance :: String -> Maybe Double
distance = cherche planetes


suivante :: Double -> Maybe String
suivante d | length list /= 0 = Just (snd (head list)) 
           | otherwise        = Nothing 
    where
        list = filter (\(dist, planete) -> dist > d) planetes


ua_vers_km :: Double -> Double
ua_vers_km d = d*1.496*10**8



distance_km_1 :: String -> Maybe Double
distance_km_1 p = case distance p of
    Nothing -> Nothing
    Just d  -> Just (ua_vers_km d)


fmap_Maybe :: (a -> b) -> Maybe a -> Maybe b
fmap_Maybe _ Nothing  = Nothing
fmap_Maybe fct (Just val) = Just (fct val)


distance_km_2 :: String -> Maybe Double
distance_km_2 = fmap_Maybe (ua_vers_km  distance) 
-- Not working