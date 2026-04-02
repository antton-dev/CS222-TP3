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
distance_km_2 = fmap_Maybe ua_vers_km . distance

distance_km_3 :: String -> Maybe Double
distance_km_3 = fmap ua_vers_km . distance

planete_suivante :: String -> Maybe String
planete_suivante p = case distance p of
    Nothing -> Nothing
    Just d  -> case suivante d of
        Nothing -> Nothing
        Just s  -> Just s


bind_Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bind_Maybe Nothing _ = Nothing
bind_Maybe (Just a) f = f a


planete_suivante_2 :: String -> Maybe String
planete_suivante_2 p = bind_Maybe (distance p) suivante

planete_suivante_3 :: String -> Maybe String
planete_suivante_3 p = do
    d <- distance p
    suivante d

distance_suivante_1 :: String -> Maybe Double
distance_suivante_1 p = case distance p of
    Nothing -> Nothing
    Just d  -> case suivante d of
        Nothing -> Nothing
        Just s  -> distance s

distance_suivante_2 :: String -> Maybe Double
distance_suivante_2 p = do
    d <- distance p
    suiv <- suivante d
    distance suiv

distance_suivante_3 :: String -> Maybe Double
distance_suivante_3 p = distance p >>= suivante >>= distance