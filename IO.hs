import Data.List


afficher_avec_etoiles :: Show a => a -> IO ()
afficher_avec_etoiles a =  putStrLn ("***" ++ show a ++ "***")

combine :: IO a -> IO b -> IO b
combine a b  = do 
    temp <- a 
    b

etoiles :: IO ()
etoiles = putStrLn "***"

retour_ligne :: IO ()
retour_ligne = putStrLn "\n"


afficher_avec_etoiles2 :: Show a => a -> IO ()
afficher_avec_etoiles2 value = combine etoiles (combine (putStrLn (show value)) etoiles) 
-- remarque : cette fonction n'est pas strictement égale à afficher_avec_etoiles puisque chaque action est séparée par un retour à la ligne.

afficher_etoiles3 :: Show a => a -> IO ()
afficher_etoiles3 value = etoiles >> putStrLn (show value) >> etoiles 



echo :: IO ()
echo = do 
    s <- getLine
    putStrLn (s)

echo2 :: IO ()
echo2 = getLine >>= putStrLn 


wc :: String -> IO ()
wc file = 