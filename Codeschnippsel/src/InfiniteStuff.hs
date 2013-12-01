-- zum Spielen:
-- * ghci starten
-- * Datei einlesen mit
--   :load InfiniteStuff
--   Main-Methode ausführen:
--   main


module InfiniteStuff
where 

-- natürliche und gerade Zahlen
--------------------------------

nat = [1..]
geradeZahlen = filter even nat

-- Nummerierung von Zeilen
------------------------------

zeilen = ["Eine","Zeile","nach","der","anderen"]

nummerierteZeilen = zip nat zeilen

nummerierteZeilenMitWenigerSpeicher = zip [1..] zeilen

-- alles einmal ausgeben

main = do
  print $ take 10 nat
  print $ take 10 geradeZahlen
  print nummerierteZeilen
  print nummerierteZeilenMitWenigerSpeicher