module Week37Exercise0 where

information :: [String] -> [String] -> [Integer] -> [String]
information names institutions years =
  let combinedInfo = zip3 names institutions years
      filteredInfo = filter (\(_, _, year) -> year >= 2022) combinedInfo
  in map (\(name, inst, year) -> name ++ " is studying at " ++ inst ++ " department and started in " ++ show year) filteredInfo
