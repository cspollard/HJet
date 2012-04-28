import Jet
import LorentzVector

data State = BeginData | BeginEvent | EndEvent | EndData

main = readDataFromRoot

readDataFromRoot :: IO ()
readDataFromRoot = do
    l <- getLine
    case parseControl l of
        BeginEvent -> parseEventData []
        EndData -> return ()
        otherwise -> readDataFromRoot

parseControl :: String -> State
parseControl l
    | l == "<event>" = BeginEvent
    | l == "</event>" = EndEvent
    | otherwise = EndData

parseEventData :: [BTree Cluster] -> IO ()
parseEventData cls = do
    l <- getLine
    if l == "</event>"
        then writeJets (aktJets 0.4 cls) >> readDataFromRoot
        else parseEventData $ lineToCluster l : cls

lineToCluster :: String -> BTree Cluster
lineToCluster l = BNode (fromPtEtaPhiE (read l)) BNil BNil

writeJets :: [BTree Cluster] -> IO ()
writeJets jets = mapM_ (putStrLn . show . toPtEtaPhiE . getData) jets
