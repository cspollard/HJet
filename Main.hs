import Jet
import LorentzVector

data State = BeginData | BeginEvent | EndEvent | EndData

main = readDataFromRoot

readDataFromRoot = do
    l <- getLine
    case parseControl l of
        BeginEvent -> parseEventData []
        EndData -> return ()
        otherwise -> readDataFromRoot

parseControl l
    | l == "<event>" = BeginEvent
    | l == "</event>" = EndEvent
    | otherwise = EndData

parseEventData cls = do
    l <- getLine
    if l == "</event>"
        then writeJets (aktJets 0.4 cls) >> readDataFromRoot
        else parseEventData $ lineToCluster l : cls

lineToCluster l = BNode (read ("Vec4 " ++ l)) BNil BNil

writeJets jets = mapM_ (putStrLn . show . getData) jets
