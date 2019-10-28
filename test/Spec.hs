import GmshAPI

main :: IO ()
main = do
   gmshInitialize [] Nothing
   gmshModelOccAddDisk 0 0 0 1 1 Nothing
   gmshModelOccSynchronize
   gmshFltkRun
   gmshFinalize
