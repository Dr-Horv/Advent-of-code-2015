
import qualified Data.ByteString as B (readFile)
import qualified Data.Aeson as JSON 

main :: IO ()
main = do 
    content <- B.readFile "input.json"
    let decoded = JSON.decodeStrict content :: Maybe JSON.Object
    let value = (\(Just x)-> x) decoded
    print $ value
    return ()

    