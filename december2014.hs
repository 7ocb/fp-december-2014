
import Data.Maybe (fromJust)
import Data.List
import Data.Ord
import Control.Monad (forM_)

data BugProp = BugProp { 
      bugPropName :: String,
      bugPropVariants :: [Maybe String] 
    } deriving (Show, Eq)

data BugsDef = BugsDef [String] [BugProp]
             deriving (Show, Eq)

bugsDef :: BugsDef
bugsDef = BugsDef                             ["Аурата сетуньская", "Десятилиньята лепая", "Семипунктата Коха", "Популий грыжомельский", "Гортикола филоперьевая"] 
          [BugProp "Наличие бомбурий"         [Just "Да",           Just "Да",             Just "Нет",          Just "Да",               Just "Нет"],   
           BugProp "Количество клептиконов"   [Just "1",            Just "1",              Just "0",            Just "3",                Just "5"],     
           BugProp "Цвет велория"             [Just "Красный",      Just "Оранжевый",      Just "Оранжевый",    Nothing,                 Just "Синий"], 
           BugProp "Наличие пумпеля"          [Just "Нет",          Just "Да",             Just "Да",           Nothing,                 Nothing],      
           BugProp "Величина пумпеля"         [Nothing,             Just "Большой",        Just "Маленький",    Nothing,                 Nothing],      
           BugProp "Возможность крокотания"   [Just "Нет",          Just "Нет",            Nothing,             Just "Да",               Just "Нет"],   
           BugProp "Возможность бульботания"  [Just "Нет",          Just "Да",             Nothing,             Just "Да",               Just "Нет"],   
           BugProp "Наличие дуков и труков"   [Nothing,             Nothing,               Nothing,             Nothing,                 Just "Да"],    
           BugProp "Цвет лемпелей"            [Just "Жёлтый",       Just "Жёлтый",         Just "Жёлтый",       Just "Белый",            Just "Белый"], 
           BugProp "Наличие пильских трапков" [Just "Да",           Just "Да",             Just "Да",           Just "Да",               Just "Да"]]

data Question = Question BugsDef String [String]
              deriving (Show, Eq)

cntOfUniqueVars :: BugProp -> Int
cntOfUniqueVars = length . nub . bugPropVariants

nextStep :: BugsDef -> Either Question (Maybe String)
nextStep         (BugsDef [] _)          = Right Nothing
nextStep         (BugsDef (result:[]) _) = Right $ Just result
nextStep         (BugsDef _ [])          = Right Nothing
nextStep origDef@(BugsDef types props) = if null askableProps
                                         then Right Nothing
                                         else Left $ Question origDef askPropName $ map fromJust askPropVars
    where askableProps = filter noUndefinedVariants props
          noUndefinedVariants = not . (Nothing `elem`) . bugPropVariants
          (BugProp askPropName askPropVars) = maximumBy (comparing cntOfUniqueVars) askableProps

resolveNextDef :: Question -> String -> BugsDef
resolveNextDef (Question (BugsDef bugs props) question variants) response = 
    BugsDef (keepNeeded bugs) $ filter ((1 < ) . cntOfUniqueVars) $ map withOnlyNeededVars props
    where indicesOfNeeded = elemIndices response variants
          withOnlyNeededVars (BugProp name vars) = BugProp name $ keepNeeded vars
          keepNeeded = map snd . filter ((`elem` indicesOfNeeded) . fst) . zip [0..] 

-- user interaction cycle
          
main :: IO ()
main = doResolveStep bugsDef

doResolveStep :: BugsDef -> IO ()
doResolveStep currentDef = do 
    case nextStep currentDef of 
      Left q -> ask q
      (Right (Just name)) -> putStrLn $ "Bug is: " ++ name
      (Right Nothing) -> putStrLn $ "Can't define bug, sorry"

ask :: Question -> IO ()
ask q@(Question _ text rawAnswers) = do 
  let answers = nub rawAnswers
      retry = putStrLn "Wrong response, try again!" >> ask q

  putStrLn $ text ++ "?"
  forM_ (zip [0..] answers) $ \(ind, variant) -> putStrLn $ (show ind) ++ ". " ++ variant
  answer <- getLine

  case (reads answer) of 
    ((answerIndex, _):_) -> if answerIndex < (length answers) && answerIndex >= 0
                            then doResolveStep (resolveNextDef q (answers !! answerIndex))
                            else retry
    _ -> retry
