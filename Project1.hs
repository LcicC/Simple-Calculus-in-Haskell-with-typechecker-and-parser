import My_Parser

data Exp = Tru
 |Fls
 |Zero
 |Error
 |Succ Exp
 |Pred Exp
 |If Exp Exp Exp
 |IsZero Exp
 |TryWith Exp Exp


data Type = Bool |Nat |Err

term :: Parser Exp
term = 
  do { symbol "true"; return Tru}
  +++ do { symbol "false"; return Fls}
  +++ do { symbol "error"; return Error}
  +++ do { symbol "if"; t <- term; symbol "then"; t1 <- term; symbol "else"; t2 <- term; return (If t t1 t2) }
  +++ do { symbol "0"; return Zero}
  +++ do { symbol "succ"; t <- term; return (Succ t)}
  +++ do { symbol "pred"; t <- term; return (Pred t)}
  +++ do { symbol "iszero"; t <- term; return (IsZero t)}
  +++ do { symbol "try"; t1 <- term; symbol "with"; t2 <- term; return (TryWith t1 t2)}


instance Show Exp where
    show Tru = "true"
    show Fls = "false"
    show (If t t1 t2) = "if "++show t++" then "++show t1++" else "++show t2
    show Zero = "0"
    show (Succ t) = "succ "++show t
    show (Pred t) = "pred "++show t
    show (IsZero t) = "iszero "++show t
    show Error = "Error"
    show (TryWith t1 t2) = "try "++show t1++" with "++show t2

instance Show Type where
 show Bool = "Bool"
 show Nat = "Nat"
 show Err = "Err"

isNum :: Exp -> Bool
isNum Zero     = True
isNum (Succ n) = isNum n
isNum _        = False

{-
isVal :: Exp -> Bool
isVal Tru = True
isVal Fls = True
isVal t   = isNum t
-}

reduce :: Exp -> Maybe Exp
reduce Tru = Nothing
reduce Fls = Nothing
reduce (If Error _ _) = Just Error
reduce (If Tru t1 _) = Just t1
reduce (If Fls _ t2) = Just t2
reduce (If t t1 t2)  = case reduce t of
  Just t' -> Just (If t' t1 t2)
  _       -> Nothing
reduce Zero = Nothing
reduce (Succ Error) = Just Error
reduce (Succ t) = case reduce t of
  Just t' -> Just (Succ t')
  _       -> Nothing
reduce (Pred Error) = Just Error
reduce (Pred Zero)               = Just Error
reduce (Pred (Succ n)) | isNum n = Just n

reduce (Pred t)                  = case reduce t of
  Just t' -> Just (Pred t')
  _       -> Nothing
reduce (IsZero Error) = Just Error
reduce (IsZero Zero)               = Just Tru
reduce (IsZero (Succ t)) | isNum t = Just Fls

reduce (IsZero t)                  = case reduce t of
  Just t' -> Just (IsZero t')
  _       -> Nothing

reduce Error = Nothing
reduce (TryWith Error t) = Just t

reduce (TryWith Tru _) = Just Tru
reduce (TryWith Fls _) = Just Fls
reduce (TryWith Zero _) = Just Zero
reduce (TryWith t _) | isNum t = Just t 
--isVal can be used

reduce (TryWith t1 t2) = case reduce t1 of
  Just t' -> Just (TryWith t' t2)
  _ -> Nothing
--reduce _ = Nothing


reduceStar :: Exp -> IO ()
reduceStar t = 
  case reduce t of
      Just t' -> do { putStrLn (show t'); reduceStar t'}
      _       -> return ()


--Error Type can be considered <= Bool/Nat
isValid :: Maybe Type -> Maybe Type -> Bool
isValid (Just Nat) (Just Nat) = True
isValid (Just Bool) (Just Bool) = True
isValid (Just Err) (Just Err) = True
isValid (Just Err) (Just Bool) = True
isValid (Just Err) (Just Nat) = True
isValid (Just Bool) (Just Err) = True
isValid (Just Nat) (Just Err) = True
isValid _ _ = False


typeOf :: Exp -> Maybe Type
typeOf Tru = Just Bool
typeOf Fls = Just Bool
typeOf Zero = Just Nat
typeOf Error = Just Err

typeOf (Succ t) = case typeOf t of
  Just Nat -> Just Nat
  Just Err -> Just Err
  _ -> Nothing

typeOf (Pred Zero) = Just Err
typeOf (Pred t) = case typeOf t of
  Just Nat -> Just Nat
  Just Err -> Just Err
  _ -> Nothing

typeOf (If t t1 t2) = case typeOf t of
  Just Bool -> if (isValid (typeOf t1) (typeOf t2)) then (typeOf t1) else Nothing
  Just Err -> if (isValid (typeOf t1) (typeOf t2)) then Just Err else Nothing
  _ -> Nothing

typeOf (IsZero t) = case typeOf t of
  Just Nat -> Just Bool
  Just Err -> Just Err
  _ -> Nothing

typeOf (TryWith t1 t2) = if isValid (typeOf t1) (typeOf t2) then typeOf t1 else Nothing

typeCheck :: Exp -> Bool
typeCheck t = case typeOf t of
  Nothing -> False
  _ -> True

interpreter :: IO ()
interpreter = do
  putStrLn "Insert Term"
  inp <- getLine
  let [(t,out)] = parse term inp
  if (typeCheck t) 
   then do
    putStrLn "Type OK: Evaluating..."
    reduceStar t
  else 
    putStrLn "Typechecking Error"


--Functions that return True iff the typechecking is correct
--for all expressions in the input list
typeTestT :: [Exp] -> Bool -> Bool
typeTestT [] acc = acc
typeTestT (x:xs) acc = typeTestT xs ((typeCheck x) && acc)

typeTestF :: [Exp] -> Bool -> Bool
typeTestF [] acc = acc
typeTestF (x:xs) acc = typeTestF xs (not(typeCheck x) && acc)
