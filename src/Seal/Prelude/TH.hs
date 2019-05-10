{-# LANGUAGE TemplateHaskell #-}

module Seal.Prelude.TH (
    module Language.Haskell.TH,
    module Language.Haskell.TH.Syntax,
    module Language.Haskell.TH.Quote,
    module Seal.Prelude.TH,
) where

import Prelude 
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax 
import Language.Haskell.TH.Quote

import Language.Haskell.TH.Lib hiding (Role)
import Debug.Trace
import Control.Monad
import GHC.Generics 
import Data.List
import System.IO.Unsafe

-- | Pretty print spliced code
pprintQ :: Ppr a => Q a -> IO ()
pprintQ q = runQ q >>= putStrLn.pprint

-- | Print AST of the code.
printQ :: Show a => Q a -> IO ()
printQ q = runQ q >>= print

-- | Debug print
dprint :: Show a => String -> Q a -> Q a
dprint str q = do
           a <- q
           trace (str ++ ": " ++ show a) q


-- | sequence-like functons on tuples
seqTup2 :: (Q a, Q b) -> Q (a, b) 
seqTup2 (a,b) = liftM2 (,) a b

seqTup3 :: (Q a, Q b, Q c) -> Q (a, b, c)
seqTup3 (a,b,c) = liftM3 (,,) a b c

seqTup4 :: (Q a, Q b, Q c, Q d) -> Q (a, b, c, d)
seqTup4 (a,b,c,d) = liftM4 (,,,) a b c d

-- | Unsequence @Q [a]@ to [Q a], but you never get rid of the outer 'Q'
unsequence :: Q [a] -> Q [Q a]
unsequence qs = do
           s <- qs
           return $ map return s

-- | Rename a 'Name'
rename :: Q Name -> (String -> String) -> Q Name
rename n f = do
         bn <- n
         let nameStr = f $ nameBase bn
         return $ mkName nameStr

rename' :: Name -> (String -> String) -> Name
rename' n f = mkName $ f $ nameBase n

rename'' :: Name -> (String -> String) -> Q Name
rename'' n f = do
          let nameStr = f $ nameBase n
          return $ mkName nameStr

{-|
> data Foo = Foo { foo :: Int }
> > $(nameToExp (++"1") 'foo)
> "foo1"       
-}
nameToExp :: (String -> String) -- ^ Function to change name. 
             -> Name 
             -> Q Exp 
nameToExp f = litE . stringL . f . nameBase 

-- | Makes a string literal expression from a constructor's name. 
conNameExp :: Con -> Q Exp 
conNameExp = litE . stringL . nameBase . getConName

-- | Apply a list of expression
{-|
> [(+), 1, 2] to (+) 1 2
> appExp' [VarE '(+) , (LitE (IntegerL 1)), (LitE (IntegerL 2))]
> >AppE (AppE (VarE GHC.Num.+) (LitE (IntegerL 1))) (LitE (IntegerL 2))
-}

appExp :: [ExpQ] -> ExpQ
appExp = appsE

appExp' :: [Exp] -> Exp
appExp' = foldl1 AppE

unappExp :: ExpQ -> Q [Exp]
unappExp = fmap unappExp'

unappExp' :: Exp -> [Exp]
unappExp' a@(AppE e1 e2) = reverse $ unfold (not.isAppE)  unappE restUnappE a
unappExp' e = [e]

foo = (AppE (AppE (VarE 'const) (LitE (IntegerL 3))) (ConE (mkName "True")))

isAppE :: Exp -> Bool
isAppE (AppE _ _) = True
isAppE _ = False

unappE :: Exp -> Exp
unappE (AppE e1 e2) = e2
unappE x = x

restUnappE :: Exp -> Exp
restUnappE (AppE e1 e2) = e1
restunappE x = x

unfold :: (t1 -> Bool) -> (t1 -> t1) -> (t1 -> t1) -> t1 -> [t1]
unfold p h t x | p x = [x]
               | otherwise =  h x : unfold p h t (t x)

-- | Apply a type constructor like 'appExp'

{-|
> > pprint $ appConT' (map ConT [''(,), ''Int , ''Bool])
> "GHC.Tuple.(,) GHC.Types.Int GHC.Types.Bool" --i.e. (Int,Bool)
-}

appConT :: [TypeQ] -> TypeQ
appConT = foldl1 appT

-- | Unapply a constructor application to a list of types
unappConT' :: Type -> [Type]
unappConT' a@(AppT t1 t2) = reverse $ unfold (not.isAppT) unappT restUnappT a
unappConT' a@(ForallT tvbs cxt t) = reverse $ unfold (not.isAppT) unappT restUnappT t
unappConT' x = [x]

-- | Unapply a constructor application to a list of types
unappConT :: TypeQ -> Q [Type]
unappConT = fmap unappConT'

--fooT = AppT (AppT (ConT ''Either) (ConT ''Int)) (ConT ''Bool)

isAppT :: Type -> Bool
isAppT (AppT _ _ ) = True
isAppT _ = False

unappT :: Type -> Type
unappT (AppT t1 t2) = t2
unappT x = x

restUnappT :: Type -> Type
restUnappT (AppT t1 t2) = t1
restUnappT x = x

appConT' :: [Type] -> Type
appConT' = foldl1 AppT


-- | convert @[a, b, c]@ to @a -> b -> c@
{-|
> > pprint $ curryType' (map ConT [''Int , ''Int , ''Bool])
> "GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Bool"
-}
curryType :: [TypeQ] -> TypeQ
curryType  = foldr1 (\t1 -> appT (appT arrowT t1))

curryType' :: [Type] -> Type
curryType' = foldr1 (\t1 -> AppT (AppT ArrowT t1))

-- | > convert @a -> b -> c@ to @[a,b,c]
{-|
> > uncurryType' (ForallT [PlainTV (mkName "a")] [] (AppT (AppT ArrowT (VarT (mkName "a"))) (ConT ''Int)))
> > [VarT a,ConT GHC.Types.Int]
-}
uncurryType :: TypeQ -> Q [Type]
uncurryType = fmap uncurryType'

uncurryType' :: Type -> [Type]
uncurryType' t@(AppT (AppT ArrowT t1) t2) = t1 : helper t2
uncurryType' t@(ForallT tyvs cxt ty) = helper ty
uncurryType' x = [x]

helper :: Type -> [Type]
helper t@(AppT (AppT ArrowT t1) t2) = t1 : helper t2
helper t = [t]

{-|
> > genBT' "a" 3
> ([PlainTV a1,PlainTV a2,PlainTV a3],[VarT a1,VarT a2,VarT a3])
-}

-- | Generate a list of type Bind and Type
genBT :: String -> Int -> Q ([TyVarBndr], [TypeQ])
genBT name n = do
           let ns = [name++ (show i) | i <- [1..n]]
           tvb <- sequence $ map (return.plainTV.mkName) ns
           typ <- sequence $ map (return.varT.mkName) ns
           return (tvb,typ)

-- | Generate a list of type Bind and Type without Q
genBT' :: String -> Int -> ([TyVarBndr], [Type])
genBT' name n = let ns = [name++ (show i) | i <- [1..n]] 
                    in (map (plainTV.mkName) ns, map (VarT . mkName) ns)


{-|
> > genPE' "a" 3
> ([VarP a1,VarP a2,VarP a3],[VarE a1,VarE a2,VarE a3])
-}
-- | Generate related patterns and expressions
genPE :: String -> Int -> Q ([PatQ],[ExpQ])
genPE name n = do 
           let ns = [name++ (show i) | i <- [1..n]]
           pat <- sequence $ map (return.varP.mkName) ns
           exp <- sequence $ map (return.varE.mkName) ns
           return (pat,exp)

-- | Generate related patterns and expressions without Q
genPE' :: String -> Int -> ([Pat], [Exp])
genPE' name n = let ns = [name++ (show i) | i <- [1..n]] 
                 in (map (VarP . mkName) ns,map (VarE . mkName) ns)

-- | Apply a list of kinds, like 'appConT'
appKind :: [Kind] -> Kind
appKind = foldr1 AppT

-- | Like 'unappConT' 
unappKind :: Kind -> [Kind]
unappKind  = unappConT'

-- | Like 'curryType' but on kind level
curryKind :: [Kind] -> Kind
curryKind =  curryType'

-- | Like 'uncurryType'
uncurryKind :: Kind -> [Kind]
uncurryKind = uncurryType'

-- | Get name from constructors
getConName :: Con -> Name 
getConName (NormalC name _)  = name 
getConName (RecC name _)     = name 
getConName (InfixC _ name _) = name 
getConName (ForallC _ _ con) = getConName con 

-- | Get type Names recursively
getTypeNames :: Type -> [Name]
getTypeNames (ForallT tvbs cxt t) = getTypeNames t
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

-- | Get type var bind name
getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV  name  ) = name
getTVBName (KindedTV name _) = name

third (a,b,c) = c

-- | Get all names recursively from a constructor
getCompositeType :: Con -> [Name]
getCompositeType (NormalC n sts)        = concatMap getTypeNames (map snd sts)
getCompositeType (RecC    n vars)       = concatMap getTypeNames (map third vars)
getCompositeType (InfixC st1 n st2)     = concatMap getTypeNames [snd st1 , snd st2]
-- This could be a problem since it will lose info for context and type variables 
getCompositeType (ForallC tvbs cxt con) = getCompositeType con

-- 代码片段
data Snippet 
  = Overwrite [Dec]
  | Merge [Dec]

overwrite :: DecsQ -> Q Snippet
overwrite oldQ = Overwrite <$> oldQ

merge :: DecsQ -> Q Snippet
merge q = Merge <$> q


infixl 1 .<
(.<) :: DecsQ -> Q Snippet -> DecsQ
oldQ .< snippetQ = do
  old <- oldQ
  snippet <- snippetQ
  case snippet of
      Overwrite news -> return $ old <> news
      Merge news -> return $ old <> news


instance Semigroup DecsQ where
  q1 <> q2 = (++) <$> q1 <*> q2