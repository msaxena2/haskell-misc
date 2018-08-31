type Name = String 

data Pattern =    Variable Name 
              |   Exists Pattern Pattern
              |   Forall Pattern Pattern
              |   Equals Pattern Pattern
              |   Plus Pattern Pattern
              |   Succ Pattern
              |   Zero  deriving (Show)


equals :: Pattern -> Pattern -> Pattern

equals a b = Equals a b 

type Id = Int

data Rule = MyRule deriving Show

data Claim =   Line Id Pattern
             | ComplexLine Id Pattern Rule deriving Show


data Proof = Proof [Claim] 
              


instance Show Proof where
  show (Proof claims)  = do
                          x <- claims
                          show x ++ ['\n']


varT :: Pattern
varT = Variable "t"

varY :: Pattern
varY = Variable "y"

varX :: Pattern
varX = Variable "x"

functionalZero :: Claim
functionalZero = Line 1 $ Exists varT (Zero `equals` varT)

functionalSuc :: Claim
functionalSuc = Line 2 $ Forall (varX) (Exists varY ( Succ (varX `equals` varY)))



axioms = [ functionalZero
 , functionalSuc ] 
--  , plus
--  , plusZero
--  , plusSuc ]

onePlusOneProof :: Proof 

onePlusOneProof = Proof axioms






