type Id = Int

data ProofSession = ProofSession Int  

instance Show ProofSession where 
  show (ProofSession id) = show id

data AssistantState = AssistantState 
                          {  proofSessions :: [ProofSession], activeSession :: ProofSession } 
                          deriving Show
type FileName = String
data Commands = Source FileName

class AssistantCommand ac where
  run :: AssistantState -> AssistantState



