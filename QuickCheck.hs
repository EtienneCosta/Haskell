module QuickCheck where

import Haskell
import Test.QuickCheck


{-- 
   
   Formato disponibilizado para correr testes das funções implementadas.

   Nome:Etienne Costa 
   Email: etienne_costa@hotmail.com
  
  Email disponibilizado para dúvidas e sugestões.

	
	prop_function::Inputs -> Bool 

Executar:
	quickCheck prop_function


--}


prop_length::[a]->Bool
prop_length l = length(l)==length(myReverse(l))

