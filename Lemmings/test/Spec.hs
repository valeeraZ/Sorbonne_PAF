import Test.Hspec
import Niveau
import NiveauSpec as NS
import QuickCheckNiveau as QCN
import QuickCheckEntite as QCE
import EnvSpec as ES
import EtatSpec as ETS

main :: IO ()
main = hspec $ do
    NS.engineSpec
    QCN.niveauSpec
    ES.engineSpec
    QCE.entiteSpec
    ETS.engineSpec