{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main
where

-- import qualified Data.StringMap                       as Lazy	-- just for dev.
import           Prelude                              hiding (lookup, map, mapM,
                                                       null, succ)

import           Control.Arrow                        (second)
import           Control.DeepSeq                      (($!!))

import           Data.Monoid
import           Data.StringMap.Strict

import           GHC.AssertNF

-- import           System.IO

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test, Testable)
import qualified Test.QuickCheck                      as Q (Property, arbitrary)
import qualified Test.QuickCheck.Monadic              as Q (PropertyM, assert,
                                                            monadicIO, pick,
                                                            run)

newtype Attr = A [Int]
    deriving (Show)

instance Monoid Attr where
    mempty = mkA []
    mappend (A xs) (A ys) = mkA (xs ++ ys)

-- evaluation of x `mappend` y to WHNF leads to NF
-- because of the $!! in mkA
--
-- example
--
--    A [1,2] `mappend` A [3,4]
-- =  { subst of mappend }
--    mkA ([1,2] ++ [3,4])
-- =  { subst of mkA }
--    A $!! ([1,2] ++ [3,4])
-- =  { subst of $!! }
--    A [1,2,3,4]
--
-- in a call of Data.StringMap.insert k (x `mappend` y) m
-- the attribute is forced to be in WHNF, and this leads to NF

type Map = StringMap Attr

-- smart constructor for evaluation into NF
-- before calling the constructor A

mkA :: [Int] -> Attr
mkA xs = A $!! xs

consA :: Int -> Attr -> Attr
consA n a = mkA [n] `mappend` a

default (Int)

main :: IO ()
main = defaultMain
       [
         testCase "isNF" test_isNF
       , testCase "m0" (checkIsNF m0)
       , testCase "m1" (checkIsNF m1)
       , testCase "m2" (checkIsNF m2)
       , testCase "m3" (checkIsNF m3)
       , testCase "m5" (checkIsNF m3)
       , testCase "m6" (checkIsNF m3)
       , testCase "m7 (map test)" (checkIsNF m7)
       , testCase "fromList l4" (checkIsNF $ fromList l4)
       , testCase "m8 (fromList''' ll)" (checkIsNF m8)

       , testCase "adjust m6" (checkIsNF $ adjust (consA 42) "ab" m6)
       , testCase "adjust m1" (checkIsNF $ adjust (consA 42) "xx" m1)
       , testCase "delete m6" (checkIsNF $ delete "ab" m6)
       , testCase "delete m1" (checkIsNF $ delete "xx" m1)

       , testCase "m2 union m3" (checkIsNF $ m2 `union` m3)
       , testCase "m2 unionWith m2" (checkIsNF $ unionWith mappend m2 m2)

         -- these test do not run properly with ghc-7.7-pre and ghc-heap-view-0.5.2
         -- no idea, whether patched ghc-heap-view or QuickCheck is the reason
       , testProperty "prop_simple" prop_simple
       , testProperty "prop_union" prop_union
       , testProperty "prop_diff" prop_diff
       ]

test_isNF :: Assertion
test_isNF = fmap not (isNF [(1::Int)..10]) @? "isNF"

checkIsNF :: Map -> Assertion
checkIsNF !m = isNF m @? ("isNF " ++ show m)

-- some simple test data
m0, m1, m2, m3, m5, m6, m7, m8 :: Map
m0 = insert "" (mkA [0,1+2]) empty
m1 = insert "abc" (mkA [1,2,3]) empty
m2 = insert "x" (mkA [0,1]) empty
m3 = insertWith mappend "abc" (mkA [4,5,6]) m1
m5 = singleton "abc" (mkA [42])
m6 = fromList l4
m7 = map (consA 0) $ fromList l4
m8 = fromList''' ll

fromList' :: [(d, [Int])] -> [(d, Attr)]
fromList' = fmap (second mkA)

fromList'' :: [(a, Int)] -> [(a, Attr)]
fromList'' = fmap (second $ mkA . return)

fromList''' :: [Key] -> StringMap Attr
fromList''' = fromList . fromList'' . flip zip [1..]

l4 :: [(String, Attr)]
l4 = fromList' [("a",[1]),("b",[2]),("c",[3]),("a",[2]),("ab",[22]),("a",[3])]


prop_simple :: Q.Property
prop_simple = Q.monadicIO $ do
                            l <- Q.pick Q.arbitrary
                            passed <- Q.run $ do -- hPutStrLn stderr $ "\n" ++ show l
                                                 -- hPutStrLn stderr $ "\n" ++ show (fromList''' l)
                                                 isNF $! fromList''' l
                            Q.assert passed

prop_union :: Q.Property
prop_union = Q.monadicIO $ do
                            l1 <- Q.pick Q.arbitrary
                            l2 <- Q.pick Q.arbitrary
                            let sm = fromList''' l1 `union` fromList''' l2
                            checkIsNFProp sm


prop_diff :: Q.Property
prop_diff = Q.monadicIO $ do
                            l1 <- Q.pick Q.arbitrary
                            l2 <- Q.pick Q.arbitrary
                            let sm = fromList''' l1 `difference` fromList''' l2
                            checkIsNFProp sm

checkIsNFProp :: a -> Q.PropertyM IO ()
checkIsNFProp sm = do
                            passed <- Q.run $ isNF $! sm
                            Q.run $ assertNF $! sm
                            Q.assert passed

ll :: [Key] -- QuickCheck generated
ll = ["|6s\FS-\184Cc9\SI=!\137^\r5\221w\151cq\140\&9K\173z\RSu\159Y`\143et"
     ,"\ta\DEL>\236^\219~\197q\STX\194q\248Jn-\USk\a B?\v>(d\RS\232\130\247?]\STX_\249i_\216d"
     ,"\DLEIJ}5\153\CAN\ETX'_M*i<\STX\SUBUr\SI\ETX3zT\155"
     ,"\FS\184\223F\204\t\GS\SI\NAK#\v\"&u\175\195\DEL\150hl\142\DLEB&\\d\249:Qq\RS!\162\&2'\DC4\206>\EOTg\250o\GSLD\185H~a|&\SUBU\b\211aZ;\244\ETB\237\203"
     ,""
     ,"J@\192\ESC\SI\ESCK\158\ETB\ETXit\v \SOH]I%Vd5V`cq\214@\128\248w\226\244F#\DC3z$Bz\134=\ENQ\t\SOH\NAK\f\NUL\231@zsk(A\SI\186d\241:\SI\162\219\SYN\189\ESC\161\&4\153tTp\FS\\\FSEH\\x\r"
     ,"p\174~ q\t\SI]\185\GS\DELD3\243r61\221k\244j?-,\"loxb."
     ,"\135k\ETX\t<*\247\212\211U%\236]\186!\USj\243\252)R\146uU\184Fg\ENQ<'\134m\171\t?~w\139\231?\175\151`\aMKY\CAN\141K$N:\154L\DC3*io\201<\175"
     ,""
     ," Z0\DC2?\n2Q\SO\170 d\132|l\151:9\SUB<\239\EOT\188D_\254N5\224\CAN\236"
     ,",\RS\RSz`Fkw`TD`\SI^+'\197\162\196\ENQ\CAN\155k"
     ,"qi\220qz\220\DC1\US\nE\157HHt<UXK\r..JcJ#.\242Q\163\255AyT\214~\NUL,V0\STXa)\155Dy\247\SOHL\250:?\NUL\139\200rx#MIg\CAN\US\r\222\"\a\203:6\NUL\140=\b:'-g\DC3L\219\&0K\178_\180\202\193zH\234fL.B."
     ,";'T53,9r{tS\SOH\a\ENQGE\STX\148\&6n^B\240\SOH\FS\166@pj\r`-\191.`\145-\172a\131\159\161w5Z\234"
     ,"z\RS\CANDY\212?\150*\ACK\v\192\CAN\221\&0\146Kgv\RSV,Jz\234J57y\130\DC4\152\ETB\SIp\DC2\ETX\\b\SOA}\232?*R\DC2\165\218\DC3\ACKx@\130/M9\193fc4\208\221:N\155\DLEv\138"
     ,"q\ENQP\241+}1\DC2\188\191!Q\163#\EOT\219t~%\SYN@\134!%8i&\ESC\DC1\182\255\SUBx \242\v\179\135J\ENQ\GSKH\SI=|u\DEL"
     ,"5>\246\153\&1L\t{u\DEL2\\6oM\EOT\180-\GS\NUL\SUB\DELn\236"
     ,"nA\158B\"\GSr]5\SIgo\224\&8\246!c\165\bZp*<\n\RS\DC4*oY"
     ,"\DC4\233O3Q0N\243\197=O\EM)>\fQ[2bS>x1R\STX\214^\217\177\EOT\243c\224\167y$aC\SO\144?1"
     ,"\188<"
     ,"l\nou?\168{\184\&9zB\139\146.\221\214\160b\ACKL*\222\aUV\US\202\DC2#\SYN \DC4;3\252xa\148?rP=4\170/\EOT\245\ACK\188"
     ,"\221\\\SOHP\DEL9\169\161\&0\195\NAKKA&\193\141\EM\rScr\229&\179@\212\STXM%\143{\NAK\FSU\216\198\235eQ\NAK\NUL:\DLErg^\SOH8<\f\SO\236\SYN$O\ESCv\RSg\tV\DC2\214[W%\221\ETBY\231\192\200\179?\ESC\199|\DC4c\144XG \145#E7\225q|\189\141\&6\255\228"
     ,"\f\239]\DLElD\EMKy\236\171z93!\ACKD~\200UV\EOT}\RSB\181<%{?| >\228\166I<\205\208\"\218@{\DELk"
     ,"\US(s\254\SUB-e\DC4\"_[r$j\241\212\231\243\SI9\CAN9gv\249i\148\157+G\a\ETB"
     ,"z\189`,\224\SOH.\189\149\&8S]}\227ix\169dA.L\163FW\nH\rUV(~Q>_y,p&[{!8\218\&2\168\155qN\EM_o\218J\170x\a2E\ETB\DC1T\222\&1Cl<\181@HS]\ENQ\t\SOi\ESC\DC22\180;%\231|#w\t\234\""
     ,"\144N8\FS\215IX}\238\&0=7\247\&1\f\SYN\240H6\f7%~h\f@g\214\DC4J8A\200\&0\fU}\NUL\132B.=Bd([H\148,D\EM\SOHV ?\226\DC4\184\202o8\160\FS+0"
     ," \ETX~h su-\230\DC4\ETB0\"\DEL+^\DELg\DC4w\SYN9\SOi\133\241\197a5\217(\162\EM\CAN\181\&8W\DELK\GSVItdh2\SO\202\nm\233\182\205h>"
     ,"Ulu1I\tG\165\202\CAN\DLE[Etr\236t\156\DELO\247\151\DLE\226\223rPXhJ\t\242\SOHd\CAN5\214GY\161hzm#\NAKxD/"
     ,">?\DC4Z<b?\143\206D;~\135eWaR\ACKW\r+o"
     ,"\146^gx9Z)iz"
     ,"12\218'1\SUB\ETXbH_\161\a]>*M\NAK?IB\STX\USd\251?.\242c3\r\141"
     ,"\EOT\200,\USq\RSZ\141\NAK~\146\SYNR\GS?\STXS"
     ,"\SUBw$,un'[4Ev\139\ACK*\150c\a\210 \242\&4\bC,i\DEL\197\197E"
     ,"Jo\SI\ACK!BK\242{*\NAK\191\155\223xt\bJPt\FS\DEL \157\202yXhu@\244T\bG#\245\DLE\248T\DC1%q\141+{\142\DLE\199\&3Y\149\174\178H\138\187"
     ,"B|i\\of\234\228`IIT\242\192\tu\222\STX{T\147\\n(=aWuO\191;\f0R\213\CANOOL\241\n\ESC"
     ,",3?\170=(1H\DEL}\180+1\209\nn\142\ETX9vp\204qxP\ndON*s\t;\EOTU\180&\NAK\173,4AjW9TLEZk\208T\130\ESCa\NUL]vvQD%;\SUBs#\210D\158Ie\164\EM=x\DC1\236"
     ,"\ACK,\rN\219o?\189\&356"
     ,"\DELWZm#TW\ESC\235\234J\RSfK3\167\SUB\133\&7 A\253T\172\\k=\NAK#<&\240mCk'\162\&0^%\DLEm's\EM(rV\235\n~\168\&6\224\DC3\CAN\195+\DC1i\225\191\nU\207&#?\226\&1RY"
     ,"*Hy4\as6\ACK K\168\US<\166\145\135<?\254\129\DC1\143S96q(\240\203HWF,\SUB\a\a\aZ\225\&1\ACK\216\131~\f\220&\DLEW\245fl\r\238P\SYN\250\178\"\246oz.\r\SUB\171\ETBd{9[\vv~<S\DC4rT\182$\DC4h6w\ETBXdx\247<w\ETX"
     ,"xg@BI'\160^\SOH\\\NULul\EOT\FS\DELK/\bQ\163\142`\DC3\181\219\223\244\&3#W\176\ETX`\231B1*:'\206\t\228q?:\nS\194\221T\237>xN\170\129D/-TH\230\STX  =\167-Bk-\134n[\201\nsX6wl948\DLEC\240"
     ,"\150.%Z};U?\146]\213%\ETBe\DC4[<\DLEY\185L?\221\DC2\USi\228\NUL2y\ETB]I\150"
     ,"^\DC2\231\&9h\DC36P\DC3V0\208x\ACK\240)\198`A\ENQR\EM9ZW\DLE\239\&1\b8D\"\DEL{u<\r,"
     ,"B1\ESC#GRj\209\f\ACK1\177\SUB\227\190\180\ETBrj\DC2-B$\n\219\249!1$\SI\t\ETXd%\FSr\ETXt)N \186\v<\EM! \DC4p0CYZ\DC2\200\207V\194\"n{\208"
     ,"\EOTf\134X(\ENQ\201'\188\228<S\169\&0\158\167\255\DC4\SUBm3\t[\143\231*!q\RSUj=\SOu\209>K\242\nA0nE\255\EOT\td\235\253\151A\145a#T\ACK\145"
     ,"8Pe\185\\\135\194\130=o3"
     ,"#\210\FSJ:\221r\ETX%gmS$\132\SOH8X|s3+>k"
     ,"\165\tzL3\141\GSc\GSC]]\245lME2{;@R'\GSLXASD\DEL\SUB\158C`?d\SI\157V1\r\174#\ENQ\SYN\220\DEL}\SOHpTn_6\"\199\GS?o\201\182\186\&5H~a}\aN\194C;f(s \f31i,"
     ,"9D.Dd\SO[\133y\DC1\190.]\SYN0C\177\194H\232;\251\SYN\140\133\ACK3\DEL(j*9(\DC3\239XPWlO\EMh&\151?f\209\&9!e\250l\241E\DEL\DC2>\235\146\206e>W \155m\166D7"
     ,"\ETB\156\167\245e\153|\190Gq\SO\SYN)#\n\USy\GS"
     ,"n\149"
     ,"\196L\130^DJ\US\DLE\155wBo\DLE|\158\SYN.-W%,\DC2O^f\238g\EOTX4"
     ,"\US\205j\DEL\160;*`- OI\SI\230\171\ESC9^-e&-/\195\143qszB2GS-\""
     ,"\160Xb2\DC1\197zH\214\ETX*HWt\176'\NAK\180l<\158kj\237_\202jB\RS\147\186\ACK\v\\p\244~\149qu^\EMK\201\152A\229-Uj=\219\SUB\DC2,RU\SI\SIm\194T1\205"
     ,"\bO>\DELr\NAKc\DC49\NUL\169\144dx\187Xk\RSC\SOH\DELI\DC3=k\GST@\DEL\134mdE\US0\134\236l~\DEL\\{\210ZNxJP\r5\EMPb+W\ENQ\b0I\128\EOTJIy\232\159\233"
     ,"\DEL\t/-B~\f\161\182\ACKp\157\f\174mX*\203\t8\185"
     ,"\\y\SI}TJ<|<\241(Et_\237\CAN\SOH\DC1\FS\FS0Bo\191)9\145\148\165\ETB\ACKasr\135(\209\132]\181x\DC3o\SYNs4%}]"
     ,"\204\218`f}\173\SYN{\167\254OPs5,oGq$F\179xf\135'D\EOT\200\166QHL[\224\bY\219\130q\EM\t"
     ,"-*r\SI\ETX\a\242\194\&4\ETX8,Iu\ETX4}\172N\SYN\fD\208\133\137\r\183&\224\DC4\179\222\201\&7\185\150\217\ETBxM\NAK-\225\252ya\246xx\202(kh\144!\175\158\239=/&D~'\185\196Q\207\&9Q\135\CAN`v)Lj\212\nK{\DC4\\9%.t"
     ,"}=q\FS^\176\\O\\\SO\f\130\bLIkb\DEL\RS\"5\f+f|j\NAK`\162\DELE\162\160\197v7n\158\t\218\205\DC2b4D`\200\GS:\US1\251`\201\180\242\190^h:\158\ETB\NUL\248\184Z\203\187E\DC1\248\237~\254U\USlSIcw`<bO\n\211\136\247\180\DLE:\135$"
     ,"\SI_!\EM\228xjwD@\fA_\DC2\DELO\f%*\CAN \n\rY/\193O\"\rg\tz\174"
     ,"\SOl\251uN\ENQ\DC1-v@\204\152%Hdy\\\177 `{H\128\f\r&\180\rvU\225\135\184M,}\134;M\169+m2\133?\SOH\176\225K1\219,\tR\202\221X\DC4Hb\201\234O\r\207\FS\194\ACK"
     ,">F\129_[-z^\EM\DELy^[-\SOIh\167h\222\134^3jGh\US\USd,\238\145\182b]\SO\146s\b\205\r\149\&8\250d\EM[j\254R\NAK?\162r\226\tci"
     ,"\EMoBm\ETX8"
     ,"B\f\249\148ljNt\f\FS\USyj&\234K&m\164\&2\172pKg"
     ,"\tg\161\253\GSjkKRRY\163:2\224\&6$\206:e\ETX*0\210\ACK\\*H0\194\139\171c\ACK\135c(3Y\DELS5\241\140Z*\NAK\212\159\DC4=m\184\SYNm_c\tC\202/A\SOH\SUB"
     ,"\ETB\235\tX@)B\DLE\b\141-sF('g\ENQ\193\202C>\236\ESC\186J^[RQ\250\DC2<oa\US\141\224~^txx\156\150\234~O>ag\b\246Ck\ETB6W\228\ETXC\ESCM\150\vKutG\CAN%\154z\220t\NAK\SIa\200\219<\v\201<\GSM"
     ,"\190\247;\209]\128\bGCaS\DEL<\240|rx\245\203}\170W\174(\162\SOBQ\230\222#HJxmhUZS\SUB\DC1#\166\165\139\184\&8]W\NUL4y\CAN\191\EM\b\179%}/'\r\a\ni} \SUB\EM$Z\DC3{it\NAK\138\164]s,@!-4\a0L\139"
     ,"'(^n4F\190+\ETB\a_u\ETXn\a"
     ,"{(\250EO|#\213==U<\STXOP\152\t\141\200+}\173\166\r\186mw\236~\RSg\187\ENQ\212\240\185P\RS;\ao^\250\128.@MHx66\"\195\232h\GSm\ttQ.|\NUL0\157{\244"
     ,"\251h\167\v0ao\165s3\GS;cB-\246CO3\DC3\RS/W\ETB\184\SOH~ \nV\"\223\189I\EM\139u\182C\DLET]\137\233\205`\229t|\v\n\163\&8f\255\GSX\NAK\nl\172"
     ,"\CAN?mAj\138\156\177?\RS\ESC\156Vv\225\&0\ACK\186\ESC\SOH <\190\NAK\239\&2\DLEW\193nF@2%kS\riL\US\246\DLE\249\137CO\DC1!{"
     ,"\DC2[M&\190\&1K\167\170O|\SI\tTE\150qL\tYP\FS_MvC\FSd|\DC1\168\f\SOD"
     ,"-;\136O\228f\USo\ENQZo\194g\SO\b\SOHY\DLE`f\r\ESC\243\&1\b\207\211| \201s\DC1-?R\RSlz Q\160\US\SIt:\ENQp\202&*1\n\205\233p*\NAK\SUBAC}\160\bX\217S\234]\187q\DLE\f\216dT\235\156\DC4\SO7\r\159Z#pgO\SI\FS"
     ,"kv\\"
     ,"HI\130+f$d\206Y\128\NULZ\228;[4\158uxnC\182"
     ,"\FSo[!Qx\198A64;\GSO#\US\f\172\DELv\ETXt\241\CAN)\227\195\DC4M)9\DC3D\150\182\185{\172Fby#Y3\"\DLE\133\DLE\241\140@4\251\EOT%\252al\SYN\RSmW`\172\207\\H?\144jx\STX\196Q.iy\SYN\n5S\142\&4H<\199\&3Wd\172"
     ,"\159%\EOT\f,\132\185\244jn}\b7\145 <\182~\v\138\ENQ}OA\a\RS<SH\222U\EMIE\184\n\GS"
     ]
