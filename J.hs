module J where
import Data.List

f (x:[])= x:[]
f (s:s1:ss) = if not(head(s1)==','||head(s1)=='.')&&((last(s)==',')||(last(s)=='.')) then s:f((k (last(s)) s1):ss) else s:f(s1:ss)

k c l = c:l

g l = foldr (\x acc -> if(head(x)==',') then 1:acc else 0:acc) [] l

g' l = foldr (\x acc -> if(last(x)==',') then 1:acc else 0:acc) [] l

findHeadComma cl il = zipWith (\c i -> if (i==1) then c else [])cl il

findLastComma cl il = zipWith (\c i -> if (i==1) then c else [])cl il

trimm l = foldr (\x acc->if not(null x) then x:acc else acc)[] l

dhc l = map (\x->drop 1 x) l

dlc l = map (\x->init x) l

delLast [] = []
delLast l = map (\x->if (last(x)==','||last(x)=='.') then (init x) else x) l

delHead [] = []
delHead l = map (\x->if (head(x)==','||head(x)=='.') then drop 1 x else x) l

addHeadComma l [] = [l]
addHeadComma l c = map(\ci->map(\li->(if isPrefixOf ci li then ',':li else li)) l) c

addLastComma l [] = [l]
addLastComma l c = map(\ci->map(\li->(if isSuffixOf ci li then li++"," else li)) l) c

compareAc a c = if length(a)>length(c) then a else if length(a)<length(c) then c else if a==c then a else if a>c then head(c):a else head(a):c 

zipComma re= foldr1 (\xs acc->zipWith compareAc xs acc) re


ff l=zipComma$addHeadComma (f(words l)) $delLast$dhc$trimm$findHeadComma (f(words l)) (g(f(words l)))

ff' l=zipComma$addLastComma (f(words l)) $delHead$dlc$trimm$findLastComma (f(words l)) (g'(f(words l)))

sumComma l li = zipComma [l, li]

delHeadComma (s:ss) = if head(s)==','||head(s)=='.' then (drop 1 s):ss else s:ss


mf l = if (f (words l))==(delHeadComma(sumComma (ff l) (ff' l))) then (f (words l)) else mf (unwords$delHeadComma(sumComma (ff l) (ff' l)))

result1 l = unwords(map (\x->if head(x)==','||head(x)=='.' then drop 1 x else x) l)