module Fix where

import Prelude hiding (map, foldr, foldl, zip, repeat, cycle, iterate)

fix f = f (fix f)

map :: (a -> b) -> [a] -> [b]
map op l = (fix f) l
  where f ex l = case l of
                   [] -> []
                   (v:vs) -> (op v):(ex vs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op s l = (fix f) l
  where f ex l = case l of
                   [] -> s
                   (v:vs) -> op v (ex vs)

foldl :: (a -> b -> b) -> b -> [a] -> b
foldl op s l = (fix f) s l
  where f ex acc l = case l of
                       [] -> acc
                       (v:vs) -> ex (op v acc) vs
