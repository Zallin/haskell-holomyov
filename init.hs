module Task1 where

data Genre = Comedy | Thriller | Action

data Quality = Quality Integer Integer

data Movie = Movie Genre Quality

data Review = Review Integer String

rate :: Movie -> Review
rate = undefined
