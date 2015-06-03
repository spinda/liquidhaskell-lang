{-# LANGUAGE ScopedTypeVariables #-}

module Util (buildExpressionParser) where

import Control.Applicative

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Expression hiding (buildExpressionParser)

buildExpressionParser :: forall m a. (Parsing m, Applicative m)
                      => OperatorTable m a
                      -> m a
                      -> m a
buildExpressionParser operators simpleExpr
    = foldl makeParser simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc,prefix,postfix) = foldr splitOp ([],[],[],[],[]) ops
              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix  <?> ""
              postfixOp  = choice postfix <?> ""

              termP      = (prefixP <*> term) <**> postfixP

              postfixP   = postfixOp <|> pure id

              prefixP    = prefixOp <|> pure id

              rassocP, rassocP1, lassocP, lassocP1, nassocP :: m (a -> a)

              rassocP  = flip <$> rassocOp <*> (termP <**> rassocP1)

              rassocP1 = rassocP <|> pure id

              lassocP  = (flip <$> lassocOp <*> termP) <**> ((.) <$> lassocP1)

              lassocP1 = lassocP <|> pure id

              nassocP = (flip <$> nassocOp <*> termP)
                        <**> pure id
           in termP <**> (rassocP <|> lassocP <|> nassocP <|> pure id)


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)

