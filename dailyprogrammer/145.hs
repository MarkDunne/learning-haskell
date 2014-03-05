module Main where

main = do
    width <- getLine >>= return . read
    trunk <- getLine
    leaf <- getLine
    let treeBuilder level = (trunk ++ level ++ trunk)
        padLevel level = replicate (width - length level) ' ' ++ level
        tree = take width $ iterate treeBuilder trunk
        paddedTree = map padLevel tree
    mapM print paddedTree
    