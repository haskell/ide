data Bam = Bam {
    number :: Int,
    s :: String
}

foo :: Bam -> Bam
foo Bam {number = y} = Bam {number = y + 5, s = ""}
