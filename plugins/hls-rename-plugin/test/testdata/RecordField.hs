data Bam = Bam {
    n :: Int,
    s :: String
}

foo :: Bam -> Bam
foo Bam {n = y} = Bam {n = y + 5, s = ""}
