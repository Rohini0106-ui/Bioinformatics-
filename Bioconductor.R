#Basics Of Bioconductor 

install.packages("BiocManager")
BiocManager::install()

#g install a bioconductor package eg genomiccrange
BiocManager::install("GenomicRanges")
BiocManager::install("Biostrings")

library(Biostrings)
dna <-DNAString("ATGCATGCAA")
reverse(dna)

complement(dna)

reverseComplement(dna)

letterFrequency(dna,'GC')

library(Biostrings)

#create dna seq usinf dnastrings
dna_seq <- DNAString("ATGCATGCAATTGGCCAT")

#transcribe using rstring 
rna_seq <- RNAString(dna_seq)
print(rna_seq)

#translate 
protein_seq <- translate(rna_seq)
print(protein_seq)

# calculate the gc content 
letter_freq <- letterFrequency(dna_seq, letters = c("G", "C"))
letter_freq

#calculate the gc content as the percentage of g and c bases in seq
gc_count <- sum(letter_freq) / length(dna_seq) * 100
gc_count # to print the content


#how to find motif in dna seq
motif <- DNAString("CGT")

motif_loc <- matchPattern(motif, dna_seq)

motif_loc

