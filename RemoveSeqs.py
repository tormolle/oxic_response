#!usr/bin/python

import pandas as pd
from Bio import SeqIO # Import Biopython

def remove_singletons(file):
    # Function allowing me to remove singletons from a fasta file, by reading the "size=" in the title.
    with open(file, "r+") as fastafile, open("all.otus.nosingleton.fasta", "w") as output:
        for record in SeqIO.parse(fastafile, "fasta"):
            if (record.description.split("=")[1]) != "1":
                SeqIO.write(record, output, "fasta")
    return

def match_OTUtable(FastaFile, OTUTab):
    with open(FastaFile, "r+") as fastafile:
        listOTU = []
        for record in SeqIO.parse(fastafile, "fasta"):
            listOTU.append(record.id.split(";")[0])
    with open(OTUTab, "r+") as OTUtab, open("all.otutab.nonchimeras.csv", "w") as output:
        data = pd.read_csv(OTUtab, sep="\t")
        data = data.loc[data['#OTU ID'].isin(listOTU)]
        data.to_csv(output, sep = "\t")
    return

# remove_singletons("all.otus.fasta")
match_OTUtable("all.otus.nonchimeras.fasta", "all.otutab.txt")
