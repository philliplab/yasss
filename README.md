Yet Another Short Sequence Simulator

The motivation for writing this package is:
- We need to simulate the unusual conditions that VRC01 might generate in the AMP study
- We need strange phylogenies (only a few generations, but a very high offspring rate)
- We want to have really fine control over the recombination
- We possibly want to simulate the Primer ID process also
- We want to simulate varying levels of immune pressure based on epitopes in the sequences
- In practice, we constantly rewrite little bits of this package taylored to whatever we are currently working on

Basic flow of the simulation (Mostly aspirational, will be still be useful if only two or three of these goals are achieved)

Simulate the growth of the quasispecies
- Inputs
-- Ancestors
-- Number of copies of each ancestor
-- (Vector) of generation sizes
-- Required population size or the number of generations
-- (Vector over generations) of recombination profiles
-- (Vector) of per codon mutation matrices (specified on the nucleotide level 64x64)
-- (Vector) of per codon indel generation

Simulate the sampling achieved by reverse transcription
- Inputs
-- primers
-- primer binding affinity function
-- Number of primers to generate

Simulate PCR
- Inputs
-- Number of cycles
-- Per cycle efficiency
-- Max number of sequences to generate
-- (vector) per nucleotide mutation matrix (4x4)
-- PCR recombination profile
-- PCR recombination boost parameter (inclease recombination rate each cycle)

Simulate the sequencing
- Inputs
-- Number of reads sequenced
-- Length of reads
-- (vector) per nucleotide error matrix (4x4)
