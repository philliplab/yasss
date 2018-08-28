# Just starting - very very early version

# Yet Another Short Sequence Simulator

The motivation for writing this package is:
- We need to simulate the unusual conditions that VRC01 might generate in the AMP study
- We need strange phylogenies (only a few generations, but a very high offspring rate)
- We want to have really fine control over the recombination
- We possibly want to simulate the Primer ID process also
- We want to simulate varying levels of immune pressure based on epitopes in the sequences
- In practice, we constantly rewrite little bits of this package taylored to whatever we are currently working on

## Basic flow of the simulation (Mostly aspirational, will be still be useful if only two or three of these goals are achieved)

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

## Mutators

Find a better place to store this documentation.

To simulate the quasispecies, a function is needed that will take a parent sequence as input and produce a child sequence by possibly producing some mutations in the sequence.

The constructs yasss uses to produce single children from a parent are called mutators, introduced as an S3 class into yasss.

It simply is a list with three elements: fun, args and arg_checker.

The fun element is a function with the argument the_seq and an arbitrary number of other arguments.

The args element is a list with the actual arguments that fun will use.

The arg_checker performs basic sanity checks on the arguments and is seperated from the actual function itself so that error checking and debigging can be simplified by checking the construction of the mutator at the start of the program without generating a very deep trace. (Since the calls will happen through mechanisms like do.call(mutator$fun, args) the trace will be hard to read also.

A constructor lives in mutator_constructor.

Each mutator has its own script and its own test file.

Presently no mutator will be allowed to produce indels. Hopefully this restriction can be lifted in the future.

At the moment six posible mutators are envisioned:
1) uniform_mutator
2) pp_uniform_mutator
3) nucleotide_based_mutator
4) pp_nucleotide_based_mutator
5) codon_based_mutator
6) pp_codon_based_mutator

pp stands for per position and allows the user to specify variable mutation rates for different positions.

Obviously each mutator is just a special case of the mutator that comes after it. As such, the ultimate goal should be to write a single general function that produces each of the special cases. However, that requires a lot more engineering that just getting a simple one up and running right now. So I will start with the uniform mutator and implement most of the package with only that mutator and then come back to this to produce the special mutators later. Also, there might be performance issue with the most general versions that will not be an issue at all for the simpler ones. In theory the performance mismatches can be completely nuetralized by moving this operation to C++ since the main performance overhead will be the lookups.

The output of a mutator is a list with the elements: 
1) parent 
2) child 
3) mutation_statistics 
4) mutation_rates

The parent is the input sequence.

The child is the produced offspring.

The mutation_statistics tracks metrics about the mutations that occurred. Initially it will be a list with a single element: n_mut

The mutation_rates element is the matrix of mutation rates is the possibly updates mutation rate array that is used to compute the mutation probabilities. This is included to future proof the code so that the addition of indel functionality will not require mojor changes.

Overall process for developing the mutators:
- develop the uniform mutator together with its test file.
- There will be a whole slew of tests that are not specific to the uniform mutator. Split them out into a general mutator tests script. Use a for loop looping over all the different types of mutators to apply these general tests to each mutator.
- After the uniform mutator is completed, write the mutator_constructor.
