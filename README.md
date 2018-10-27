# Some basic functionality working

# Yet Another Short Sequence Simulator

The motivation for writing this package is:
- We need to simulate the unusual conditions that VRC01 might generate in the AMP study
- We need strange phylogenies (only a few generations, but a very high offspring rate)
- We want to have really fine control over the recombination
- We possibly want to simulate the Primer ID process also
- We want to simulate varying levels of immune pressure based on epitopes in the sequences
- In practice, we constantly rewrite little bits of this package taylored to whatever we are currently working on

## Getting started

```
library(yasss)
example(sim_pop)
```

Also consider the `sim_proc_many_pops` function as a main function of `yasss`.

The design vignette contains some more details about how the package is put
together and some thoughts about the current problems I am working on.
