# ProbNetKAT
Written for my MSc Thesis, this repository implements ProbNetKAT, a probabilistic extension of the SDN language NetKAT.
Two versions are available: a pure Haskell

# Instructions
Make sure you have ns3 installed.

Run 

    stack build

To build the program and required packages.
Alternatively, to both build and run the program, run:

    stack run -- {options}

Where the options are as follows:

    --help           Display the message.
    -i (file)        Run interference on program
    -c (file)        Compile program to NS-3 C++
    -p (file)        Attempt to parse program"
    -t (file)        Attempt to parse program and show the resulting tree"


To use the compiled program, the ns3 folder in this repository should be symlinked into the scratch folder of the ns3 installation folder. 
