# ProbNetKAT
Written for my MSc Thesis, this repository implements ProbNetKAT, a probabilistic extension of the SDN language NetKAT.
Two options are available: compiling a ProbNetKAT (PNK) program to network simulator Ns3 code, or running interference directly on the program.

# Instructions
Make sure you have ns3 installed if you wish to simulate running PNK programs. For instructions on how to install ns3, please see https://www.nsnam.org/docs/release/3.40/installation/html/index.html

Run 

    stack build

To build the program and required packages.
Alternatively, to both build and run the program, run:

    stack run -- {options}

Where the options are as follows:

    --help               Display the help message.
    -i (file) (input)    Run exact inference on program
    -s (file) (input)    Run sample inference on program
    -c (file)            Compile program to NS-3 C++
    -p (file)            Attempt to parse program
    -t (file)            Attempt to parse program and show the resulting tree
    -d (file)            Generate automata and output to dot file (normalised and regular)

For inference, you can supply an input set of histories as follow:

    stack run -- -i path/file "[[(1,1),(0,0)],[(2,2),(0,0)]]"

Where (1,1) is a packet with 'sw' and 'pt' both set to 1, '[(1,1),(0,0)]' a history, and the entire expression between quotation marks representing a set of histories as a list. Default value is [[(0,0)]].


To use the compiled program, the ns3 folder in this repository should be symlinked into the scratch folder of the ns3 installation folder.

    ln -s ./ns3/ /path/to/ns3install/scratch/pnk

After that, run the simulator.

    ./ns3 run scratch/pnk/pnk-sim.cc


For now, the configuration of the network in the simulation is pnk-sim.cc, you can change n_nodes to change the number of nodes. Packets entering the PNK network can be programmed as well.