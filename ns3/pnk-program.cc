struct PnkPrgrm {
    PnkPrgrmNode* start;
    std::map<uint32_t, PnkPrgrmNode*> nodeNrToNode;
}

struct PnkPrgrmNode {
    uint32_t instr;
    uint32_t arg;
    PnkPrgrmNode* next1; // no branching: next1
    PnkPrgrmNode* next2; // used for branching
    PnkPrgrmNode* prev;
};