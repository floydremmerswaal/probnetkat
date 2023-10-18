#ifndef PNK_PROGRAM_H
#define PNK_PROGRAM_H

#include "pnk-client-server-helper.h"

const int SW = 0;
const int PT = 1;
const int TESTSW = 2;
const int TESTPT = 3;
const int DUP = 4;
const int DROP = 5;
const int SKIP = 6;
const int PROB = 7;
const int PAR = 8;
const int KLEENESTART = 9;
const int KLEENESTOP = 10;

class NewPnkPrgrmNode
{
    // we have a number of edges
    NewPnkPrgrmNode();
    ~NewPnkPrgrmNode();
    int nodenr;
    std::vector<NewPnkPrgrmEdge> edges;

}

class NewPnkPrgrmEdge

{
  public:
    NewPnkPrgrmEdge();
    ~NewPnkPrgrmEdge();
    int from;
    int to;
    int weight;
    int edgeNr;
};

class PnkPrgrmNode
{
  public:
    PnkPrgrmNode();
    ~PnkPrgrmNode();
    uint32_t instr;
    uint32_t arg;
    double farg;
    std::vector<PnkPrgrmNode*> next;
    std::vector<PnkPrgrmNode*> prev;
    int nodenr; // so the node knows its own number
};

PnkPrgrmNode::PnkPrgrmNode()
{
    instr = 0;
    arg = 0;
    farg = 0.0f;
    nodenr = -1;
}

PnkPrgrmNode::~PnkPrgrmNode()
{
    return;
}

class PnkPrgrm
{
  public:
    PnkPrgrm();
    ~PnkPrgrm();
    int addNode(int parentnodenr, uint32_t instr, uint32_t arg, double farg);
    int addRawNode(uint32_t instr, uint32_t arg, double farg);
    int addLink(int from, int to);
    PnkPrgrmNode* getNode(uint32_t nodenr);

  private:
    PnkPrgrmNode* start;
    std::map<uint32_t, PnkPrgrmNode*> nodeNrToNode;
    uint32_t nodeCount;
};

PnkPrgrm::PnkPrgrm()
{
    nodeCount = 0;
    return;
}

PnkPrgrm::~PnkPrgrm()
{
    for (uint32_t i = 0; i < nodeCount; i++)
    {
        delete nodeNrToNode[i];
    }
    return;
}

PnkPrgrmNode*
PnkPrgrm::getNode(uint32_t nodenr)
{
    if (nodeNrToNode.count(nodenr) > 0)
        return nodeNrToNode[nodenr];
    return nullptr;
}

// create a link from node from to node to
int
PnkPrgrm::addLink(int from, int to)
{
    PnkPrgrmNode* fromnode = getNode(from);
    PnkPrgrmNode* tonode = getNode(to);

    if (fromnode == nullptr || tonode == nullptr)
    {
        return -1;
    }
    fromnode->next.push_back(tonode);
    tonode->prev.push_back(fromnode);

    return 0;
}

int
PnkPrgrm::addRawNode(uint32_t instr, uint32_t arg = 0, double farg = 0.0f)
{
    PnkPrgrmNode* newnode = new PnkPrgrmNode();
    newnode->instr = instr;
    newnode->arg = arg;
    newnode->farg = farg;
    newnode->nodenr = nodeCount;
    nodeNrToNode[nodeCount] = newnode;
    nodeCount++;
    return newnode->nodenr;
}

int
PnkPrgrm::addNode(int parentnodenr, uint32_t instr, uint32_t arg, double farg)
{
    PnkPrgrmNode* newnode = new PnkPrgrmNode();
    newnode->instr = instr;
    newnode->arg = arg;
    newnode->farg = farg;

    if (nodeCount == 0)
    {
        newnode->nodenr = 0;
        start = newnode;
        nodeNrToNode[0] = start;
    }
    else
    {
        PnkPrgrmNode* node = nodeNrToNode[parentnodenr];
        node->next.push_back(newnode);

        newnode->prev.push_back(node);
        newnode->nodenr = nodeCount;
        nodeNrToNode[nodeCount] = newnode;
    }
    nodeCount++;
    return newnode->nodenr;
}

#include "compiled-pnk-program.h"

#endif