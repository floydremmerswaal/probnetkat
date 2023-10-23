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

struct PnkEdge
{
    PnkEdge();
    ~PnkEdge();
    uint32_t to;
    double weight;
};

PnkEdge::PnkEdge()
{
    to = 0;
    weight = 0;
    return;
}

PnkEdge::~PnkEdge()
{
    return;
}

struct PnkNode
{
    PnkNode();
    ~PnkNode();

    uint32_t nodenr;
    uint32_t arg;
    uint32_t instr;
    std::vector<PnkEdge> edges;
};

PnkNode::PnkNode()
{
    nodenr = 0;
    arg = 0;
    instr = 0;
    return;
}

PnkNode::~PnkNode()
{
    return;
}

class PnkPrgrm
{
  public:
    PnkPrgrm();
    ~PnkPrgrm();
    int addNode(uint32_t instr, uint32_t arg);
    int addEdge(int from, int to, double weight);
    PnkNode* getNode(uint32_t nodenr);

  private:
    PnkNode* start;
    std::map<uint32_t, PnkNode*> nodeNrToNode;
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

PnkNode*
PnkPrgrm::getNode(uint32_t nodenr)
{
    if (nodeNrToNode.count(nodenr) > 0)
        return nodeNrToNode[nodenr];
    return nullptr;
}

// create a link from node from to node to
int
PnkPrgrm::addEdge(int from, int to, double weight = 1.0f)
{
    PnkNode* fromnode = getNode(from);

    if (fromnode == nullptr || getNode(to) == nullptr)
    {
        return -1;
    }

    fromnode->edges.push_back(PnkEdge());
    fromnode->edges.back().to = to;
    fromnode->edges.back().weight = weight;

    return 0;
}

int
PnkPrgrm::addNode(uint32_t instr, uint32_t arg = 0)
{
    PnkNode* newnode = new PnkNode();
    newnode->instr = instr;
    newnode->arg = arg;
    newnode->nodenr = nodeCount;
    nodeNrToNode[nodeCount] = newnode;
    nodeCount++;
    return newnode->nodenr;
}

#include "compiled-pnk-program.h"

#endif