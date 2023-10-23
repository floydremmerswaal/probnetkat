#ifndef COMPILED_PNK_PROGRAM_H
#define COMPILED_PNK_PROGRAM_H
#include "pnk-program.h"
PnkPrgrm getAutomaton() {
	PnkPrgrm ret;
	ret.addNode(SKIP, 0.0);
	ret.addNode(SW, 1.0);
	ret.addNode(DUP, 0.0);
	ret.addNode(SW, 2.0);
	ret.addNode(DUP, 0.0);
	ret.addNode(SW, 0.0);
	ret.addNode(DUP, 0.0);
	ret.addEdge(0, 1, 1.0);
	ret.addEdge(1, 2, 1.0);
	ret.addEdge(2, 3, 1.0);
	ret.addEdge(3, 4, 1.0);
	ret.addEdge(4, 5, 1.0);
	ret.addEdge(5, 6, 1.0);
	ret.addEdge(6, 0, 1.0);
	return ret;
}
#endif