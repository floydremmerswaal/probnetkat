#ifndef COMPILED_PNK_PROGRAM_H
#define COMPILED_PNK_PROGRAM_H
#include "pnk-program.h"
PnkPrgrm getAutomaton() {
	PnkPrgrm ret;
	ret.addNode(0,  KLEENESTART, 0, 0.0);
	ret.addNode(0,  SW, 2.0, 0.0);
	ret.addNode(1,  DUP, 0, 0.0);
	ret.addNode(2,  SW, 1.0, 0.0);
	ret.addNode(3,  DUP, 0, 0.0);
	ret.addNode(4,  SW, 0.0, 0.0);
	ret.addNode(5,  DUP, 0, 0.0);
	ret.addNode(6,  KLEENESTOP, 0, 0.0);
	return ret;
}
#endif