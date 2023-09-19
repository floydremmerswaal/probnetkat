/*
 *  Copyright (c) 2007,2008,2009 INRIA, UDCAST
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * Author: Amine Ismail <amine.ismail@sophia.inria.fr>
 *                      <amine.ismail@udcast.com>
 */

#include "pnk-server.h"

#include "ns3/packet-loss-counter.h"
#include "ns3/seq-ts-header.h"

#include "ns3/inet-socket-address.h"
#include "ns3/inet6-socket-address.h"
#include "ns3/ipv4-address.h"
#include "ns3/log.h"
#include "ns3/nstime.h"
#include "ns3/packet.h"
#include "ns3/simulator.h"
#include "ns3/socket-factory.h"
#include "ns3/socket.h"
#include "ns3/uinteger.h"

#include "map"

#include "pnk-header.h"

#define SW 0
#define PT  1
#define TESTSW 2
#define TESTPT 3
#define DUP 4
#define DROP 5
#define SKIP 6
#define PROB 7
#define PAR 8
#define KLEENE 9

struct PnkPrgrmNode {
    uint32_t instr;
    uint32_t arg;
    double farg;
    PnkPrgrmNode* next1; // no branching: next1
    PnkPrgrmNode* next2; // used for branching
    PnkPrgrmNode* prev;
    int nodenr;
};

class PnkPrgrm {
    public:
        PnkPrgrm();
        ~PnkPrgrm();
        int addNode(int nodenr, uint32_t instr, uint32_t arg, double farg, bool nextis1);
        PnkPrgrmNode* getNode(uint32_t nodenr);

    private:
        PnkPrgrmNode* start;
        std::map<uint32_t, PnkPrgrmNode*> nodeNrToNode;
        uint32_t nodeCount;
};

PnkPrgrm::PnkPrgrm(){
    nodeCount = 0;
    return;
}

PnkPrgrmNode* PnkPrgrm::getNode(uint32_t nodenr){
    if (nodeNrToNode.count(nodenr) > 0)
        return nodeNrToNode[nodenr];
    return nullptr;
}

PnkPrgrm::~PnkPrgrm(){
    for (uint32_t i = 0; i < nodeCount; i++){
        delete nodeNrToNode[i];
    }
    return;
}

int PnkPrgrm::addNode(int parentnodenr, uint32_t instr, uint32_t arg, double farg, bool isnext2){
    PnkPrgrmNode* newnode = new PnkPrgrmNode();
    newnode->instr = instr;
    newnode->arg = arg;
    newnode->farg = farg;
    newnode->next1 = nullptr;
    newnode->next2 = nullptr;
    
    if (parentnodenr == -1){
        newnode->nodenr = 0;
        start = newnode;
        nodeNrToNode[0] = start; 
    } else {
        PnkPrgrmNode* node = nodeNrToNode[parentnodenr];
        if (!isnext2){
            node->next1 = newnode;
        } else {
            node->next2 = newnode;
        }
        newnode->prev = node;
        newnode->nodenr = nodeCount;
        nodeNrToNode[nodeCount] = newnode;
    }
    nodeCount++;
    return newnode->nodenr;
}

PnkPrgrm getCurrentProgram(){
    PnkPrgrm ret;
    int nodenr = 0;
    ret.addNode(-1, SW, 2, 1.0f, false);
    ret.addNode(0, DUP, 0, 0.0f, false);
    nodenr = ret.addNode(1, PROB, 0, 0.5, false );
    //branching left
    int left = ret.addNode(nodenr, SW, 0, 0.0f, false);
    left = ret.addNode(left, DUP, 0, 0.0f, false);
    //branching right
    int right = ret.addNode(nodenr, SW, 1, 0.0f, true);
    right = ret.addNode(right, DUP, 0, 0.0f, false);
    // maybe we should be able to merge the paths again?
    ret.addNode(left, DROP, 0, 0.0F, false);
    ret.addNode(right, DROP, 0, 0.0F, false);
    return ret;
}

std::string instrString(uint32_t instr){
    std::string ret = "";

    switch(instr){
        case SW:
            ret = "SW";
            break;
        case PT:
            ret = "PT";
            break;
        case DUP:
            ret = "DUP";
            break;
        case DROP:
            ret = "DROP";
            break;
        case SKIP:
            ret = "SKIP";
            break;
        case TESTSW:
            ret = "TESTSW";
            break;
        case TESTPT:
            ret = "TESTPT";
            break;
        case PROB:
            ret = "PROB";
            break;
        case PAR:
            ret = "PAR";
        default:
            ret = "UNKNOWN";
            break;
    }
    return ret;
}

namespace ns3
{

NS_LOG_COMPONENT_DEFINE("PnkServer");

NS_OBJECT_ENSURE_REGISTERED(PnkServer);


TypeId
PnkServer::GetTypeId()
{
    static TypeId tid =
        TypeId("ns3::PnkServer")
            .SetParent<Application>()
            .SetGroupName("Applications")
            .AddConstructor<PnkServer>()
            .AddAttribute("Port",
                          "Port on which we listen for incoming packets.",
                          UintegerValue(100),
                          MakeUintegerAccessor(&PnkServer::m_port),
                          MakeUintegerChecker<uint16_t>())
            .AddAttribute("PacketWindowSize",
                          "The size of the window used to compute the packet loss. This value "
                          "should be a multiple of 8.",
                          UintegerValue(32),
                          MakeUintegerAccessor(&PnkServer::GetPacketWindowSize,
                                               &PnkServer::SetPacketWindowSize),
                          MakeUintegerChecker<uint16_t>(8, 256))
            .AddTraceSource("Rx",
                            "A packet has been received",
                            MakeTraceSourceAccessor(&PnkServer::m_rxTrace),
                            "ns3::Packet::TracedCallback")
            .AddTraceSource("RxWithAddresses",
                            "A packet has been received",
                            MakeTraceSourceAccessor(&PnkServer::m_rxTraceWithAddresses),
                            "ns3::Packet::TwoAddressTracedCallback");
    return tid;
}

PnkServer::PnkServer()
    : m_lossCounter(0)
{
    NS_LOG_FUNCTION(this);
    m_received = 0;
    m_nodeAddressMap = {};
    m_socketMap = {};
}

PnkServer::~PnkServer()
{
    NS_LOG_FUNCTION(this);
}

uint16_t
PnkServer::GetPacketWindowSize() const
{
    NS_LOG_FUNCTION(this);
    return m_lossCounter.GetBitMapSize();
}

void
PnkServer::SetPacketWindowSize(uint16_t size)
{
    NS_LOG_FUNCTION(this << size);
    m_lossCounter.SetBitMapSize(size);
}

uint32_t
PnkServer::GetLost() const
{
    NS_LOG_FUNCTION(this);
    return m_lossCounter.GetLost();
}

uint64_t
PnkServer::GetReceived() const
{
    NS_LOG_FUNCTION(this);
    return m_received;
}

void
PnkServer::DoDispose()
{
    NS_LOG_FUNCTION(this);
    Application::DoDispose();
}

void
PnkServer::StartApplication()
{
    NS_LOG_FUNCTION(this);

    if (!m_socket)
    {
        TypeId tid = TypeId::LookupByName("ns3::UdpSocketFactory");
        m_socket = Socket::CreateSocket(GetNode(), tid);
        InetSocketAddress local = InetSocketAddress(Ipv4Address::GetAny(), m_port);
        if (m_socket->Bind(local) == -1)
        {
            NS_FATAL_ERROR("Failed to bind socket");
        }
    }

    m_socket->SetRecvCallback(MakeCallback(&PnkServer::HandleRead, this));

    if (!m_socket6)
    {
        TypeId tid = TypeId::LookupByName("ns3::UdpSocketFactory");
        m_socket6 = Socket::CreateSocket(GetNode(), tid);
        Inet6SocketAddress local = Inet6SocketAddress(Ipv6Address::GetAny(), m_port);
        if (m_socket6->Bind(local) == -1)
        {
            NS_FATAL_ERROR("Failed to bind socket");
        }
    }

    m_socket6->SetRecvCallback(MakeCallback(&PnkServer::HandleRead, this));

    for (auto const& x : m_nodeAddressMap)
    {
        // Now create sockets and store them in the socket map

        Address peerAddress = InetSocketAddress(x.second, 9);

        TypeId tid = TypeId::LookupByName("ns3::UdpSocketFactory");
        Ptr<Socket> cur_socket = Socket::CreateSocket(GetNode(), tid);
        if (cur_socket->Bind() == -1)
        {
            NS_FATAL_ERROR("Failed to bind socket");
        }
        cur_socket->Connect(peerAddress);

        m_socketMap[x.first] = cur_socket;
    }

    RngSeedManager::SetSeed(3);  // Changes seed from default of 1 to 3
    RngSeedManager::SetRun(7);

    m_rng = CreateObject<UniformRandomVariable>();
    m_rng->SetAttribute("Min", DoubleValue(0));
    m_rng->SetAttribute("Max", DoubleValue(1));
}

void 
PnkServer::SetNodeAddressMap(std::map<uint32_t, Ipv4Address> nodemap){
    m_nodeAddressMap = nodemap;
}

void
PnkServer::StopApplication()
{
    NS_LOG_FUNCTION(this);

    if (m_socket)
    {
        m_socket->SetRecvCallback(MakeNullCallback<void, Ptr<Socket>>());
    }
}

void
PnkServer::HandleRead(Ptr<Socket> socket)
{
    PnkPrgrm curprog = getCurrentProgram();
    const uint32_t thisNetworkNodeNumber = GetNode()->GetId();
    std::cout << "HandleRead called for nodeid " << thisNetworkNodeNumber << std::endl;
    NS_LOG_FUNCTION(this << socket);
    Ptr<Packet> packet;
    Address from;
    Address localAddress;
    while ((packet = socket->RecvFrom(from)))
    {
        socket->GetSockName(localAddress);
        m_rxTrace(packet);
        m_rxTraceWithAddresses(packet, from, localAddress);

        /*
                Ptr<Node> n = nodes.Get(1);
                Ptr<Ipv4> ipv4 = n->GetObject<Ipv4>();
                Ipv4InterfaceAddress ipv4_int_addr = ipv4->GetAddress(1, 0);
                Ipv4Address ip_addr = ipv4_int_addr.GetLocal();

        */

        // we moeten weten welke socket, denk ik.        

        if (packet->GetSize() > 0)
        {
            uint32_t receivedSize = packet->GetSize();
            SeqTsHeader seqTs;
            uint32_t currentSequenceNumber = 0;
            // packet->RemoveHeader(seqTs);
            // uint32_t currentSequenceNumber = seqTs.GetSeq();

            PnkHeader pnkhead;
            packet->RemoveHeader(pnkhead);


            std::cout << "Pnk header found." << std::endl;
            std::cout << "Cur: " << pnkhead.GetCur() << std::endl;
            std::cout << "Sw: " << pnkhead.GetSwitch() << std::endl;
            std::cout << "Pt: " << pnkhead.GetPort() << std::endl;

            Ptr<Packet> packet_copy = packet->Copy();
            bool done = false;
            bool takefirstbranch = true;
            while (!done){
                takefirstbranch = true;
                PnkPrgrmNode* curnode = curprog.getNode(pnkhead.GetCur());
                uint32_t instr = curnode->instr;
                uint32_t arg = curnode->arg;
                double farg = curnode->farg;
                uint32_t curnodenr = curnode->nodenr;

                std::cout << "PC: " << curnodenr << ", instr: " << instrString(instr) << ", arg: " << arg << ", farg: " << farg << std::endl;

                switch(instr){
                    case SW: // set the switch number of the packet
                        pnkhead.SetSwitch(arg);
                        std::cout << "SW <- " << arg << std::endl;
                        break;
                    case PT:
                        pnkhead.SetPort(arg);
                        std::cout << "PT <- " << arg << std::endl;
                        break;
                    case DUP: {// send the packet
                        // figure out which socket to send the packet on
                        std::cout << "DUP" << std::endl;
                        
                        int nextnodenr;
                        if (curnode->next1 != nullptr){
                            nextnodenr = curnode->next1->nodenr;
                            std::cout << "Next program node is " << nextnodenr << std::endl;
                        }
                        else {
                            // error we do not know what to do
                            std::cout << "No next node???" << std::endl;
                            return;
                        }

                        pnkhead.SetCur(nextnodenr);
                        packet_copy->AddHeader(pnkhead);

                        if (m_socketMap[pnkhead.GetSwitch()]->Send(packet_copy)) {
                            std::cout << "Sending to node " << pnkhead.GetSwitch() << ", ip " << m_nodeAddressMap[pnkhead.GetSwitch()] << std::endl;
                        } else {
                            std::cout << "Sending to " << m_nodeAddressMap[pnkhead.GetSwitch()] << " failed" << std::endl;
                        }
                        done = true;
                        break;
                    }
                    case DROP: {
                        std::cout << "DROP" << std::endl;
                        return;
                    }
                    case PROB: {
                        std::cout << "PROB(" << farg << ")" << std::endl;
                        double rnd = m_rng->GetValue();
                        std::cout << "Rnd value: " << rnd << " so taking " << (rnd < farg ? "left" : "right") << " branch" << std::endl;
                        takefirstbranch = (rnd < farg);
                        std::cout << "Take first branch: " << takefirstbranch << std::endl;
                        break;
                    }
                    case PAR: {
                        // this one requires some thought
                        break;
                    }
                    case TESTSW: {
                        std::cout << "TEST SW" << pnkhead.GetSwitch() << " == " << arg << " ?" << std::endl;
                        if (arg == pnkhead.GetSwitch()){
                            std::cout << "True" << std::endl;
                            // do nothing
                        }
                        else {
                            // effectively drop
                            std::cout << "False" << std::endl;
                            done = true;
                            return;
                        }
                        // break;
                    }
                    case TESTPT: {
                        std::cout << "TEST PT(" << pnkhead.GetPort() << ") == " << arg << " ?" << std::endl;
                        if (arg == pnkhead.GetPort()){
                            std::cout << "True" << std::endl;
                            // do nothing
                        }
                        else {
                            // effectively drop
                            std::cout << "False" << std::endl;
                            done = true;
                            return;
                        }
                        // break;
                    }
                    case SKIP: {
                        std::cout << "SKIP" << std::endl;
                        break;
                    }
                    default: {
                        std::cout << "WARNING: unknown instruction" << std::endl;
                        done = true;
                        break;
                    }
                }
                // pc++; oh how simple it was when we just had linear programs
                
                if (takefirstbranch && curnode->next1 != nullptr){
                    pnkhead.SetCur(curnode->next1->nodenr); 
                }  else if (!takefirstbranch && curnode->next2 != nullptr) {
                    pnkhead.SetCur(curnode->next2->nodenr);
                } else {
                    std::cout << "ERRORRRR" << std::endl;
                    return;
                }

            }


            if (InetSocketAddress::IsMatchingType(from))
            {
                NS_LOG_INFO("TraceDelay: RX " << receivedSize << " bytes from "
                                              << InetSocketAddress::ConvertFrom(from).GetIpv4()
                                              << " Sequence Number: " << currentSequenceNumber
                                              << " Uid: " << packet->GetUid() << " TXtime: "
                                              << seqTs.GetTs() << " RXtime: " << Simulator::Now()
                                              << " Delay: " << Simulator::Now() - seqTs.GetTs());
            }
            else if (Inet6SocketAddress::IsMatchingType(from))
            {
                NS_LOG_INFO("TraceDelay: RX " << receivedSize << " bytes from "
                                              << Inet6SocketAddress::ConvertFrom(from).GetIpv6()
                                              << " Sequence Number: " << currentSequenceNumber
                                              << " Uid: " << packet->GetUid() << " TXtime: "
                                              << seqTs.GetTs() << " RXtime: " << Simulator::Now()
                                              << " Delay: " << Simulator::Now() - seqTs.GetTs());
            }

            m_lossCounter.NotifyReceived(currentSequenceNumber);
            m_received++;
        }
    }
}

} // Namespace ns3
