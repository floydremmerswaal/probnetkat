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

#include "map"
#include "pnk-header.h"
#include "pnk-program.h"

#include "ns3/core-module.h"
#include "ns3/inet-socket-address.h"
#include "ns3/inet6-socket-address.h"
#include "ns3/ipv4-address.h"
#include "ns3/log.h"
#include "ns3/nstime.h"
#include "ns3/packet-loss-counter.h"
#include "ns3/packet.h"
#include "ns3/seq-ts-header.h"
#include "ns3/simulator.h"
#include "ns3/socket-factory.h"
#include "ns3/socket.h"
#include "ns3/uinteger.h"

std::string
instrString(uint32_t instr)
{
    std::string ret = "";

    switch (instr)
    {
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
    case KLEENESTART:
        ret = "KLEENESTART";
        break;
    case KLEENESTOP:
        ret = "KLEENESTOP";
        break;
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

    PnkServer::getBranch(uint32_t nodeNr, uint32_t branchNr)
    {
        if (m_branchMap
                ->contains(nodeNr, branchNr))
        {
            return m_branchMap[nodeNr][branchNr];
        }
        else
        {
            return false;
        }
    }

    PnkServer::setBranch(uint32_t nodeNr, uint32_t branchNr, bool value)
    {
        m_branchMap->insert(std::pair<std::pair<int, int>, bool>(std::pair<int, int>(nodeNr, branchNr), value));
    }

    PnkServer::onDrop(Ptr<const Packet> packet)
    {
        std::cout << "Dropped packet" << std::endl;
        // if a branch was taken in a PAR, we need to signal that the branch is no longer to be taken

        // get packet header
        PnkHeader pnkhead;
        packet->RemoveHeader(pnkhead);
        if pnkhead
            .GetBranchCount() > 0
            {
                // get the branch number
                uint32_t nodenr = pnkhead.PopBranch();
                setBranch(nodenr, pnkhead.GetCur(), false);
            }
    }

    PnkServer::PnkServer() should
        : m_lossCounter(0)
    {
        NS_LOG_FUNCTION(this);
        m_received = 0;
        m_nodeAddressMap = {};
        m_socketMap = {};
        nodenrToBranchMap = {};
        // TODO initialize the program, so that we do not have to construct the program every time
        // HandleRead is called
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
        } else {
            // parallel node
            // for now, just pick the first one

            // TODO reference a map of the parent program that tells us which
            // par branches lead to drops, and skip those if possible (if all branches are marked
            // we will pick one of them anyway)

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

        for (const auto &x : m_nodeAddressMap)
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

        RngSeedManager::SetSeed(3); // Changes seed from default of 1 to 3
        RngSeedManager::SetRun(7);

        m_rng = CreateObject<UniformRandomVariable>();
        m_rng->SetAttribute("Min", DoubleValue(0));
        m_rng->SetAttribute("Max", DoubleValue(1));
    }

    void
    PnkServer::SetNodeAddressMap(std::map<uint32_t, Ipv4Address> nodemap)
    {
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

    bool
    PnkServer::SendToNodeNr(uint32_t nodenr, Ptr<Packet> packet)
    {
        if (m_socketMap.count(nodenr) == 0)
        {
            std::cout << "WARNING: no socket found for node " << nodenr << std::endl;
            std::cout << "This might be a problem in the ProbNetKAT program or the network configuration" << std::endl;
            std::cout << "Are you sure this node exists?" << std::endl;
            return false;
        }
        return m_socketMap[nodenr]->Send(packet);
    }

    int getNextNodeNr(PnkPrgrm *program, int curnode, Ptr<UniformRandomVariable> m_rng)
    {
        PnkNode *node = program->getNode(curnode);
        if (node->edges.size() == 0)
        {
            return -1;
        }
        else if (node->edges.size() == 1)
        {
            return node->edges[0].to;
        }
        else
        {
            // if it is a prob node, normalise the weights
            // pick one of the edges
            if (node->instr == PROB)
            {
                double sum = 0;
                for (uint32_t i = 0; i < node->edges.size(); i++)
                {
                    sum += node->edges[i].weight;
                }

                double rnd = m_rng->GetValue() * sum;
                double cur = 0;
                for (uint32_t i = 0; i < node->edges.size(); i++)
                {
                    cur += node->edges[i].weight;
                    if (rnd < cur)
                    {
                        return node->edges[i].to;
                    }
                }
            }
            else
            {
                // parallel node
                // for now, just pick the first one
                return node->edges[0].to;
            }
        }
        return -1;
    }

    void
    PnkServer::HandleRead(Ptr<Socket> socket)
    {
        static PnkPrgrm curprog = getAutomaton();
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

            if (packet->GetSize() > 0)
            {
                uint32_t receivedSize = packet->GetSize();
                SeqTsHeader seqTs;
                uint32_t currentSequenceNumber = 0;

                PnkHeader pnkhead;
                packet->RemoveHeader(pnkhead);

                std::cout << "Pnk header found." << std::endl;
                std::cout << "Cur: " << pnkhead.GetCur() << std::endl;
                std::cout << "Sw: " << pnkhead.GetSwitch() << std::endl;
                std::cout << "Pt: " << pnkhead.GetPort() << std::endl;

                Ptr<Packet> packet_copy = packet->Copy();
                bool done = false;

                while (!done)
                {
                    PnkNode *curnode = curprog.getNode(pnkhead.GetCur());
                    uint32_t instr = curnode->instr;
                    uint32_t arg = curnode->arg;
                    uint32_t curnodenr = curnode->nodenr;

                    std::cout << "PC: " << curnodenr << ", instr: " << instrString(instr)
                              << ", arg: " << arg << std::endl;

                    switch (instr)
                    {
                    case SW: // set the switch number of the packet
                        pnkhead.SetSwitch(arg);
                        std::cout << "SW <- " << arg << std::endl;
                        break;
                    case PT:
                        pnkhead.SetPort(arg);
                        std::cout << "PT <- " << arg << std::endl;
                        break;
                    case DUP:
                    { // send the packet
                        // figure out which socket to send the packet on
                        std::cout << "DUP" << std::endl;

                        int nextnodenr = getNextNodeNr(&curprog, curnodenr, m_rng);

                        std::cout << "Next program node is " << nextnodenr << std::endl;

                        pnkhead.SetCur(nextnodenr);
                        packet_copy->AddHeader(pnkhead);

                        if (SendToNodeNr(pnkhead.GetSwitch(), packet_copy))
                        {
                            std::cout << "Sending to node " << pnkhead.GetSwitch() << ", ip "
                                      << m_nodeAddressMap[pnkhead.GetSwitch()] << std::endl;
                        }
                        else
                        {
                            std::cout << "Sending to " << m_nodeAddressMap[pnkhead.GetSwitch()]
                                      << " failed" << std::endl;
                        }
                        done = true;
                        return;
                    }
                    case DROP:
                    {
                        std::cout << "DROP" << std::endl;
                        return;
                    }
                    case PROB:
                    {
                        std::cout << "PROB" << std::endl;
                        break;
                    }
                    case PAR:
                    {
                        std::cout << "PAR" << std::endl;
                        break;
                    }
                    case TESTSW:
                    {
                        // TODO, test against the current node number instead of the header value..?
                        // actually..
                        // if we are in switch 2
                        // sw <- 1; sw = 2; dup
                        // should be dropped, right? so maybe we should not check for the node number

                        std::cout << "TEST SW(" << pnkhead.GetSwitch() << ") == " << arg << "?"
                                  << std::endl;
                        if (arg == pnkhead.GetSwitch())
                        {
                            std::cout << "True" << std::endl;
                            // do nothing
                        }
                        else
                        {
                            // effectively drop
                            std::cout << "False" << std::endl;
                            done = true;
                            return;
                        }
                        break;
                        // break;
                    }
                    case TESTPT:
                    {
                        // TODO, test for port of this node? discussion at TESTSW
                        // probably should not do that for ports, but probably SHOULD for switches

                        std::cout << "TEST PT(" << pnkhead.GetPort() << ") == " << arg << "?"
                                  << std::endl;
                        if (arg == pnkhead.GetPort())
                        {
                            std::cout << "True" << std::endl;
                            // do nothing
                        }
                        else
                        {
                            // effectively drop
                            std::cout << "False" << std::endl;
                            done = true;
                            return;
                        }
                        break;
                        // break;
                    }
                    case SKIP:
                    {
                        std::cout << "SKIP" << std::endl;
                        break;
                    }
                    default:
                    {
                        std::cout << "WARNING: unknown instruction" << std::endl;
                        done = true;
                        break;
                    }
                    }
                    // pc++; oh how simple it was when we just had linear programs

                    int nextnodenr = getNextNodeNr(&curprog, curnodenr, m_rng);
                    if (nextnodenr == -1)
                    {
                        std::cout << "WARNING: no next node found" << std::endl;
                        done = true;
                        return;
                    }
                    else
                    {
                        pnkhead.SetCur(nextnodenr);
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
