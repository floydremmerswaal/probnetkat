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

const uint32_t SW = 0;
const uint32_t PT = 1;
const uint32_t DUP = 2;
const uint32_t DROP = 3;

uint32_t instr_map[] = {SW, DUP, SW, DUP, SW, DUP, DROP};

uint32_t argument_map[] = {2, 0, 1, 0, 2, 0, 0};

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
    NS_LOG_FUNCTION(this << socket);
    Ptr<Packet> packet;
    Address from;
    Address localAddress;
    while ((packet = socket->RecvFrom(from)))
    {
        socket->GetSockName(localAddress);
        m_rxTrace(packet);
        m_rxTraceWithAddresses(packet, from, localAddress);
        if (packet->GetSize() > 0)
        {
            uint32_t receivedSize = packet->GetSize();
            SeqTsHeader seqTs;
            uint32_t currentSequenceNumber = 0;
            // packet->RemoveHeader(seqTs);
            // uint32_t currentSequenceNumber = seqTs.GetSeq();

            PnkHeader pnkhead;
            packet->RemoveHeader(pnkhead);
            uint16_t pnkhead_val = pnkhead.GetData();

            std::cout << "Pnk header found: " << pnkhead_val << std::endl;

            // print the attributes Node0, Node1 and Node2

            uint32_t instr = instr_map[pnkhead_val];
            Ptr<Packet> packet_copy = packet->Copy();
            bool done = false;
            // switch number
            uint32_t target_sw = 0;
            // program counter
            uint32_t pc = 0;


            while (!done){
                switch(instr_map[pc]){
                    case SW: // set the switch number of the packet
                        std::cout << "SW" << std::endl;
                        target_sw = argument_map[pc];
                        break;
                    case DUP: // send the packet
                        // figure out which socket to send the packet on
                        std::cout << "DUP" << std::endl;
                        uint32_t arg = argument_map[pnkhead_val];
                        m_socket->Send(packet_copy);
                        done = true;
                        break;
                    case DROP:
                        std::cout << "DROP" << std::endl;
                        done = true;
                        break;
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
