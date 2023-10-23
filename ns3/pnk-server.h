/*
 * Copyright (c) 2007,2008,2009 INRIA, UDCAST
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
 *
 */

#ifndef PNK_SERVER_H
#define PNK_SERVER_H

#include "ns3/packet-loss-counter.h"


#include "ns3/core-module.h"
#include "ns3/address.h"
#include "ns3/application.h"
#include "ns3/event-id.h"
#include "ns3/ptr.h"
#include "ns3/traced-callback.h"

namespace ns3
{
/**
 * \ingroup applications
 * \defgroup pnkclientserver PnkClientServer
 */

/**
 * \ingroup pnkclientserver
 *
 * \brief A pnk server, receives pnk packets from a remote hsost.
 *
 * pnk packets carry a 32bits sequence number followed by a 64bits time
 * stamp in their payloads. The application uses the sequence number
 * to determine if a packet is lost, and the time stamp to compute the delay.
 */
class PnkServer : public Application
{
  public:
    /**
     * \brief Get the type ID.
     * \return the object TypeId
     */
    static TypeId GetTypeId();
    PnkServer();
    ~PnkServer() override;
    /**
     * \brief Returns the number of lost packets
     * \return the number of lost packets
     */
    uint32_t GetLost() const;

    /**
     * \brief Returns the number of received packets
     * \return the number of received packets
     */
    uint64_t GetReceived() const;

    /**
     * \brief Returns the size of the window used for checking loss.
     * \return the size of the window used for checking loss.
     */
    uint16_t GetPacketWindowSize() const;

    /**
     * \brief Set the size of the window used for checking loss. This value should
     *  be a multiple of 8
     * \param size the size of the window used for checking loss. This value should
     *  be a multiple of 8
     */
    void SetPacketWindowSize(uint16_t size);

    void SetNodeAddressMap(std::map<uint32_t, Ipv4Address> nodemap);

    bool SendToNodeNr(uint32_t nodeNr, Ptr<Packet> packet);

  protected:
    void DoDispose() override;

  private:
    void StartApplication() override;
    void StopApplication() override;

    /**
     * \brief Handle a packet reception.
     *
     * This function is called by lower layers.
     *
     * \param socket the socket the packet was received to.
     */

    std::map<uint32_t, Ipv4Address> m_nodeAddressMap;
    std::map<uint32_t, Ptr<Socket>> m_socketMap; //!< IPv4 Socket maps 
    Ptr<UniformRandomVariable> m_rng;

    void HandleRead(Ptr<Socket> socket);

    uint16_t m_port;                 //!< Port on which we listen for incoming packets.
    Ptr<Socket> m_socket;            //!< IPv4 Socket
    Ptr<Socket> m_socket6;           //!< IPv6 Socket
    
    uint64_t m_received;             //!< Number of received packets
    PacketLossCounter m_lossCounter; //!< Lost packet counter

    std::map<uint32_t, std::map<uint32_t, bool>> nodenrToBranchMap;

    /// Callbacks for tracing the packet Rx events
    TracedCallback<Ptr<const Packet>> m_rxTrace;

    /// Callbacks for tracing the packet Rx events, includes source and destination addresses
    TracedCallback<Ptr<const Packet>, const Address&, const Address&> m_rxTraceWithAddresses;
};

} // namespace ns3

#endif /* PNK_SERVER_H */
