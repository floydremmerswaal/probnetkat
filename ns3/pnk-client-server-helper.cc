/*
 * Copyright (c) 2008 INRIA
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
 * Author: Mohamed Amine Ismail <amine.ismail@sophia.inria.fr>
 */
#include "pnk-client-server-helper.h"

#include "ns3/string.h"
#include "ns3/uinteger.h"

#include "pnk-client.h"
#include "pnk-server.h"
#include "pnk-trace-client.h"

namespace ns3
{

PnkServerHelper::PnkServerHelper()
{
    m_factory.SetTypeId(PnkServer::GetTypeId());
}

PnkServerHelper::PnkServerHelper(uint16_t port)
{
    m_factory.SetTypeId(PnkServer::GetTypeId());
    SetAttribute("Port", UintegerValue(port));
}

void
PnkServerHelper::SetAttribute(std::string name, const AttributeValue& value)
{
    m_factory.Set(name, value);
}

ApplicationContainer
PnkServerHelper::Install(NodeContainer c)
{
    ApplicationContainer apps;
    for (NodeContainer::Iterator i = c.Begin(); i != c.End(); ++i)
    {
        Ptr<Node> node = *i;

        m_server = m_factory.Create<PnkServer>();
        node->AddApplication(m_server);
        apps.Add(m_server);
    }
    return apps;
}

Ptr<PnkServer>
PnkServerHelper::GetServer()
{
    return m_server;
}

PnkClientHelper::PnkClientHelper()
{
    m_factory.SetTypeId(PnkClient::GetTypeId());
}

PnkClientHelper::PnkClientHelper(Address address, uint16_t port)
{
    m_factory.SetTypeId(PnkClient::GetTypeId());
    SetAttribute("RemoteAddress", AddressValue(address));
    SetAttribute("RemotePort", UintegerValue(port));
}

PnkClientHelper::PnkClientHelper(Address address)
{
    m_factory.SetTypeId(PnkClient::GetTypeId());
    SetAttribute("RemoteAddress", AddressValue(address));
}

void
PnkClientHelper::SetAttribute(std::string name, const AttributeValue& value)
{
    m_factory.Set(name, value);
}

ApplicationContainer
PnkClientHelper::Install(NodeContainer c)
{
    ApplicationContainer apps;
    for (NodeContainer::Iterator i = c.Begin(); i != c.End(); ++i)
    {
        Ptr<Node> node = *i;
        Ptr<PnkClient> client = m_factory.Create<PnkClient>();
        node->AddApplication(client);
        apps.Add(client);
    }
    return apps;
}

PnkTraceClientHelper::PnkTraceClientHelper()
{
    m_factory.SetTypeId(PnkTraceClient::GetTypeId());
}

PnkTraceClientHelper::PnkTraceClientHelper(Address address, uint16_t port, std::string filename)
{
    m_factory.SetTypeId(PnkTraceClient::GetTypeId());
    SetAttribute("RemoteAddress", AddressValue(address));
    SetAttribute("RemotePort", UintegerValue(port));
    SetAttribute("TraceFilename", StringValue(filename));
}

PnkTraceClientHelper::PnkTraceClientHelper(Address address, std::string filename)
{
    m_factory.SetTypeId(PnkTraceClient::GetTypeId());
    SetAttribute("RemoteAddress", AddressValue(address));
    SetAttribute("TraceFilename", StringValue(filename));
}

void
PnkTraceClientHelper::SetAttribute(std::string name, const AttributeValue& value)
{
    m_factory.Set(name, value);
}

ApplicationContainer
PnkTraceClientHelper::Install(NodeContainer c)
{
    ApplicationContainer apps;
    for (NodeContainer::Iterator i = c.Begin(); i != c.End(); ++i)
    {
        Ptr<Node> node = *i;
        Ptr<PnkTraceClient> client = m_factory.Create<PnkTraceClient>();
        node->AddApplication(client);
        apps.Add(client);
    }
    return apps;
}

} // namespace ns3
