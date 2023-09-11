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
 * Author: Mathieu Lacage <mathieu.lacage@sophia.inria.fr>
 */
#include "pnkhelper.h"

#include "ns3/names.h"
#include "ns3/udp-echo-client.h"
#include "ns3/udp-echo-server.h"
#include "ns3/uinteger.h"

#include "pnkclient.h"
#include "pnkserver.h"

namespace ns3
{

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
PnkServerHelper::Install(Ptr<Node> node) const
{
    return ApplicationContainer(InstallPriv(node));
}

ApplicationContainer
PnkServerHelper::Install(std::string nodeName) const
{
    Ptr<Node> node = Names::Find<Node>(nodeName);
    return ApplicationContainer(InstallPriv(node));
}

ApplicationContainer
PnkServerHelper::Install(NodeContainer c) const
{
    ApplicationContainer apps;
    for (NodeContainer::Iterator i = c.Begin(); i != c.End(); ++i)
    {
        apps.Add(InstallPriv(*i));
    }

    return apps;
}

Ptr<Application>
PnkServerHelper::InstallPriv(Ptr<Node> node) const
{
    Ptr<Application> app = m_factory.Create<PnkServer>();
    node->AddApplication(app);

    return app;
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

void
PnkClientHelper::SetFill(Ptr<Application> app, std::string fill)
{
    app->GetObject<PnkClient>()->SetFill(fill);
}

void
PnkClientHelper::SetFill(Ptr<Application> app, uint8_t fill, uint32_t dataLength)
{
    app->GetObject<PnkClient>()->SetFill(fill, dataLength);
}

void
PnkClientHelper::SetFill(Ptr<Application> app,
                             uint8_t* fill,
                             uint32_t fillLength,
                             uint32_t dataLength)
{
    app->GetObject<PnkClient>()->SetFill(fill, fillLength, dataLength);
}

ApplicationContainer
PnkClientHelper::Install(Ptr<Node> node) const
{
    return ApplicationContainer(InstallPriv(node));
}

ApplicationContainer
PnkClientHelper::Install(std::string nodeName) const
{
    Ptr<Node> node = Names::Find<Node>(nodeName);
    return ApplicationContainer(InstallPriv(node));
}

ApplicationContainer
PnkClientHelper::Install(NodeContainer c) const
{
    ApplicationContainer apps;
    for (NodeContainer::Iterator i = c.Begin(); i != c.End(); ++i)
    {
        apps.Add(InstallPriv(*i));
    }

    return apps;
}

Ptr<Application>
PnkClientHelper::InstallPriv(Ptr<Node> node) const
{
    Ptr<Application> app = m_factory.Create<PnkClient>();
    node->AddApplication(app);

    return app;
}

} // namespace ns3
