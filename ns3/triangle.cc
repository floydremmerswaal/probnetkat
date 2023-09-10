/*
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
 * Author: George F. Riley<riley@ece.gatech.edu>
 */

#include "ns3/applications-module.h"
#include "ns3/core-module.h"
#include "ns3/internet-module.h"
#include "ns3/netanim-module.h"
#include "ns3/network-module.h"
#include "ns3/point-to-point-layout-module.h"
#include "ns3/point-to-point-module.h"

#include <iostream>

using namespace ns3;

int main(int argc, char *argv[])
{

    NodeContainer nodes(3);
    PointToPointHelper pointToPoint;
    NetDeviceContainer devices;

    devices = pointToPoint.Install(nodes.Get(0), nodes.Get(1));
    devices.Add(pointToPoint.Install(nodes.Get(0), nodes.Get(2)));
    devices.Add(pointToPoint.Install(nodes.Get(1), nodes.Get(2)));

    // Install Stack
    InternetStackHelper stack;
    stack.Install(nodes);

    Ipv4AddressHelper address;
    address.SetBase("10.1.1.0", "255.255.255.0");

    Ipv4InterfaceContainer interfaces = address.Assign(devices);


    for (uint32_t i = 0; i < 3; ++i)
    {
        // Create an on/off app sending packets to the same leaf right side
    }

    std::cout << "test 123" << std::endl;
    // clientApps.Start(Seconds(0.0));
    // clientApps.Stop(Seconds(10.0));

    Simulator::Run();
    Simulator::Destroy();
    return 0;
}
