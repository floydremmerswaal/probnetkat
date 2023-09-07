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

#define RMIN 1
#define RMAX 10

using namespace ns3;

int main(int argc, char *argv[])
{
    Config::SetDefault("ns3::OnOffApplication::PacketSize", UintegerValue(512));
    Config::SetDefault("ns3::OnOffApplication::DataRate", StringValue("500kb/s"));

    RngSeedManager::SetSeed(3); // Changes seed from default of 1 to 3
    RngSeedManager::SetRun(7);  // Changes run number from default of 1 to 7

    Ptr<UniformRandomVariable> x = CreateObject<UniformRandomVariable>();
    x->SetAttribute("Min", DoubleValue(RMIN));
    x->SetAttribute("Max", DoubleValue(RMAX));
    int myRandomNo = 0;
    for (int i = 0; i < 10; ++i)
    {
        myRandomNo = x->GetInteger();
        std::cout << "Random Number: " << myRandomNo << std::endl;
    }

                     // If non-zero, number of both left and right
    std::string animFile = "pnk-animation.xml"; // Name of file for animation output

    NodeContainer nodes(3);
    PointToPointHelper pointToPoint;
    NetDeviceContainer devices;

    devices.Add(pointToPoint.Install(nodes.Get(0), nodes.Get(1)));
    devices.Add(pointToPoint.Install(nodes.Get(0), nodes.Get(2)));
    devices.Add(pointToPoint.Install(nodes.Get(1), nodes.Get(2)));

    // Install Stack
    InternetStackHelper stack;
    pointToPoint.InstallStack(stack);

    // Assign IP Addresses
    devices.AssignIpv4Addresses(Ipv4AddressHelper("10.1.1.0", "255.255.255.0"),
                          Ipv4AddressHelper("10.2.1.0", "255.255.255.0"),
                          Ipv4AddressHelper("10.3.1.0", "255.255.255.0"));

    // Install on/off app on all right side nodes
    OnOffHelper clientHelper("ns3::UdpSocketFactory", Address());
    clientHelper.SetAttribute("OnTime", StringValue("ns3::UniformRandomVariable"));
    clientHelper.SetAttribute("OffTime", StringValue("ns3::UniformRandomVariable"));
    ApplicationContainer clientApps;

    for (uint32_t i = 0; i < ((d.RightCount() < d.LeftCount()) ? d.RightCount() : d.LeftCount());
         ++i)
    {
        // Create an on/off app sending packets to the same leaf right side
        AddressValue remoteAddress(InetSocketAddress(d.GetLeftIpv4Address(i), 1000));
        clientHelper.SetAttribute("Remote", remoteAddress);
        clientApps.Add(clientHelper.Install(d.GetRight(i)));
    }

    clientApps.Start(Seconds(0.0));
    clientApps.Stop(Seconds(10.0));

    // Set the bounding box for animation
    d.BoundingBox(1, 1, 100, 100);

    // Create the animation object and configure for specified output
    AnimationInterface anim(animFile);
    anim.EnablePacketMetadata();                                // Optional
    anim.EnableIpv4L3ProtocolCounters(Seconds(0), Seconds(10)); // Optional

    // Set up the actual simulation
    Ipv4GlobalRoutingHelper::PopulateRoutingTables();

    Simulator::Run();
    std::cout << "Animation Trace file created:" << animFile << std::endl;
    Simulator::Destroy();
    return 0;
}
