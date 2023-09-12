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
 */

#include "pnkhelper.h"

#include "ns3/applications-module.h"
#include "ns3/core-module.h"
#include "ns3/internet-module.h"
#include "ns3/mobility-helper.h"
#include "ns3/netanim-module.h"
#include "ns3/network-module.h"
#include "ns3/point-to-point-module.h"

// Triangle Network Topology
//
//       10.1.1.0
// n0 -------------- n1
//    point-to-point
//

using namespace ns3;

NS_LOG_COMPONENT_DEFINE("FirstScriptExample");

int main(int argc, char *argv[])
{
    CommandLine cmd(__FILE__);
    cmd.Parse(argc, argv);

    Time::SetResolution(Time::NS);
    LogComponentEnable("PnkClientApplication", LOG_LEVEL_INFO);
    LogComponentEnable("PnkServerApplication", LOG_LEVEL_INFO);
    // LogComponentEnable("PnkServerApplication", LOG_LEVEL_LOGIC);

    NodeContainer nodes;
    nodes.Create(3);

    // MobilityHelper mobility;
    // mobility.SetMobilityModel("ns3::ConstantPositionMobilityModel");
    // mobility.Install(nodes);

    PointToPointHelper pointToPoint;
    pointToPoint.SetDeviceAttribute("DataRate", StringValue("5Mbps"));
    pointToPoint.SetChannelAttribute("Delay", StringValue("2ms"));

    NetDeviceContainer devices;
    devices = pointToPoint.Install(nodes.Get(0), nodes.Get(1));
    devices.Add(pointToPoint.Install(nodes.Get(0), nodes.Get(2)));
    devices.Add(pointToPoint.Install(nodes.Get(1), nodes.Get(2)));

    // some animation stuf
    std::string animFile = "pnk-animation.xml"; // Name of file for animation output

    InternetStackHelper stack;
    stack.Install(nodes);

    Ipv4AddressHelper address;
    address.SetBase("10.1.1.0", "255.255.255.0");

    Ipv4InterfaceContainer interfaces = address.Assign(devices);

    PnkServerHelper echoServer(9);

    ApplicationContainer serverApps = echoServer.Install(nodes);
    serverApps.Start(Seconds(1.0));
    serverApps.Stop(Seconds(10.0));

    PnkClientHelper echoClient(interfaces.GetAddress(1), 9);

    echoClient.SetAttribute("MaxPackets", UintegerValue(1));
    echoClient.SetAttribute("Interval", TimeValue(Seconds(1.0)));
    echoClient.SetAttribute("PacketSize", UintegerValue(1024));

    ApplicationContainer clientApps = echoClient.Install(nodes);

    // for (auto app = clientApps.Begin(); app != clientApps.End(); ++app)
    // {
    //     echoClient.SetFill(app, "Hello World");
    // }

    clientApps.Start(Seconds(2.0));
    clientApps.Stop(Seconds(10.0));

    for (uint32_t i = 0; i < 3; ++i)
    {
        // Create an on/off app sending packets to the same leaf right side
        AddressValue remoteAddress(InetSocketAddress(interfaces.GetAddress((i + 1) % 3), 100 + i));
        echoClient.SetAttribute("RemoteAddress", remoteAddress);
        clientApps.Add(echoClient.Install(nodes.Get(i)));
    }

    // Create the animation object and configure for specified output
    AnimationInterface anim(animFile);
    anim.EnablePacketMetadata();                                // Optional
    anim.EnableIpv4L3ProtocolCounters(Seconds(0), Seconds(10)); // Optional

    // Set up the actual simulation
    Ipv4GlobalRoutingHelper::PopulateRoutingTables();

    echoClient.SetFill(clientApps.Get(0), "1");
    echoClient.SetFill(clientApps.Get(1), "02");
    echoClient.SetFill(clientApps.Get(2), "003");
    echoClient.SetFill(clientApps.Get(3), "0004");
    echoClient.SetFill(clientApps.Get(4), "00005");
    echoClient.SetFill(clientApps.Get(5), "000006");

    Simulator::Run();
    std::cout << "Animation Trace file created:" << animFile << std::endl;
    Simulator::Destroy();
}
