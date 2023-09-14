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

#include "pnk-client-server-helper.h"

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

NS_LOG_COMPONENT_DEFINE("TriangleTest");

int main(int argc, char *argv[])
{
    CommandLine cmd(__FILE__);
    cmd.Parse(argc, argv);

    Time::SetResolution(Time::NS);
    LogComponentEnable("PnkClient", LOG_LEVEL_INFO);
    LogComponentEnable("PnkServer", LOG_LEVEL_INFO);
    // LogComponentEnable("PnkServerApplication", LOG_LEVEL_LOGIC);

    // NodeContainer nodes;
    // nodes.Create(3);

    // MobilityHelper mobility;
    // mobility.SetMobilityModel("ns3::ConstantPositionMobilityModel");
    // mobility.Install(nodes);

    const int n_nodes = 3;

    NodeContainer nodes; // Declare nodes objects
    nodes.Create(n_nodes);

    std::string LinkRate("10Mbps");
    std::string LinkDelay("2ms");

    NS_LOG_INFO("Create P2P Link Attributes.");

    PointToPointHelper p2p;
    p2p.SetDeviceAttribute("DataRate", StringValue(LinkRate));
    p2p.SetChannelAttribute("Delay", StringValue(LinkDelay));

    NS_LOG_INFO("Install Internet Stack to Nodes.");

    InternetStackHelper internet;
    internet.Install(NodeContainer::GetGlobal());

    NS_LOG_INFO("Assign Addresses to Nodes.");

    Ipv4AddressHelper ipv4_n;
    ipv4_n.SetBase("10.0.0.0", "255.255.255.252");

    NS_LOG_INFO("Create Links Between Nodes.");

    uint32_t linkCount = 0;

    std::vector<std::vector<bool>> Adj_Matrix;

    Adj_Matrix.push_back({0,1,1});
    Adj_Matrix.push_back({0,0,1});
    Adj_Matrix.push_back({0,0,0});

    for (size_t i = 0; i < Adj_Matrix.size(); i++)
    {
        for (size_t j = 0; j < Adj_Matrix[i].size(); j++)
        {
            if (Adj_Matrix[i][j] == 1)
            {
                NodeContainer n_links = NodeContainer(nodes.Get(i), nodes.Get(j));
                NetDeviceContainer n_devs = p2p.Install(n_links);
                ipv4_n.Assign(n_devs);
                ipv4_n.NewNetwork();
                linkCount++;
                NS_LOG_INFO("matrix element [" << i << "][" << j << "] is 1");
            }
            else
            {
                NS_LOG_INFO("matrix element [" << i << "][" << j << "] is 0");
            }
        }
    }

    // some animation stuf
    std::string animFile = "pnk-animation.xml"; // Name of file for animation output

    PnkServerHelper echoServer(9);

    ApplicationContainer serverApps = echoServer.Install(nodes);
    serverApps.Start(Seconds(1.0));
    serverApps.Stop(Seconds(10.0));

    PnkClientHelper echoClient(interfaces.GetAddress(0));

    // std::cout << "Number of interfaces: " << interfaces.GetN() << std::endl;

    // echoClient.SetAttribute("RemoteAddress", remoteAddress1);   
    // echoClient.SetAttribute("RemotePort", UintegerValue(9)); 
    ApplicationContainer clientApps = echoClient.Install(nodes.Get(0));


    clientApps.Start(Seconds(2.0));
    clientApps.Stop(Seconds(10.0));

    // Create the animation object and configure for specified output
    AnimationInterface anim(animFile);
    anim.EnablePacketMetadata();                                // Optional
    anim.EnableIpv4L3ProtocolCounters(Seconds(0), Seconds(10)); // Optional

    // Set up the actual simulation
    Ipv4GlobalRoutingHelper::PopulateRoutingTables();

    Simulator::Run();
    std::cout << "Animation Trace file created:" << animFile << std::endl;
    Simulator::Destroy();
}
