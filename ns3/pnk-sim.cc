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

#include "pnk-program.cc"

#include "ns3/applications-module.h"
#include "ns3/core-module.h"
#include "ns3/internet-module.h"
#include "ns3/mobility-helper.h"
#include "ns3/mobility-module.h"
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

NS_LOG_COMPONENT_DEFINE("PnkSim");

// create a full adjacency matrix (all nodes are connected to all other nodes)
std::vector<std::vector<bool>>
getAdj_MatrixFull(int n)
{
    std::vector<std::vector<bool>> Adj_Matrix;
    for (int i = 0; i < n; i++)
    {
        std::vector<bool> row;
        for (int j = 0; j < n; j++)
        {
            if (i == j)
            {
                row.push_back(false);
            }
            else
            {
                row.push_back(true);
            }
        }
        Adj_Matrix.push_back(row);
    }
    return Adj_Matrix;
}

int main(int argc, char *argv[])
{
    CommandLine cmd(__FILE__);
    cmd.Parse(argc, argv);

    Time::SetResolution(Time::NS);
    LogComponentEnable("PnkClient", LOG_LEVEL_INFO);
    LogComponentEnable("PnkServer", LOG_LEVEL_INFO);

    // settings
    const int n_nodes = 3;

    std::string animFile = "pnk-animation.xml"; // Name of file for animation output
    const int initial_packet_destinations[] = {0, 1, 0};
    const int intital_packet_number = 3;
    const bool use_time_list = true; // use the list of times to send the packets
                                     // or set the time between packets

    const double initial_packet_times[] = {2.0, 2.5, 3.0};

    const int initial_packet_time = 2;
    const double time_between_packets = 0.5;

    std::vector<std::vector<bool>> Adj_Matrix = getAdj_MatrixFull(n_nodes);
    // Adj_Matrix.push_back({0,1,1});
    // Adj_Matrix.push_back({1,0,1});
    // Adj_Matrix.push_back({1,1,0});

    // create the nodes

    NodeContainer nodes;       // Declare nodes objects
    nodes.Create(n_nodes + 1); // one more for the master node

    auto masternode = nodes.Get(n_nodes);

    std::string LinkRate("10Mbps");
    std::string LinkDelay("100ms");

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

    // create links, we only take the upper (right) triangle of the adjacency matrix
    // because the adjacency matrix is symmetric

    for (size_t i = 0; i < Adj_Matrix.size(); i++)
    {
        for (size_t j = i; j < Adj_Matrix[i].size(); j++)
        {
            if (Adj_Matrix[i][j] == 1)
            {
                NodeContainer n_links = NodeContainer(nodes.Get(i), nodes.Get(j));
                NetDeviceContainer n_devs = p2p.Install(n_links);
                ipv4_n.Assign(n_devs);
                ipv4_n.NewNetwork();
                NS_LOG_INFO("matrix element [" << i << "][" << j << "] is 1");
            }
            else
            {
                NS_LOG_INFO("matrix element [" << i << "][" << j << "] is 0");
            }
        }
    }

    // add master node to send initial packets (network ingress)
    // we place the master node at the end so that the other nodes numbers dont get shifted (and we
    // start at 0) this master node is connected to all other nodes thought: we only have to connect
    // it to the list of initial packet destinations this might be a premature optimization: we can
    // just connect it to all nodes and it will work fine also, that might make it more flexible
    // (needing just a config change instead of a recompile)

    bool master_node_connections[n_nodes] = {false};
    for (int i = 0; i < intital_packet_number; i++)
    {
        master_node_connections[initial_packet_destinations[i]] = true;
    }

    for (size_t i = 0; i < Adj_Matrix.size(); i++)
    {
        if (master_node_connections[i])
        {
            NodeContainer n_links = NodeContainer(masternode, nodes.Get(i));
            NetDeviceContainer n_devs = p2p.Install(n_links);
            ipv4_n.Assign(n_devs);
            ipv4_n.NewNetwork();
        }
    }

    // define the mobility/location of the nodes

    MobilityHelper mobility_n;
    Ptr<ListPositionAllocator> positionAlloc_n = CreateObject<ListPositionAllocator>();

    for (size_t i = 0; i < n_nodes; i++)
    {
        // place the nodes in a circle around 55,55 with radius 45
        double x = 55 + 45 * cos(2 * M_PI * i / n_nodes);
        double y = 55 + 45 * sin(2 * M_PI * i / n_nodes);

        positionAlloc_n->Add(Vector(x, y, 0));
        Ptr<Node> n0 = nodes.Get(i);
        Ptr<ConstantPositionMobilityModel> nLoc = n0->GetObject<ConstantPositionMobilityModel>();
        if (!nLoc)
        {
            nLoc = CreateObject<ConstantPositionMobilityModel>();
            n0->AggregateObject(nLoc);
        }
        Vector nVec(x, y, 0);
        nLoc->SetPosition(nVec);
    }

    // place the master node at (5, 5)

    positionAlloc_n->Add(Vector(5, 5, 0));
    Ptr<Node> n0 = masternode;
    Ptr<ConstantPositionMobilityModel> nLoc = n0->GetObject<ConstantPositionMobilityModel>();
    if (!nLoc)
    {
        nLoc = CreateObject<ConstantPositionMobilityModel>();
        n0->AggregateObject(nLoc);
    }
    Vector nVec(20, 20, 0);
    nLoc->SetPosition(nVec);

    mobility_n.SetPositionAllocator(positionAlloc_n);
    mobility_n.Install(nodes);

    // determine and print the ipv4 addresess of the nodes (TODO remove the printing when needed)

    std::map<uint32_t, Ipv4Address> nodeAddressMap;
    for (int i = 0; i < n_nodes; i++)
    {
        Ptr<Node> n = nodes.Get(i);
        Ptr<Ipv4> ipv4 = n->GetObject<Ipv4>();
        Ipv4InterfaceAddress ipv4_int_addr = ipv4->GetAddress(1, 0);
        Ipv4Address ip_addr = ipv4_int_addr.GetLocal();
        std::cout << "Node " << i << " has address " << ip_addr << std::endl;
        std::string nodename = "Node" + std::to_string(i);
        nodeAddressMap[i] = ip_addr;
    }

    PnkServerHelper serverHelp(9);
    ApplicationContainer serverApps = serverHelp.Install(nodes, nodeAddressMap);

    NS_LOG_INFO("Setup CBR Traffic Sources.");

    uint16_t port = 9;

    // create the initial packets
    for (int i = 0; i < intital_packet_number; i++)
    {
        Ptr<Ipv4> ipv4 = nodes.Get(initial_packet_destinations[i])->GetObject<Ipv4>();
        Ipv4InterfaceAddress ipv4_int_addr = ipv4->GetAddress(1, 0);
        Ipv4Address ip_addr = ipv4_int_addr.GetLocal();
        PnkClientHelper clienth(ip_addr, port); // traffic flows from node[i] to node[j]
        clienth.SetAttribute("MaxPackets", UintegerValue(1));
        double time = use_time_list ? initial_packet_times[i]
                                    : initial_packet_time + i * time_between_packets;
        ApplicationContainer apps = clienth.Install(masternode);
        apps.Start(Seconds(time));
        apps.Stop(Seconds(time + 0.0001));
    }

    // Create the animation object and configure for specified output
    AnimationInterface anim(animFile);
    anim.EnablePacketMetadata();                                // Optional
    anim.EnableIpv4L3ProtocolCounters(Seconds(0), Seconds(10)); // Optional

    // Set up the actual simulation
    Ipv4GlobalRoutingHelper::PopulateRoutingTables();

    Simulator::Stop(Seconds(10.0));

    Simulator::Run();
    std::cout << "Animation Trace file created:" << animFile << std::endl;
    Simulator::Destroy();
}
