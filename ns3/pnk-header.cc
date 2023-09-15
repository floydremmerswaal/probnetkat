#include "pnk-header.h"
#include "ns3/header.h"

namespace ns3 {

    NS_LOG_COMPONENT_DEFINE("PnkHeader");

    NS_OBJECT_ENSURE_REGISTERED(PnkHeader);

    PnkHeader::PnkHeader () : m_data(0)
    {
        NS_LOG_FUNCTION(this);
        m_data = 0;
    }

    TypeId
    PnkHeader::GetTypeId()
    {
        static TypeId tid = TypeId("ns3::PnkHeader")
                                .SetParent<Header>()
                                .SetGroupName("Applications")
                                .AddConstructor<PnkHeader>();
        return tid;
    }

    void 
    PnkHeader::SetData (uint16_t data)
    {
        m_data = data;
    }
    uint16_t
    PnkHeader::GetData (void)
    {
        return m_data;
    }

    uint32_t 
    PnkHeader::GetSerializedSize (void) const
    {
        // two bytes of data to store
        return 2;
    }
    void 
    PnkHeader::Serialize (Buffer::Iterator start) const
    {
        start.WriteHtonU16 (m_data);
    }
    uint32_t 
    PnkHeader::Deserialize (Buffer::Iterator start)
    {
        m_data = start.ReadNtohU16 ();
        return 2;
    }
    void 
    PnkHeader::Print (std::ostream &os) const
    {
        os << m_data;
    }

    TypeId
    PnkHeader::GetInstanceTypeId() const
    {
        return GetTypeId();
    }


} // namespace ns3