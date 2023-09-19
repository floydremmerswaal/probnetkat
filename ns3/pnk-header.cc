#include "pnk-header.h"
#include "ns3/header.h"

namespace ns3 {

    NS_LOG_COMPONENT_DEFINE("PnkHeader");

    NS_OBJECT_ENSURE_REGISTERED(PnkHeader);

    PnkHeader::PnkHeader () : m_data(0), m_sw(0), m_pt(0), m_cur(0)
    {
        NS_LOG_FUNCTION(this);
        m_data = 0;
        m_sw = 0;
        m_pt = 0;
        m_cur = 0;
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

    void 
    PnkHeader::SetSwitch (uint16_t sw)
    {
        m_sw = sw;
    }
    uint16_t
    PnkHeader::GetSwitch (void)
    {
        return m_sw;
    }

    void 
    PnkHeader::SetPort (uint16_t pt)
    {
        m_pt = pt;
    }
    uint16_t
    PnkHeader::GetPort (void)
    {
        return m_pt;
    }


    void 
    PnkHeader::SetCur (uint16_t cur)
    {
        m_cur = cur;
    }
    uint16_t
    PnkHeader::GetCur (void)
    {
        return m_cur;
    }


    uint32_t 
    PnkHeader::GetSerializedSize (void) const
    {
        // eight bytes of data to store (4 times 2 bytes)
        return 8;
    }
    void 
    PnkHeader::Serialize (Buffer::Iterator start) const
    {
        start.WriteHtonU16 (m_data);
        start.WriteHtonU16(m_sw);
        start.WriteHtonU16(m_pt);
        start.WriteHtonU16(m_cur);
        
    }
    uint32_t 
    PnkHeader::Deserialize (Buffer::Iterator start)
    {
        m_data = start.ReadNtohU16 ();
        m_sw = start.ReadNtohU16();
        m_pt = start.ReadNtohU16();
        m_cur = start.ReadNtohU16();
        return 8;
    }
    void 
    PnkHeader::Print (std::ostream &os) const
    {
        os << "d:" << m_data << ", sw:" << m_sw << ", pt:" << m_pt << ", cur:" << m_cur; 
    }

    TypeId
    PnkHeader::GetInstanceTypeId() const
    {
        return GetTypeId();
    }


} // namespace ns3