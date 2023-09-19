#ifndef PNK_HEADER_H
#define PNK_HEADER_H

#include "ns3/header.h"
#include "ns3/buffer.h"
#include <iostream>

namespace ns3  {

    class PnkHeader : public Header
    {
    public:
    PnkHeader ();
    // new methods
    void SetData (uint16_t data);
    uint16_t GetData(void);
    void SetSwitch(uint16_t sw);
    uint16_t GetSwitch(void);
    void SetPort(uint16_t pt);
    uint16_t GetPort(void);
    void SetCur(uint16_t cur);
    uint16_t GetCur(void);

    // new method needed
    static TypeId GetTypeId (void);
    // overridden from Header
    TypeId GetInstanceTypeId() const override;
    void Print(std::ostream& os) const override;
    uint32_t GetSerializedSize() const override;
    void Serialize(Buffer::Iterator start) const override;
    uint32_t Deserialize(Buffer::Iterator start) override;
    private:
    uint16_t m_data;
    uint16_t m_sw;
    uint16_t m_pt;
    uint16_t m_cur;
    
    };

} // namespace ns3
#endif /* PNK_HEADER_H */