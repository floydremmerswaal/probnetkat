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
    uint16_t GetData (void);
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
    };

} // namespace ns3
#endif /* PNK_HEADER_H */