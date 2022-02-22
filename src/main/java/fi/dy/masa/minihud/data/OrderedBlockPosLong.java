package fi.dy.masa.minihud.data;

import net.minecraft.util.math.BlockPos;

public class OrderedBlockPosLong
{
    public final long posLong;
    public final int order;

    public OrderedBlockPosLong(long posLong, int order)
    {
        this.posLong = posLong;
        this.order = order;
    }

    public BlockPos getPos()
    {
        return BlockPos.fromLong(this.posLong);
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) { return true; }
        if (o == null || this.getClass() != o.getClass()) { return false; }

        OrderedBlockPosLong that = (OrderedBlockPosLong) o;
        return this.posLong == that.posLong && this.order == that.order;
    }

    @Override
    public int hashCode()
    {
        int result = (int) (this.posLong ^ (this.posLong >>> 32));
        result = 31 * result + this.order;
        return result;
    }

    public static OrderedBlockPosLong of(BlockPos pos, int order)
    {
        return new OrderedBlockPosLong(pos.toLong(), order);
    }
}
