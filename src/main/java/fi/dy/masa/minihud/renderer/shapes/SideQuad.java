package fi.dy.masa.minihud.renderer.shapes;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;

public record SideQuad(long startPos, int width, int height, Direction side)
{
    @Override
    public String toString()
    {
        return "SideQuad{start=" + String.format("BlockPos{x=%d,y=%d,z=%d}",
                                                 BlockPos.unpackLongX(this.startPos),
                                                 BlockPos.unpackLongY(this.startPos),
                                                 BlockPos.unpackLongZ(this.startPos)) +
                       ", width=" + this.width +
                       ", height=" + this.height +
                       ", side=" + this.side + '}';
    }
}
